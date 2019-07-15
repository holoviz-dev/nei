;;;  -*- lexical-binding: t; -*-
(require 'nei-util)
(require 'nei-parse)
(require 'nei-at-point)
(require 'json)


(defvar nei--prompt-regexp "^# In\\[\\([\\[:alnum:] ]*\\)\]"
  "The regular expression used to match prompts. Not to be changed by users.")

(defface nei-prompt-face
  '((t :foreground "#ffcc66"))
  "Face used for the code prompts"
  )

(defface nei-md-face
  `((t :foreground
       ,(nei--average-with-background-color
         (face-attribute 'font-lock-string-face :foreground) 5
         )))
  "Face for markdown separators. Default is 33% between font-lock-string-face and background"
)

(defvar-local nei--fontified nil
  "Whether or not the nei buffer is currently fontified")


(defvar nei--revert-in-progress nil
  "Boolean that indicates if a file revert operation is in progress")

(defun nei-insert-escaped-triple-quotes ()
  "Inserts escaped triple quote (zero width space separated"
  (interactive)
  (insert "\"​\"​\"")
  )

;;=========================;;
;; Fontification interface ;;
;;=========================;;

(defun nei-fontify-matcher ()
  '(( "^# In\\[\\([\\[:alnum:] ]*\\)\]"
     (0 (let ((match (match-string 1)))
          (progn
            (set-text-properties 0 (length match) nil match)
            (add-text-properties (match-beginning 0) (match-end 0)
                                 (list 'display
                                       (concat "In[" match
                                               "]") 'face 'nei-prompt-face))
            nil))))
    )
)


(defun nei-fontify-markdown-matcher-open ()
  '(("^\"\"\""
     (0 (let ((match (match-string 1)))
          (progn
            (set-text-properties 0 (length match) nil match)
            (add-text-properties (match-beginning 0) (match-end 0)
                                 (list 'face 'nei-md-face))
            nil))))
    )
  )

(defun nei-fontify-markdown-matcher-close ()
  '(("^\"\"\" #:md:"
     (0 (let ((match (match-string 1)))
          (progn
            (set-text-properties 0 (length match) nil match)
            (add-text-properties (match-beginning 0) (match-end 0)
                                 (list 'face 'nei-md-face))
            nil))))
    )
  )



(defun nei-fontify ()
  (let ((modified (buffer-modified-p)))
    (font-lock-add-keywords nil (nei-fontify-matcher))
    (font-lock-add-keywords nil (nei-fontify-markdown-matcher-open) t)
    (font-lock-add-keywords nil (nei-fontify-markdown-matcher-close) t)
    

    (save-excursion
      (font-lock-fontify-keywords-region (point-min) (point-max))
      )
    (setq nei--fontified t)
    (set-buffer-modified-p modified)
    )
  )

(defun nei-defontify ()
  (let ((modified (buffer-modified-p)))
    (font-lock-remove-keywords nil (nei-fontify-matcher))
    (font-lock-remove-keywords nil (nei-fontify-markdown-matcher-open))
    (font-lock-remove-keywords nil (nei-fontify-markdown-matcher-close))

    (remove-text-properties (point-min) (point-max)
                            (list 'display
                                  nei--prompt-regexp 'face 'nei-prompt-face))
    (remove-text-properties (point-min) (point-max)
                            (list 'display
                                  "^\"\"\"" 'face 'nei-md-face))

    (remove-text-properties (point-min) (point-max)
                            (list 'display
                                  "^\"\"\" #:md:" 'face 'nei-md-face))
    (save-excursion
      (font-lock-fontify-keywords-region (point-min) (point-max))
      )
    (setq nei--fontified nil)
    (set-buffer-modified-p modified)
    )
  )


;;==================================;;
;; Navigation and prompt management ;;
;;==================================;;

(defun nei--update-exec-prompt (count)
  (let* ((undo-list buffer-undo-list)
         (replacement (format "# In[%d]" count))
         (prev-point-pos (point))
         (offset 0)) ;; Offset due to point number shifting from 9-10, 99-100 etc
    (buffer-disable-undo)
    (save-excursion  ;; TODO: Insert prompt if missing
      (save-match-data
        (let ((modified (buffer-modified-p)))
          (re-search-forward nei--prompt-regexp nil t -1) ;; Needs to do nothing if no match
          (setq offset (- (length replacement) (length (match-string 0))))
          (replace-match replacement)
          (set-buffer-modified-p modified)
          )
        )
      )
    (setq buffer-undo-list undo-list)
    (if (null (eq buffer-undo-list t))
        (add-to-list 'buffer-undo-list (+ prev-point-pos offset)))
    )
  )

(defun nei--clear-execution-prompts ()
  (interactive)
  (save-excursion
    (perform-replace "# In\\[.*\\]" "# In[ ]" nil t nil nil nil (point-min) (point-max))
    )
  )


;;========================;;
;; Cell swap and movement ;;
;;========================;;


(defun nei--expand-markdown-bounds (bounds)
  "Markdown bounds expanded by the following newline if present"
  (save-excursion
    (goto-char (car bounds))
    (if (and (bounds-of-thing-at-point 'nei-markdown-cell) (< (cdr bounds) (point-max)))
        (cons (car bounds) (+ 1 (cdr bounds)))
      bounds
      )
    )
  )

(defun nei--point-within-bounds (bounds)
  (and (>= (point) (car bounds)) (< (point) (cdr bounds)))
  )


(defun nei--swap-cells-by-bounds (fst-bounds snd-bounds)
  "Given the bounds of two adjacent cells as given by
   bounds-of-thing-at-point, swap them within the original line span
   while preserving any content between the cells.

   If the point is within or between these bounds, it's relative
   position is preserved after the swap "

  (let* ((fst-bounds (nei--expand-markdown-bounds fst-bounds))
         (snd-bounds (nei--expand-markdown-bounds snd-bounds))
         (fst-string (buffer-substring (car fst-bounds) (cdr fst-bounds)))
         (snd-string (buffer-substring (car snd-bounds) (cdr snd-bounds)))
         (separation (buffer-substring (cdr fst-bounds) (car snd-bounds)))
         (replacement (s-concat snd-string separation fst-string))
         (sep-bounds (cons (cdr fst-bounds) (car snd-bounds)))
         (fst-delta (- (cdr fst-bounds) (car fst-bounds)))
         (snd-delta (- (cdr snd-bounds) (car snd-bounds)))
         (sep-delta (- (car snd-bounds) (cdr fst-bounds)))
         (new-fst-bounds (cons (car fst-bounds) (+ (car fst-bounds) snd-delta)))
         (new-sep-bounds (cons (cdr new-fst-bounds) (+ (cdr new-fst-bounds) sep-delta)))
         (new-snd-bounds (cons (cdr new-sep-bounds) (+ (cdr new-sep-bounds) fst-delta)))
         (target-pos nil)
         )
    (cond ((nei--point-within-bounds fst-bounds)
           (setq target-pos (+ (car new-snd-bounds)
                               (- (point) (car fst-bounds)))))
          ((nei--point-within-bounds sep-bounds)
           
           (setq target-pos (+ (car new-sep-bounds)
                               (- (point) (car sep-bounds)))))
          ((nei--point-within-bounds snd-bounds)
           (setq target-pos (+ (car new-fst-bounds)
                               (- (point) (car snd-bounds)))))
          )

    (save-excursion 
      (goto-char (car fst-bounds))
      (nei--hold-mode "on") 
      (delete-region (car fst-bounds) (cdr snd-bounds))
      (nei--hold-mode "off") 
      (insert replacement)
      )
    (goto-char target-pos)
    )
  )

(defun nei-move-cell-down ()
  (interactive)
  (let ((cell-bounds (bounds-of-thing-at-point 'nei-cell))
        (next-cell-bounds (save-excursion
                            (end-of-line)
                            (if (forward-thing 'nei-cell)
                                (bounds-of-thing-at-point 'nei-cell)))))
    (if (and cell-bounds next-cell-bounds)
        (nei--swap-cells-by-bounds cell-bounds next-cell-bounds)  
      )
    )
  )

(defun nei-move-cell-up ()
  (interactive)
  (let ((cell-bounds (bounds-of-thing-at-point 'nei-cell))
        (prev-cell-bounds (save-excursion
                            (if (forward-thing 'nei-cell -1)
                                (bounds-of-thing-at-point 'nei-cell)))))
    (if (and cell-bounds prev-cell-bounds)
        (nei--swap-cells-by-bounds prev-cell-bounds cell-bounds)  
      )
    )
  )

;;=======================================;;
;; Functions for handling cell selection ;;
;;=======================================;;

(defun nei-select-cell ()
  "Select a single cell at the point position"
  (interactive)
  (let ((cell-bounds (bounds-of-thing-at-point 'nei-cell)))
    (if cell-bounds
        (progn 
          (goto-char (car cell-bounds))
          (set-mark (cdr cell-bounds))
          )
      )
    )
  )

(defun nei-select-cells ()
  "Select the cells within the marked region"
  (interactive)
  (if mark-active
      (let* ((point-before-mark (< (point) (mark)))
             (min-pos (min (point) (mark)))
             (max-pos (max (point) (mark)))
             (bounds-at-min (save-excursion (goto-char min-pos)
                                            (bounds-of-thing-at-point 'nei-cell)))
             (bounds-at-max (save-excursion (goto-char max-pos)
                                            (bounds-of-thing-at-point 'nei-cell)))
             (new-min (if bounds-at-min (car bounds-at-min)))
             (new-max (if bounds-at-min (cdr bounds-at-max)))
             )
         (if (not (null new-max)) (goto-char new-max))
         (if (not (null new-min)) (set-mark new-min))
         (if point-before-mark (exchange-point-and-mark))        
         )
    (nei-select-cell)
    )
  )


;;===============================================;;
;; Functions for handling copy/paste with output ;;
;;===============================================;;


(defvar nei--killed-with-output nil
  "Internal variable that holds the cell boundaries of cells that were
  killed that when pasted should have their output restored.")

(defun nei-kill-cells-with-output ()
  "Cell aware version of kill-region that expands the region to include
  the entire cells covered when marking a region and that preserves cell
  output when yanked"
  (interactive)
  (message "SENDING MESSAGE TO SERVER ABOUT KILLED CELLS")
  (save-excursion 
    (if mark-active
        (progn
          (nei-select-cells)
          (setq nei--killed-with-output "INFO ABOUT KILLED CELLS")
          (kill-region (mark) (point))
          (setq nei--killed-with-output nil)
          )
      )
   )
  )


(defun nei--yank-handler (info)
  "Custom yank-handler that messages the server about cells that need
  their output restored upon yanking"
  ;;(message "MESSAGE TO SERVER ABOUT PASTE %s" (nth 3 info))
  (insert (car info))
  )

(defun nei--filter-buffer-substring-function (beg end &optional delete)
  "Custom filter-buffer-substring-function that applied the yank-handler
  text property when there is information to store about killed cells
  that should retain their output when yanked"
  (let ((substr (buffer-substring--filter beg end delete)))
    (if nei--killed-with-output
        (put-text-property 0 (length substr) 'yank-handler
                           (list 'nei--yank-handler
                                 (list substr beg end nei--killed-with-output))
                           substr)
      )
    substr
    )
  )

(setq filter-buffer-substring-function #'nei--filter-buffer-substring-function)

;;====================================;;
;; Functions for handling file revert ;;
;;====================================;;


(defun nei--verify-visited-file-modtime ()
  "Checks for equality of file modtime with modtime stored by visited-file-modtime"
  (eq 0 (visited-file-modtime)) ;; Not set or temporarily disabled
  (equal
   (nth 5 (file-attributes nei--ipynb-buffer-filename))
   (visited-file-modtime)
   )
  )
 
(defun nei--buffer-stale-function (&optional noconfirm)
  "Custom buffer-stale-function used to know when to revert from file on disk"
  (and nei--ipynb-buffer-filename
       (file-readable-p nei--ipynb-buffer-filename)
       (not (buffer-modified-p (current-buffer)))
       (not (nei--verify-visited-file-modtime ))))


(defun nei--reverter-function (&optional _arg _noconfirm)
  (let ((auto-revert-verbose-state auto-revert-verbose))
    (setq auto-revert-verbose nil)
    (if (not nei--revert-in-progress)
        (progn
          (setq nei--revert-in-progress t)
          (let ((answer (read-char-choice
                         "File changed on disk. Overwrite (o), Reload (r) or Diff (d)?: "
                         '(?o ?r ?d))))
            (if (eq answer ?r)
                (progn
                  (message "Reloading notebook")
                  ;; TODO: This will reset kernel state which is unnecessary
                  (let ((ipynb-source-filename nei--ipynb-buffer-filename))
                    (select-window (get-buffer-window (buffer-name)))
                    (kill-buffer (buffer-name))
                    (nei-open-notebook ipynb-source-filename)))
              )
            (if (eq answer ?o)
                (progn
                  (message "Overwriting file..." (buffer-name))
                  (nei--write-ipynb-hook)
                  )
            )
            (if (eq answer ?d)
                (message "Ediff resolve not yet implemented...")
              )
            )
          (setq nei--revert-in-progress nil)
        )
      )
    (setq auto-revert-verbose auto-revert-verbose-state)
    )
  )

(provide 'nei-edit)
