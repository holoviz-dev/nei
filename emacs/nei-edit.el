;;;  -*- lexical-binding: t; -*-

(require 'nei-util)
(require 'nei-parse)
(require 'json)



(defvar nei--prompt-regexp "^# In\\[\\([\\[:alnum:] ]*\\)\]"
  "The regular expression used to match prompts. Not to be changed by users.")

(defface nei-prompt-face
  '((t :foreground "#ffcc66"))
  "Face used for the code prompts"
)

(defface nei-cell-highlight-face
  '((((class color) (background light)) :background "grey95")
    (((class color) (background  dark)) :background "grey20"))
  "Face for highlighting the current cell.")

(defvar nei--fontified nil
  "Whether or not the nei buffer is currently fontified")

(defvar nei--highlight-overlay nil
  "The overlay used to highlight current cell in nei")

;;===================================;;
;; Mark current cell with an overlay ;; 
;;===================================;;

(defun nei--start-of-line (pos)
  (save-excursion (goto-char pos) (beginning-of-line) (point))
)

(defun nei--end-of-line (pos)
  (save-excursion (goto-char pos) (end-of-line) (point))
)


(defun nei--update-highlight-cell ()
  "Uses regular expression search forwards/backwards to highlight 
   the current cell with an overlay" 
  
  (if (null nei--highlight-overlay)
      (setq nei--highlight-overlay
            (let ((ov (make-overlay 1 1 nil t)))
              (overlay-put ov 'face 'nei-cell-highlight-face)
              ov))
    )
  (let ((prev-code (save-excursion (re-search-forward "^# In\\[" nil t -1)))
        (next-code (save-excursion (re-search-forward "^# In\\[" nil t 1)))
        (prev-md (save-excursion (re-search-forward "^\"\"\"" nil t -1)))
        (next-md (save-excursion (re-search-forward "^\"\"\"" nil t 1))))
    (let ((prev-code-pos (if (null prev-code) (point-min) prev-code))
          (next-code-pos (if (null next-code) (point-max) next-code))
          (prev-md-pos (if (null prev-md) (point-min) prev-md))
          (next-md-pos (if (null next-md) (point-max) next-md)))
      
      (if (> prev-code-pos prev-md-pos)
          (move-overlay nei--highlight-overlay prev-code-pos 
                        (nei--start-of-line next-code-pos))
          (move-overlay nei--highlight-overlay prev-md-pos
                        (nei--start-of-line next-md-pos))
        )
      )
    )
  )
 

(defun nei--point-move-disable-highlight-hook ()
  "Post-command hook to disable cell highlight when the 
   point moves out the current overlay region"
  (if (and (memq this-command '(next-line previous-line))
           (not (null nei--highlight-overlay))
           (eq (current-buffer) (overlay-buffer nei--highlight-overlay)))
      (if (or (< (point) (overlay-start nei--highlight-overlay))
              (> (point) (overlay-end nei--highlight-overlay)))
          (move-overlay nei--highlight-overlay 0 0)
        )
    )
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


(defun nei-fontify ()
  (let ((modified (buffer-modified-p)))
    (font-lock-add-keywords nil (nei-fontify-matcher))
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
    (remove-text-properties (point-min) (point-max) 
                            (list 'display
                                  nei--prompt-regexp 'face 'nei-prompt-face))
    
    (save-excursion
      (font-lock-fontify-keywords-region (point-min) (point-max))
      )
    (setq nei--fontified nil)
    (set-buffer-modified-p modified)
    )
  )


(defun nei-toggle-fontify ()
  (interactive)
  (if nei--fontified (nei-defontify) (nei-fontify))
)


;;==================================;;
;; Navigation and prompt management ;;
;;==================================;;


(defun nei--update-exec-prompt (count)
  (save-excursion  ;; TODO: Insert prompt if missing
    (save-match-data
      (let ((modified (buffer-modified-p)))
        (re-search-forward nei--prompt-regexp nil t -1) ;; Needs to do nothing if no match
        (replace-match (format "# In[%d]" count))
        (set-buffer-modified-p modified)
        )
      
      )
    )
  )

(defun nei-move-point-to-next-cell ()
  "Move the point to the next cell"
  (interactive)
  (goto-char (nei--point-search-code-position 1))
)

(defun nei-move-point-to-previous-cell ()
  "Move the point to the previous cell"
  (interactive)  
  (goto-char (nei--point-search-code-position -2))
  )


;; TODO: Make these more robust e.g when at the top/bottom of the buffer
(defun nei-move-cell-up ()
  (interactive)
  (nei--hold-mode "on")
  (let* ((start (nei--point-search-code-position -1 t))
         (end (nei--point-search-code-position 1 t))
         (line-text (delete-and-extract-region start end)))
    
    (goto-char (nei--point-search-code-position -1 t))
    (nei--hold-mode "off")
    (insert line-text)
    )
  )

(defun nei-move-cell-down ()
  (interactive)
  (nei--hold-mode "on")

  (let* ((start (nei--point-search-code-position -1 t))
         (end (nei--point-search-code-position 1 t))
         (line-text (delete-and-extract-region start end)))
    
    (goto-char (nei--point-search-code-position 2 t))
    (nei--hold-mode "off")
    (insert line-text)
    )
  )


(defun nei--point-search-code-position (count &optional at-prompt)
  "Uses regular expression search by count to report position of cells.
   If at-prompt is truthy, the position is at the start of the prompt."
  (save-excursion ;; Currently only handles code cells
    (re-search-forward nei--prompt-regexp nil t count)
    (if at-prompt
        (nei--start-of-line (point))
      (1+ (nei--end-of-line (point)))
      )
    )
  )

(defun nei-clear-execution-prompts () ;; TODO: Clear the remote prompts too
  (interactive)
  (save-excursion 
    (perform-replace "# In\\[.*\\]" "# In[ ]" nil t nil nil nil (point-min) (point-max))
    )
  )


(provide 'nei-edit)
