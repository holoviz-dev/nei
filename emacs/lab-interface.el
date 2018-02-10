(require 'lab-util)
(require 'lab-parse)
(require 'json)



(defvar labmode--prompt-regexp "^# In\\[\\([\\[:alnum:] ]*\\)\]"
  "The regular expression used to match prompts. Not to be changed by users.")

(defface labmode-prompt-face
  '((t :foreground "#ffcc66"))
  "Face used for the code prompts"
)

(defface labmode-cell-highlight-face
  '((((class color) (background light)) :background "grey95")
    (((class color) (background  dark)) :background "grey20"))
  "Face for highlighting the current cell.")

(defvar labmode-fontified nil
  "Whether or not the labmode buffer is currently fontified")

(defvar labmode--highlight-overlay nil
  "The overlay used to highlight current cell in labmode")

;;===================================;;
;; Mark current cell with an overlay ;; 
;;===================================;;

(defun labmode--start-of-line (pos)
  (save-excursion (goto-char pos) (beginning-of-line) (point))
)

(defun labmode--end-of-line (pos)
  (save-excursion (goto-char pos) (end-of-line) (point))
)


(defun labmode--update-highlight-cell ()
  "Uses regular expression search forwards/backwards to highlight 
   the current cell with an overlay" 
  
  (if (null labmode--highlight-overlay)
      (setq labmode--highlight-overlay
            (let ((ov (make-overlay 1 1 nil t)))
              (overlay-put ov 'face 'labmode-cell-highlight-face)
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
          (move-overlay labmode--highlight-overlay prev-code-pos 
                        (labmode--start-of-line next-code-pos))
          (move-overlay labmode--highlight-overlay prev-md-pos
                        (labmode--start-of-line next-md-pos))
        )
      )
    )
  )
 

(defun labmode--point-move-disable-highlight-hook ()
  "Post-command hook to disable cell highlight when the 
   point moves out the current overlay region"
  (if (and (memq this-command '(next-line previous-line))
           (not (null labmode--highlight-overlay))
           (eq (current-buffer) (overlay-buffer labmode--highlight-overlay)))
      (if (or (< (point) (overlay-start labmode--highlight-overlay))
              (> (point) (overlay-end labmode--highlight-overlay)))
          (move-overlay labmode--highlight-overlay 0 0)
        )
    )
  )


;;=========================;;
;; Fontification interface ;;
;;=========================;;

(defun labmode-fontify-matcher ()
  '(( "^# In\\[\\([\\[:alnum:] ]*\\)\]"
     (0 (let ((match (match-string 1)))
          (progn
            (set-text-properties 0 (length match) nil match)
            (add-text-properties (match-beginning 0) (match-end 0)
                                 (list 'display
                                       (concat "In[" match
                                               "]") 'face 'labmode-prompt-face))
            nil))))
    )
)


(defun labmode-fontify ()
  (font-lock-add-keywords nil (labmode-fontify-matcher))
  (save-excursion
    (font-lock-fontify-keywords-region (point-min) (point-max))
    )
  (setq labmode-fontified t)
  )

(defun labmode-defontify ()
  (font-lock-remove-keywords nil (labmode-fontify-matcher))
  (remove-text-properties (point-min) (point-max) 
                          (list 'display
                                labmode--prompt-regexp 'face 'labmode-prompt-face))

  (save-excursion
    (font-lock-fontify-keywords-region (point-min) (point-max))
    )
  (setq labmode-fontified nil)
  )


(defun labmode-toggle-fontify ()
  (interactive)
  (if labmode-fontified (labmode-defontify) (labmode-fontify))
)

;;==================================;;
;; Navigation and prompt management ;;
;;==================================;;


(defun labmode--update-exec-prompt (count)
  (save-excursion  ;; TODO: Insert prompt if missing
    (save-match-data
      (re-search-forward labmode--prompt-regexp nil t -1) ;; Needs to do nothing if no match
      (replace-match (format "# In[%d]" count))
      )
    )
  )

(defun labmode-move-point-to-next-cell ()
  "Move the point to the next cell"
  (interactive)
  (goto-char (labmode--point-search-code-position 1))
)

(defun labmode-move-point-to-previous-cell ()
  "Move the point to the previous cell"
  (interactive)  
  (goto-char (labmode--point-search-code-position -2))
  )


;; TODO: Make these more robust e.g when at the top/bottom of the buffer
(defun labmode-move-cell-up ()
  (interactive)
  (labmode--hold-mode "on")
  (let* ((start (labmode--point-search-code-position -1 t))
         (end (labmode--point-search-code-position 1 t))
         (line-text (delete-and-extract-region start end)))
    
    (goto-char (labmode--point-search-code-position -1 t))
    (labmode--hold-mode "off")
    (insert line-text)
    )
  )

(defun labmode-move-cell-down ()
  (interactive)
  (labmode--hold-mode "on")

  (let* ((start (labmode--point-search-code-position -1 t))
         (end (labmode--point-search-code-position 1 t))
         (line-text (delete-and-extract-region start end)))
    
    (goto-char (labmode--point-search-code-position 2 t))
    (labmode--hold-mode "off")
    (insert line-text)
    )
  )


(defun labmode--point-search-code-position (count &optional at-prompt)
  "Uses regular expression search by count to report position of cells.
   If at-prompt is truthy, the position is at the start of the prompt."
  (save-excursion ;; Currently only handles code cells
    (re-search-forward labmode--prompt-regexp nil t count)
    (if at-prompt
        (labmode--start-of-line (point))
      (1+ (labmode--end-of-line (point)))
      )
    )
  )

(provide 'lab-interface)
