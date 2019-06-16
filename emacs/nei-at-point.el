;; Integration of NEI with thing-at-point in order to make thing-at-point
;; aware of markdown and code cells. Symbols supported are nei-cell,
;; nei-code-cell and nei-markdown-cell
;;
;; The bounds at point functions are defined first as the forward
;; functions often make use of them.
;;
;; This approach is designed to help NEI be agnostic towards the exact
;; text format. The thing-at-point API is then used by the rest of NEI
;; mode.


(defvar nei--prompt-regexp "^# In\\[\\([\\[:alnum:] ]*\\)\]"
  "The regular expression used to match prompts. Not to be changed by users.")

(defvar nei--md-close-regexp "^\"\"\" # :md:$")


;;
;; Bounds of things (markdown and code cells)
;;

(defun nei--bounds-of-markdown-cell-at-point ()
  "Function to return bounds of markdown cell at point to integrate with thing-at-point"
  ;; Works by checking for end marker, looking forward for opening and
  ;; checking no triple quotes exist in between.
  (let ((end   (save-excursion  (progn (beginning-of-visual-line)
                                       (re-search-forward nei--md-close-regexp nil t 1))))
        (start (save-excursion  (progn (end-of-visual-line)
                                       (re-search-backward "^\"\"\"$" nil t 1)))))
    (if (not (null end))
        (save-excursion
          (goto-char end)
          (let ((intermediate (re-search-backward "\"\"\"" nil t 2)))
            (if (and (not (null start))
                     (not (null intermediate))
                     (<= intermediate start))
                (cons start end)
              )
            )
          )
      )
    )
  )

(defun nei--bounds-of-code-cell-at-point ()
  "Function to return bounds of code cell at point to integrate with thing-at-point"
  ;; If in a markdown cell, then not in a code cell
  (if (not (bounds-of-thing-at-point 'nei-markdown-cell))
      (let* ((next-md-bounds (save-excursion
                              (forward-thing 'nei-markdown-cell)
                              (bounds-of-thing-at-point 'nei-markdown-cell)))
            (prev-md-bounds (save-excursion
                              (forward-thing 'nei-markdown-cell -1)
                              (bounds-of-thing-at-point 'nei-markdown-cell)))
            (md-min-limit (or (cdr prev-md-bounds) 0))
            (md-max-limit (or (car next-md-bounds) (point-max)))
            (start-code (progn (save-excursion
                                 (end-of-visual-line)
                                 (re-search-backward nei--prompt-regexp nil t 1))))
            (next-code (progn (save-excursion
                                (end-of-visual-line)
                                (re-search-forward nei--prompt-regexp nil t 1)
                                (beginning-of-visual-line)
                                (- (point) 1))))
            (end (min (or next-code (point-max)) (or md-max-limit (point-max)))))

        (if (and (not (null start-code)) (< md-min-limit start-code))
            (progn
              (cons start-code end)
              )
          )
        )
    )
  )

(defun nei--bounds-of-cell-at-point ()
  "Function to return bounds of cell at point to integrate with thing-at-point"
  (or (nei--bounds-of-markdown-cell-at-point) (nei--bounds-of-code-cell-at-point))
  )


;;
;; Forward things (markdown and code cells)
;;

(defun re-search-forward-thing (regexp thing &optional bound noerror steps not-thing)
  "Utility similar to re-search-forward that only registers a match if
  there is thing at the position of the regexp match (as determined by
  bounds-of-thing-at-point). If not-thing is true, registers a match
  only if the thing is *not* present.

  Sets the point to the end of the occurrence found, and return point."
  (let* ((counter 0)
         (steps (if (null steps) 1 steps))
         (target-count (abs steps))
         (delta (if (< steps 0) -1 1))
         (position nil)
         (continue-search t))

    (save-excursion
      (while continue-search
        (setq position (re-search-forward regexp bound noerror delta))
        (if (and (not not-thing) (bounds-of-thing-at-point thing))
            (setq counter (+ 1 counter))
          )
        (if (and not-thing (not (bounds-of-thing-at-point thing)))
            (setq counter (+ 1 counter))
          )

        (if (or (eq position nil) (eq counter target-count))
            (setq continue-search nil))

        )
      )
    (if (not (null position))
        (progn
          (goto-char position)
          position
          )
      )
    )
  )


(defun nei--forward-markdown-cell (&optional arg)
  "Move point forward ARG markdown cells (backwards is ARG is negative).
   Returns t if the point is moved else nil."
    (let* ((target-pos nil)
           (arg (or arg 1))
           ;; If re-searching forward from inside an md cell, the next end boundary
           ;; is still within that cell. skips adds an offset to arg to compensate
           ;; which is not needed if arg is negative (jumping backwards)
           (skips (+ arg (if (bounds-of-thing-at-point 'nei-markdown-cell)
                             (if (> 0 arg) 0 1) 0)))
           (match (save-excursion
                    (re-search-forward-thing nei--md-close-regexp
                                             'nei-markdown-cell nil t skips))))
      (if match
          (progn
            (goto-char match)
            (let ((bounds (bounds-of-thing-at-point 'nei-markdown-cell)))
              (if bounds
                  (setq target-pos (+ (car bounds) 4))
                )
              )
            )
        )
      (if target-pos (progn (goto-char target-pos) t))
      )
    )


(defun nei--forward-code-cell (&optional arg)
  "Move point forward ARG code cells (backwards is ARG is negative).
   Returns t if the point is moved else nil."
    (let ((match-pos (save-excursion
                  (re-search-forward-thing nei--prompt-regexp
                                           'nei-markdown-cell nil t 1 t))))
      (if match-pos
          (progn  (goto-char (+ match-pos 1)) t)
        )
      )
    )


(defun nei--forward-cell (&optional arg)
  "Move point forward ARG cells (backwards is ARG is negative).
   Returns t if the point is moved else nil."  
  (let ((target-pos nil)
        (arg (or arg 1))
        (next-md nil)
        (next-code nil))

    (save-excursion
      (dotimes (x arg)
        (setq next-md (save-excursion (if (nei--forward-markdown-cell) (point))))
        (setq next-code (save-excursion (if (nei--forward-code-cell) (point))))


        (cond ((and (null next-md) (null next-code))
               (setq target-pos nil))
              ((and next-md next-code)
               (setq target-pos (min next-md next-code)))
              (next-md (setq target-pos next-md))
              (next-code (setq target-pos next-code)))

        (if target-pos (goto-char target-pos))
        )
      )

    (if target-pos (progn (goto-char target-pos) t))
    )
  )


(defun nei--register-things-at-point ()
  ;; Markdown cells
  (put 'nei-markdown-cell 'bounds-of-thing-at-point
       'nei--bounds-of-markdown-cell-at-point)
  (put 'nei-markdown-cell 'forward-op
       'nei--forward-markdown-cell)
  ;; Code cells
  (put 'nei-code-cell 'bounds-of-thing-at-point
       'nei--bounds-of-code-cell-at-point)
  (put 'nei-code-cell 'forward-op
       'nei--forward-code-cell)
  ;; Both types of cell
  (put 'nei-cell 'bounds-of-thing-at-point
       'nei--bounds-of-cell-at-point)
  (put 'nei-cell 'forward-op
       'nei--forward-cell)
  )

(provide 'nei-at-point)
