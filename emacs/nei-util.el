;;;  -*- lexical-binding: t; -*-
;; Module for utilities

(require 'htmlize)

(defun nei--logging (&rest args)
  (if nei-verbose (apply 'message  args))
  )

(defun nei--line-bounds (bounds &optional zero-indexed relative)
  """
  If zero-indexed is true, then start counting lines from zero.
  If relative is true, count lines in  narrowed region.
  """
  (if bounds 
      (let* ((start-line (line-number-at-pos (car bounds) (not relative)))
             (end-line   (line-number-at-pos (cdr bounds) (not relative)))
             (offset-start (if zero-indexed (- start-line 1) start-line))
             (offset-end (if zero-indexed (- end-line 1) end-line)))
        (cons offset-start offset-end)
        )
    )
  )

(defun assoc-value (key alist)
  "Helper to make accessing keys from an alist more readable"
  (cdr (assoc key alist))
  )

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun neiutil--make-buffer-uninteresting ()
  "rename the current buffer to begin with a space"
  (interactive)
  (unless (string-match-p "^ " (buffer-name))
    (rename-buffer (concat " " (buffer-name)))))


(defun neiutil--get-string-from-file (filepath)
  "Return filepath's file content."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string))
  )

(defun neiutil--replace-in-string (what with in)
  "Utility for easier regexp search and replace"
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))


(defun nei--find-conda-envs ()
  "Build an alist of environments using conda.el to populate kernel
   environment selection"
  (let ((accumulator nil))
    (dolist (env-name (conda-env-candidates) accumulator)
      (add-to-list 'accumulator
                   (cons env-name 
                         (concat (conda--get-path-prefix
                                  (conda-env-name-to-dir env-name)) "/" "python")
                         )
                   )
      )
    )
  )


;;==================================;;
;; Utilities to htmlize python code ;;
;;==================================;;


(defun nei--htmlize-css-to-highlight (css)
  "Map the CSS from htmlize to the keywords used by highlight.js"
  (setq css (replace-regexp-in-string ".keyword" ".hljs-keyword" css))
  (setq css (replace-regexp-in-string ".comment" ".hljs-comment" css))
  (setq css (replace-regexp-in-string ".string" ".hljs-string" css))
  (setq css (replace-regexp-in-string ".function-name" ".hljs-title" css))
  (setq css (replace-regexp-in-string ".type" ".hljs-type" css))
  (setq css (replace-regexp-in-string ".variable-name" ".hljs-attribute" css))
  (setq css (replace-regexp-in-string ".constant" ".hljs-meta" css))
  )


(defun nei--htmlize-css ()
  "Use htmlize to generate CSS for syntax highlighting"
  (let (( buf (generate-new-buffer "*htmlize-css*")) ( css nil ))
    (with-current-buffer buf
      (let ((python-faces
             (list font-lock-comment-face font-lock-comment-delimiter-face
                   font-lock-doc-face font-lock-constant-face font-lock-function-name-face
                   font-lock-builtin-face font-lock-type-face font-lock-string-face
                   font-lock-keyword-face font-lock-variable-name-face 'nei-prompt-face)))
        (htmlize-css-insert-head python-faces
                                 (htmlize-make-face-map
                                  (adjoin 'default  python-faces)))
        )
      (setq css (buffer-substring (point-min) (point-max)))
      )
    (kill-buffer "*htmlize-css*")
    (nei--htmlize-css-to-highlight css)
    )
)


(defun nei--prompt-for-filename-until-predicate (prompt error-msg predicate)
  (let ((result nil))
    (setq result (read-file-name prompt))
    (if (not (funcall predicate result))
        (progn
          (message nil)
          (sleep-for 0 50)
          (message error-msg)
          (sleep-for 1.5)
          (setq result (nei--prompt-for-filename-until-predicate prompt error-msg predicate))
          )
      )
    result
    )
  )

(defun nei--average-colors (color &rest colors)
  "Takes an average of the color list supplied"
  (let* ((colors (cons color colors))
         (colors (mapcar 'color-name-to-rgb colors))
         (len    (length colors))
         (sums   (apply 'cl-mapcar '+ colors))
         (avg    (mapcar (lambda (v) (/ v len)) sums)))
    (apply 'color-rgb-to-hex avg)))


(defun nei--average-with-background-color (color factor)
  (let ((bg (face-attribute 'default :background)))
    (if (or (eq 'unspecified color) (eq 'unspecified bg))
        "white"
      (apply 'nei--average-colors (cons color (make-list factor bg)))
      )
    )
  )


(provide 'nei-util)
