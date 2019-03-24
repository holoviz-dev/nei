;;;  -*- lexical-binding: t; -*-
;; Module for utilities

(require 'htmlize)

(defun nei--logging (&rest args)
  (if nei-verbose (apply 'message  args))
  )

(defun assoc-value (key alist)
  "Helper to make accessing keys from an alist more readable"
  (cdr (assoc key alist))
)

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

;;==================================;;
;; Utilities to htmlize python code ;;
;;==================================;;


(defun nei--htmlize-css-to-highlight (css)
  "Map the CSS from htmlize to the keywords used by highlight.js"
  (setq css (replace-regexp-in-string ".keyword" ".hljs-keyword" css))
  (setq css (replace-regexp-in-string ".comment" ".hljs-comment" css))
  (setq css (replace-regexp-in-string ".doc" ".hljs-string" css))
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

(provide 'nei-util)
