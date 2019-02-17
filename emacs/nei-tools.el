

(defvar nei--detect-ipynb-regexp "{\n \"cells\": \\[\n  {\n" ;; e.g for string-match: 
  "Regular expression used to detect IPYNB JSON")

(defun nei--visited-file-type ()
  "Returns PY or IPYNB or nil if no file is being visited"
  (cond ((s-ends-with? ".ipynb" (buffer-file-name)) "IPYNB")
        ((s-ends-with? ".py"(buffer-file-name)) "PY")))


(defun nei--insert-ipynb-point () ;; Can make safer - e.g check for EOL
  "Added a special text marker for the point in IPYNB buffer to track the point.
   Returns t if the marker is added, nil otherwise"
  (ignore-errors ;; E.g regexp fails because at start of file
    (let ((nei-point-marker "NEI-POINT>")
          (line-boundary (and (looking-back "^    " 4)
                              (s-equals? (string (char-after)) "\""))))
      (if line-boundary (forward-char))
      (insert nei-point-marker)
      t
      )
    )
  )

(defun nei--jump-to-ipynb-point ()
  (beginning-of-buffer)
  (perform-replace "NEI-POINT>" "" nil nil nil)
  )

(defun nei--from-ipynb (&optional file-mode-line)
  (let* ((marked-point (nei--insert-ipynb-point))
         (cells (nei-parse-json (json-read-from-string (buffer-string))))
         (text (nei--cells-to-text cells)))
    (erase-buffer)
    (if file-mode-line
        (insert "# -*- mode: python; eval: (nei-mode)-*-\n\n")
      )
    (insert text)
    (nei-mode)
    (if marked-point (nei--jump-to-ipynb-point))
    )
  )


(defun nei-enable-dired-mode () ;; Make into toggle
  "Uses magic-fallback-mode-alist to handle .ipynb files opened in dired mode"
  (interactive)
  (setq magic-fallback-mode-alist
            (append
             '(("{\n \"cells\": \\[\n  {\n" . nei-mode))
             magic-fallback-mode-alist))
  )

(provide 'nei-tools)
