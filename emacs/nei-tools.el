
(defvar nei--detect-ipynb-regexp "{\n \"cells\": \\[\n  {\n" ;; e.g for string-match: 
  "Regular expression used to detect IPYNB JSON")

(defun nei--visited-file-type ()
  "Returns PY or IPYNB or nil if no file is being visited"
  (cond ((s-ends-with? ".ipynb" (buffer-file-name)) "IPYNB")
        ((s-ends-with? ".py"(buffer-file-name)) "PY")))



(defun nei--insert-ipynb-point (&optional insert-anywhere)
  ;; Can make safer - e.g check for EOL
  "Added a special text marker for the point in IPYNB buffer to track the point.
   Returns t if the marker is added, nil otherwise.

  If anywhere, will insert the marker at the point position which may break the JSON.
  "
  (if insert-anywhere (insert "NEI-POINT>")
    (ignore-errors
      (let ((nei-point-marker "NEI-POINT>")
            (line-boundary (and (looking-back "^    " 4)
                                (s-equals? (string (char-after)) "\""))))
        (if line-boundary
            (progn
              (forward-char)
              (insert nei-point-marker)))
        t
        )
      )
    )
  )

(defun nei--jump-to-ipynb-point ()
  (beginning-of-buffer)
  (while (re-search-forward "NEI-POINT>" nil t)
    (replace-match "" nil nil))
  (point)
  )

(defun nei--marked-source (&optional anywhere)
  (save-excursion
    (nei--insert-ipynb-point anywhere)
    (let ((source  (buffer-string)))
      (nei--jump-to-ipynb-point)
      (set-buffer-modified-p nil)
      source
      )
    )
  )

(defun nei-parse-buffer (backoff)
  (condition-case nil
      (nei--cells-to-text
       (nei-parse-json (json-read-from-string (nei--marked-source backoff))))
    (error (nei-parse-json (json-read-from-string (buffer-string))))
    )
  )

(defun nei-add-file-mode-line ()
  "Add a file mode line"
  (interactive)
  (beginning-of-buffer)
  (insert "# -*- mode: python; eval: (nei-mode)-*-\n\n")
  )


(defun nei-view-ipynb ()
  "Open a NEI view on an IPYNB file"
  (interactive)
  (nei--ipynb-buffer t)
  )

(defun nei--ipynb-buffer (&optional backoff background keep-original)
  "backoff - attempt reparse. 
   background - switch to buffer.
   keep-original - kill original buffer or not"
  (let* ((text (nei-parse-buffer backoff))
         (new-buffer (get-buffer-create
                      (s-prepend "NEI:" (buffer-name)))
                     ))

    (with-current-buffer new-buffer
      (if (not background) 
          (switch-to-buffer new-buffer))
      (erase-buffer) ;; The buffer may already exist (e.g visiting a different point)
      (insert text)
      (nei-mode)
      (goto-char (nei--jump-to-ipynb-point))
      )
    
    (if (not keep-original)
        (kill-buffer (current-buffer)))
    )
  (set-buffer-modified-p nil)
  )



(defun nei-next-error-hook ()
  (if (s-equals? (nei--visited-file-type) "IPYNB") 
      (nei--ipynb-buffer)
    )
  )

(defun nei--ipynb-suggestion ()
  (message "Switch to NEI mode using the nei-view-ipynb command")
  (fundamental-mode)
  )

(defun nei-tools () ;; Make into toggle
  "Uses magic-fallback-mode-alist to handle .ipynb files opened in dired mode"
  (interactive)
  (add-hook 'next-error-hook #'nei-next-error-hook)
  (setq magic-fallback-mode-alist
            (append
             '(("{\n \"cells\": \\[\n  {\n" . nei--ipynb-suggestion))
             magic-fallback-mode-alist))
  )

(provide 'nei-tools)
