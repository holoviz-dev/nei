;;;  -*- lexical-binding: t; -*-

(defvar nei--detect-ipynb-regexp "{\n \"cells\": \\[\n  {\n" ;; e.g for string-match: 
  "Regular expression used to detect IPYNB JSON")

(defvar nei--point-marker "NEI-POINT>"
  "Magic string used to track the point from the JSON source in ipynb files"
  )

(defvar nei--ediff-window-list nil "Used to restore windows when ediff is quit")

(defvar nei--ediff-unstaged-buffer-name nil "Tracks buffer name for unstaged changes")

(defvar nei--ediff-revision-buffer-name nil "Tracks buffer name for revision")


(defun nei--write-file-hook ()
  (or (nei--write-ipynb-hook) (nei--write-python-hook) (nei--write-buffer-to-file-hook))
  )



(defun nei--write-buffer-to-file-hook () ; TODO: Handle overwriting files
  (if (and (not (buffer-file-name))
           (not nei--ipynb-buffer-filename))
      (let ((selected-filename 
             (nei--prompt-for-filename-until-predicate
              "File to save in: "
              "Please specify a file ending in either .py or .ipynb"
              (lambda (x) (or (s-ends-with? ".ipynb" x) (s-ends-with? ".py" x))))
             )
            )
        (if (s-ends-with? ".py" selected-filename)
            (progn
              (set-visited-file-name selected-filename)
              (nei--write-python-hook))
          (progn
            (nei--write-notebook
             (if nei-write-notebook-output "full-notebook" "cleared")
             selected-filename)
            (kill-buffer)
            (find-file selected-filename)
            (nei-view-ipynb)
            )
         )
        )
    t
    )
  )

(defun nei--write-python-hook ()
  (if (and (buffer-file-name)
           (s-ends-with? ".py" (buffer-file-name))
           nei-write-cleared-python-prompts)
      (progn
        (write-region
         (replace-regexp-in-string "# In\\[.*\\]" "# In[ ]" (buffer-string))
         nil buffer-file-name nil t)
        t)
    nil)
  )

(defun nei--write-ipynb-hook ()
  (if (and (s-starts-with? "NEI>" (buffer-name))
           (s-ends-with? ".ipynb" (buffer-name))
           (and (null (buffer-file-name))
                nei--ipynb-buffer-filename))
      (progn
        (nei--write-notebook
         (if nei-write-notebook-output "full-notebook" "cleared")
         nei--ipynb-buffer-filename)
        (set-buffer-modified-p nil)
        ;; Disable revert checks until file confirmed as written with write_complete message.
        (set-visited-file-modtime 0)
        t
        )
    nil
    )
  )


(defun nei--visited-file-type ()
  "Returns PY or IPYNB or nil if no file is being visited"
  (cond ((s-ends-with? ".ipynb" (buffer-file-name)) "IPYNB")
        ((s-ends-with? ".py"(buffer-file-name)) "PY")))



(defun nei--insert-ipynb-point (&optional insert-anywhere)
  ;; Can make safer - e.g check for EOL
  "Added a special text marker for the point in IPYNB buffer to track the point.
   Returns t if the marker is added, nil otherwise.

   If anywhere is true, this function will insert the marker exactly at
   the point position which may break the JSON.
  "
  (if insert-anywhere (insert nei--point-marker)
    (ignore-errors
      (let ((nei-point-marker nei--point-marker)
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


(defun nei--locate-ipynb-point (text)
  "Without mutating the current buffer, return a cons of the appropriate
   point and text cleaned of the temporary marker"
  (with-temp-buffer
    (insert text)
    (beginning-of-buffer)
    (while (re-search-forward nei--point-marker nil t)
      (replace-match "" nil nil))
    (cons (point) (buffer-string))
    )
  )


(defun nei--marked-source (&optional anywhere)
  (let ((buffer-contents (buffer-string))
        (point-pos (point)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char point-pos)
      (nei--insert-ipynb-point anywhere)
      (buffer-string)
      )
    )
  )

(defun nei-parse-ipynb-buffer (backoff)
  "Parse the JSON contents of an ipynb buffer returning the results as Python code"
  (condition-case nil
      (nei--cells-to-text
       (nei-parse-json (json-read-from-string (nei--marked-source backoff))))
    (error (nei--cells-to-text
            (nei-parse-json (json-read-from-string (buffer-string)))))
    )
  )

(defun nei-view-ipynb ()
  "Open a NEI view on an IPYNB file"
  (interactive)
  (condition-case nil
      (nei--ipynb-buffer t)
    (error (message "This buffer does not appear to be valid notebook JSON"))
    )
  )


(defun nei--ipynb-buffer (&optional backoff background keep-original)
  "backoff - attempt reparse. 
   background - switch to buffer.
   keep-original - kill original buffer or not"
  (let* ((nei-cells (nei-parse-json (json-read-from-string (buffer-string))))
         (text (nei-parse-ipynb-buffer backoff))
         (ipynb-source-filename (buffer-file-name))
         (nei-buffer-name (s-prepend "NEI>" (buffer-name)))
         (new-bufferp (not (get-buffer nei-buffer-name)))
         (nei-buffer (get-buffer-create nei-buffer-name))
         (nei-buffer-text nil))
         
      
      (with-current-buffer nei-buffer
        (let* ((point-and-clean-text (nei--locate-ipynb-point text))
               (new-point-pos (car point-and-clean-text))
               (clean-text (cdr point-and-clean-text)))
             
          (if new-bufferp
              (progn (insert clean-text) (nei-mode)))

          (setq nei--ipynb-buffer-filename ipynb-source-filename)
          (setq nei-buffer-text clean-text)
          (if (not background)
              (progn 
                (switch-to-buffer nei-buffer)
                (goto-char new-point-pos)
                (recenter)
                )
            )
          )

        (nei--load-from-file nei-cells nei--ipynb-buffer-filename nei-buffer-text)
        (set-buffer-modified-p nil)
        ;; Set modtime for revert system. TODO: Lock buffer?
        (set-visited-file-modtime (nth 5 (file-attributes nei--ipynb-buffer-filename)))
        )
    
      (if (not keep-original)
          (kill-buffer (current-buffer)))
      )
  )



(defun nei-next-error-hook ()
  (if (s-equals? (nei--visited-file-type) "IPYNB") 
      (nei--ipynb-buffer)
    )
  )

(defun nei--ipynb-suggestion ()
  (message "Switch to NEI mode using nei-view-ipynb (%s)"
           (mapconcat 'key-description (where-is-internal 'nei-view-ipynb) " "))
  (fundamental-mode)
  )

(defun nei-rgrep-integration () ;; Make into toggle
  "Sets up custom next error hook for use from rgrep"
  (interactive)
  (add-hook 'next-error-hook #'nei-next-error-hook)
)

(defun nei-magit-view-diff ()
  "When hovering on an unstaged notebook in magit, this function
   can be called to apply ediff to view the changes as plaintext"
  (interactive)
  (let ((filename (magit-current-file)))
    (if (s-ends-with? ".ipynb" filename)
        (progn
          (setq nei--ediff-window-list (current-frame-configuration))
          (delete-other-windows)
          (magit-diff-visit-file filename t)
          (setq nei--ediff-unstaged-buffer-name (s-prepend "NEI>" (buffer-name)))
          (nei-view-ipynb)
          (other-window 1)
          (magit-find-file "master" filename)
          (setq nei--ediff-revision-buffer-name (s-prepend "NEI>" (buffer-name)))
          (nei-view-ipynb)
          (ediff-buffers nei--ediff-unstaged-buffer-name nei--ediff-revision-buffer-name)
          (with-current-buffer nei--ediff-unstaged-buffer-name
            (setq buffer-read-only t))
          (with-current-buffer nei--ediff-revision-buffer-name
            (setq buffer-read-only t))
          )
      )
    )
  )

(defun nei--cleanup-ediff-buffers ()
  "For use with an 'ediff-quit-hook"
  (kill-buffer nei--ediff-unstaged-buffer-name)
  (kill-buffer nei--ediff-revision-buffer-name)
  (setq nei--ediff-unstaged-buffer-name nil)
  (setq nei--ediff-revision-buffer-name nil)
  (set-frame-configuration nei--ediff-window-list)
  )


(provide 'nei-tools)
