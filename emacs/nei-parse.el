;;;  -*- lexical-binding: t; -*-

(require 's)
(require 'nei-util)

(defun nei--markdown-cell (source)
  (list (cons 'mode "markdown") (cons 'source source))
  )

;; Can also supply 'input' (i.e HTML) from emacs. No longer used.
(defun nei--code-cell (source &optional prompt)
  (list (cons 'mode "code")
        (cons 'source source)
        (cons 'prompt prompt)
        )
  )

(defun nei--split-newlines (string)
  (mapcar (lambda (x) (concat x "\n")) (split-string string "\n"))
  )

;;=====================================;;
;; Parsing cells from .ipynb JSON file ;;
;;=====================================;;

(defun nei-parse-notebook-file (filename)
  "Given an .ipynb file return the parsed list of cells"
  (nei-parse-json (json-read-file filename))
  )

(defun nei-parse-json (data)
  "Extract structure from JSON parsed using json-read-file or json-read-from-string"
  (let* ((extracted)
         (cells (assoc-value 'cells data))
         (metadata (assoc-value 'metadata data))
         (nbformat (assoc-value 'nbformat data))
         (nbformat-minor (assoc-value 'nbformat_minor data)))
    (dolist (cell (append cells nil) extracted)

      (let* ((mode (assoc-value 'cell_type cell))
             (source  (s-join "" (assoc-value 'source cell)))
             (prompt (assoc-value 'execution_count cell))
             (nei-cell (if (string= mode "code")
                           (nei--code-cell source prompt)
                         (nei--markdown-cell source))))
        (setq extracted (push nei-cell extracted))
        )
      )
    (reverse extracted)
    )
)

;;==============================;;
;; Rendering cells back to text ;;
;;==============================;;

(defun nei--code-cell-to-text (cell)
  "Given a code cell return the textual equivalent"
  (let* ((source (assoc-value 'source cell))
        (prompt (assoc-value 'prompt cell))
        (prompt-line (if (null prompt)
                         "# In[ ]\n"
                       (s-concat "# In[" (format "%s" prompt) "]\n"))))
    (s-concat prompt-line  source)
    )
)

(defun nei--markdown-cell-to-text (cell)
  "Given a markdown cell return the textual equivalent"
  (s-concat "\"\"\"\n" (assoc-value 'source cell) "\n\"\"\"")
)


(defun nei--cells-to-text (cells)
  "Given a list of cells return the textual equivalent"
  (let ((lines nil))
    (dolist (cell cells lines)
      (if (string= (assoc-value 'mode cell) "code")
          (setq lines (push (nei--code-cell-to-text cell) lines))
        (setq lines (push (nei--markdown-cell-to-text cell) lines))
        )
      )
    (s-join "\n\n" (reverse lines))
    )
)
(provide 'nei-parse)
