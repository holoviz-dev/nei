;; File defining NEI key bindings and bindings to menu entries.
(require 'nei-commands)


(defun nei--define-key-map (map)
  ;; Capitalized commands
  (define-key map (kbd "C-c W") 'nei-write-notebook)
  (define-key map (kbd "C-c I") 'nei-insert-notebook)
  (define-key map (kbd "C-c E") 'nei-exec-by-line)
  (define-key map (kbd "C-c L") 'nei-clear-all-cell-outputs)
  (define-key map (kbd "C-c C") 'nei-update-css)

  (define-key map (kbd "C-c w") 'nei-move-cell-up)
  (define-key map (kbd "C-c s") 'nei-move-cell-down)
  (define-key map (kbd "C-c <down>") 'nei-move-point-to-next-cell)
  (define-key map (kbd "C-c <up>") 'nei-move-point-to-previous-cell)
  (define-key map (kbd "C-c c") 'nei-insert-code-cell)
  (define-key map (kbd "C-c m") 'nei-insert-markdown-cell)
  (define-key map (kbd "C-c e") 'nei-exec-by-line-and-move-to-next-cell)
  (define-key map (kbd "C-c i") 'nei-interrupt-kernel)
  (define-key map (kbd "C-c r") 'nei-restart-kernel)
  (define-key map (kbd "C-c l") 'nei-clear-cell-by-line)
  (define-key map (kbd "C-c n") 'nei-clear-notebook-and-restart)

  (define-key map (kbd "C-c v") 'nei-view-browser)
  (define-key map (kbd "C-c V") 'nei-view-notebook)

  (define-key map (kbd "C-c ,") 'nei-scroll-up)
  (define-key map (kbd "C-c .") 'nei-scroll-down)
  map
  )


(defvar nei-mode-map (nei--define-key-map (make-sparse-keymap))
  "The sparse keymap with NEI bindings.")


(defun nei--buffers-with-kernels ()
  "Returns a list of buffers with attached kernels"
  (seq-filter
   (lambda (buff) (buffer-local-value 'nei--active-kernel buff))
   (buffer-list))
  )


(defun nei-list-buffers-with-kernels ()
  "Opens a temporary buffer containing a list of buffers with attatched kernels"
  (interactive)
  (with-output-to-temp-buffer "NEI Kernels"
    (dolist (buff (nei--buffers-with-kernels))
      (princ (buffer-name buff))
      )
    )
  )


(defun nei--update-kernel-menu-entry (add-entry)
  "Adds or removes a dynamic menu entry for buffers with attached kernels"
  (lexical-let ((entry-name (buffer-name)))
    (define-key nei-mode-map
      (vector 'menu-bar 'NEI 'Kernel 'Live\ Kernels (make-symbol entry-name))
      (if add-entry (cons (buffer-name)
                          (lambda () (interactive) (switch-to-buffer entry-name))
                          ) nil)
      )
    )
  )


(defun nei--menu-stub ()
  (interactive)
  (message "Menu stub")
  )

(easy-menu-define nei-mode-menu nei-mode-map
  "Notebook Emacs Interface"
  '("NEI"
    ("Kernel"
      ["Start" nei-start-kernel (not nei--active-kernel)]
      ["Interrupt" nei-interrupt-kernel nei--active-kernel]
      ["Restart" nei-restart-kernel nei--active-kernel]
      ["Shutdown" nei-shutdown-kernel nei--active-kernel]
      "---"
      ("Live Kernels"
       "---"
       ["Buffer List"
        nei-list-buffers-with-kernels (nei--buffers-with-kernels)]
       )
      )
    ("Server"
     ("Actions"
      ["Start and Connect" nei-connect (not ws-connection)]
      ["Halt and Disconnect" nei-disconnect ws-connection]
      )
     ("Install"
      ["Pip install" nei-server-pip-install (not (nei--server-available))]
      )
     ["Information" nei-server-info t]
     ["Message Log" nei-server-log t]
     )
    ("Browser"
     ["View" nei-view-browser ws-connection]
     ["HTML Preview" nei-view-notebook ws-connection] 
     "---"
     ["Scroll up" nei-scroll-up ws-connection]
     ["Scroll down" nei-scroll-down ws-connection]
     ["Update CSS" nei-update-css ws-connection]
     )
    "---"
    ("Buffer"
      ;; Switch buffer?
      ["Insert mode line" nei--menu-stub t]
      ["Fontify" nei-toggle-fontify :style radio :selected nei--fontified]
      )
    ("Edit"
     ["Insert Code" nei-insert-code-cell t]
     ["Insert Markdown" nei-insert-markdown-cell t]
     "---"
     ["Next Cell" nei-move-point-to-next-cell t]
     ["Previous Cell" nei-move-point-to-previous-cell t]
     "---"
     ["Move Cell Up" nei-move-cell-up t]
     ["Move Cell Down" nei-move-cell-down t]
     ["Toggle rgrep integration" nei-rgrep-integration t] ;; Should be a toggle
     )
    ("Operations"
     ["Execute In Place" nei-exec-by-line ws-connection]
     ["Execute and Move" nei-exec-by-line-and-move-to-next-cell ws-connection]
     ["Clear cell output"  nei-clear-cell-by-line ws-connection]
     ["Clear all cell output"  nei-clear-all-cell-outputs ws-connection]
     ["Clear Notebook and Restart" nei-clear-notebook-and-restart ws-connection]
     )
    ("Notebook"
     ["Insert Notebook" nei-insert-notebook] ;; Insert Notebook At Point?
     ["Write Notebook" nei-write-notebook])
    )
  )


(provide 'nei-bindings)
