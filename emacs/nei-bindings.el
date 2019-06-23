;;;  -*- lexical-binding: t; -*-

;; File defining NEI key bindings and bindings to menu entries.
(require 'nei-commands)


(defun nei--define-key-map (map)
  ;; Capitalized commands
  (define-key map (kbd "C-c W") 'nei-write-notebook)
  (define-key map (kbd "C-c I") 'nei-insert-notebook)
  (define-key map (kbd "C-c F") 'nei-open-notebook)
  (define-key map (kbd "C-c E") 'nei-exec-by-line)
  (define-key map (kbd "C-c L") 'nei-clear-all-cell-outputs)
  (define-key map (kbd "C-c C") 'nei-update-theme)
  (define-key map (kbd "C-c M") 'nei-server-log)

  (define-key map (kbd "C-c w") 'nei-move-cell-up)
  (define-key map (kbd "C-c s") 'nei-move-cell-down)
  (define-key map (kbd "C-c <down>") 'nei-move-point-to-next-cell)
  (define-key map (kbd "C-c <up>") 'nei-move-point-to-previous-cell)
  (define-key map (kbd "C-c c") 'nei-insert-code-cell)
  (define-key map (kbd "C-c m") 'nei-insert-markdown-cell)
  (define-key map (kbd "C-c <DEL>") 'nei-delete-cell)
  (define-key map (kbd "C-c e") 'nei-exec-by-line-and-move-to-next-cell)
 
  (define-key map (kbd "C-c k") 'nei-start-kernel)
  (define-key map (kbd "C-c i") 'nei-interrupt-kernel)
  (define-key map (kbd "C-c r") 'nei-restart-kernel)
  (define-key map (kbd "C-c K") 'nei-shutdown-kernel)
  (define-key map (kbd "C-c l") 'nei-clear-cell-by-line)
  (define-key map (kbd "C-c n") 'nei-clear-notebook-and-restart)

  (define-key map (kbd "C-c v") 'nei-view-browser)
  (define-key map (kbd "C-c V") 'nei-view-notebook)

  (define-key map (kbd "C-c ,") 'nei-scroll-up)
  (define-key map (kbd "C-c .") 'nei-scroll-down)

  (define-key map (kbd "C-c h") 'nei-toggle-display-code)
  (define-key map (kbd "C-c H") 'nei-toggle-display-all-code)

  (define-key map (kbd "C-c '") 'nei-move-point-to-next-code-cell)
  (define-key map (kbd "C-c ;") 'nei-move-point-to-previous-code-cell)
  (define-key map (kbd "C-c @") 'nei-move-point-to-next-markdown-cell)
  (define-key map (kbd "C-c :") 'nei-move-point-to-previous-markdown-cell)
  
  ;; More unusual bindings
  (define-key map [(S-return)] 'nei-exec-by-line-and-move-to-next-cell)
  (define-key map (kbd "C-<wheel-up>") 'nei-scroll-up)
  (define-key map (kbd "C-<double-wheel-up>") 'nei-scroll-up)
  (define-key map (kbd "C-<triple-wheel-up>") 'nei-scroll-up)

  ;; In Firefox, useful to set mousewheel.with_meta.action = 1 in about:config
  (define-key map (kbd "C-<wheel-down>") 'nei-scroll-down)
  (define-key map (kbd "C-<double-wheel-down>") 'nei-scroll-down)
  (define-key map (kbd "C-<triple-wheel-down>") 'nei-scroll-down)
 
  map
  )


(defvar nei-mode-map (nei--define-key-map (make-sparse-keymap))
  "The sparse keymap with NEI bindings.")

;;==========;;
;; NEI MENU ;;
;;==========;;

(defun nei-toggle-fontify ()
  (interactive)
  (if nei--fontified (nei-defontify) (nei-fontify))
  )

(defun nei-toggle-write-cleared-python-prompts ()
  (interactive)
  (setq nei-write-cleared-python-prompts
        (not nei-write-cleared-python-prompts))
  )

(defun nei-toggle-write-notebook-output ()
  (interactive)
  (setq nei-write-notebook-output
        (not nei-write-notebook-output))
  )

(easy-menu-define nei-mode-menu nei-mode-map
  "Notebook Emacs Interface"
  '("NEI"
    ("Kernel"
      ("Buffers"
       "---"
       ["List Buffers"
        nei-list-buffers-with-kernels (nei--buffers-with-kernels)]
       )
      "---"
      ["Start" nei-start-kernel (not nei--active-kernel)]
      ["Start with..." nei-start-kernel-with (not nei--active-kernel)]
      ["Interrupt" nei-interrupt-kernel nei--active-kernel]
      ["Restart" nei-restart-kernel nei--active-kernel]
      ["Shutdown" nei-shutdown-kernel nei--active-kernel]
      )
    ["Execute In Place" nei-exec-by-line nei--active-kernel]
    ["Execute and Move" nei-exec-by-line-and-move-to-next-cell nei--active-kernel]
    "---"
    ["Next Cell" nei-move-point-to-next-cell t]
    ["Previous Cell" nei-move-point-to-previous-cell t]
    ("Navigate"
     ["Next Code Cell" nei-move-point-to-next-code-cell t]
     ["Previous Code Cell" nei-move-point-to-previous-code-cell t]
     ["Next Markdown Cell" nei-move-point-to-next-markdown-cell t]
     ["Previous Markdown  Cell" nei-move-point-to-previous-markdown-cell t]
     )
     "---"
    ["Insert Code" nei-insert-code-cell t]
    ["Insert Markdown" nei-insert-markdown-cell t]
    "---"
    ["Delete Cell" nei-delete-cell t]
    "---"
    ["Move Cell Up" nei-move-cell-up t]
    ["Move Cell Down" nei-move-cell-down t]
    "---"
    ("File"
     ["Visit New Notebook" nei-open-notebook nei--ws-connection]
     ["Write to IPYNB" nei-write-notebook nei--ws-connection]
     ["Export to HTML" nei-export-to-html nei--ws-connection]
     "---"
     ["Clear Python Prompts " nei-toggle-write-cleared-python-prompts
      :style toggle :selected nei-write-cleared-python-prompts]
     ["Write Notebook Output" nei-toggle-write-notebook-output
      :style toggle :selected nei-write-notebook-output]
     )
    ("Browser"
     ["View" nei-view-browser nei--ws-connection]
     ["HTML Preview" nei-view-notebook nei--ws-connection]
     "---"
     ["Clear cell output"  nei-clear-cell-by-line nei--ws-connection]
     ["Clear all outputs and prompts"  nei-clear-all-cell-outputs nei--ws-connection]
     "---"
     ["Toggle code display"  nei-toggle-display-code nei--ws-connection]
     ["Toggle all code display"  nei-toggle-display-all-code nei--ws-connection]
     "---"
     ["Scroll up" nei-scroll-up nei--ws-connection]
     ["Scroll down" nei-scroll-down nei--ws-connection]
     "---"
     ["Update CSS Theme" nei-update-theme nei--ws-connection]
     )
    ("Buffer"
     ["Insert Notebook at Point" nei-insert-notebook]
     ["Clear Notebook and Restart Kernel" nei-clear-notebook-and-restart nei--active-kernel]
     ["Insert escaped triple quotes (\"\"\")" nei-insert-escaped-triple-quotes]
     ["Fontify" nei-toggle-fontify :style radio :selected nei--fontified]
     )
    ("Server"
     ("Status"
      ["Start and Connect" nei-connect (not nei--ws-connection)]
      ["Halt and Disconnect" nei-disconnect nei--ws-connection]
      )
     ("Install"
      ["Pip install" nei-server-pip-install]
      ;; Using (not (nei--server-available)) makes the menu slow
      )
     ["Information" nei-server-info t]
     ["Message Log" nei-server-log t]
     )
    )
  )

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



(defun nei-global-config (add-file-menu-entry rgrep-integration connect
                          magic-alist view-ipynb completions magit-diff)
  "Function to enable global integrations, to be enabled in .emacs.

  add-file-menu-entry: Add 'Visit New Notebook' to the File menu (C-c F).
  rgrep-integration:   Add next-error-hook for rgrep support
  connect:             Start NEI server and establish websocket connection
  magic-alist:         Suggest hint when viewing notebook JSON in buffers
  view-ipynb:          Set global shortcut for viewing ipynb buffers (C-c I)
  "
  (if add-file-menu-entry
      (progn
        (global-set-key (kbd "C-c F") 'nei-open-notebook)
        (define-key-after
          (lookup-key global-map [menu-bar file])
          [open-notebook] '("Visit New Notebook" . nei-open-notebook) 'new-file)
        )
    )
  (if rgrep-integration (nei-rgrep-integration))
  (if connect
      (condition-case nil
          (nei-connect)
        (error (message "Could not connect to NEI server"))
        )
    )


  (if magic-alist
      ;; Suggests the use of nei-view-ipynb if notebook JSON detected
      (setq magic-fallback-mode-alist
            (append
             (cons (cons nei--detect-ipynb-regexp  'nei--ipynb-suggestion) nil)
             magic-fallback-mode-alist))
    )

  (if view-ipynb
      (global-set-key (kbd "C-c I") 'nei-view-ipynb)
    )

  (if completions
      (setq completion-at-point-functions
            '(nei-completion-at-point)
            )
    )

  (if magit-diff 
      (progn 
        (global-set-key (kbd "C-c d") 'nei-magit-view-diff)
        (add-hook 'ediff-quit-hook 'nei--cleanup-ediff-buffers)
        )
    )
)

(provide 'nei-bindings)
