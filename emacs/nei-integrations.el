;;;  -*- lexical-binding: t; -*-

;; Utility functions for integration with other emacs functionality or
;; other tools

(defvar nei--ediff-window-list nil "Used to restore windows when ediff is quit")

(defvar nei--ediff-unstaged-buffer-name nil "Tracks buffer name for unstaged changes")

(defvar nei--ediff-revision-buffer-name nil "Tracks buffer name for revision")


(defun nei-global-config (add-file-menu-entry rgrep-integration connect
                          magic-alist view-ipynb completions magit-diff)
  "Function to enable global integrations, to be enabled in .emacs.

  add-file-menu-entry: Add 'Visit New Notebook' to the File menu (C-c F).
  rgrep-integration:   Add next-error-hook for rgrep support
  connect:             Start NEI server and establish websocket connection
  magic-alist:         Suggest hint when viewing notebook JSON in buffers
  view-ipynb:          Set global shortcut for viewing ipynb buffers (C-c I)
  "
  (add-to-list 'desktop-buffer-mode-handlers
               '(python-mode . nei--desktop-restore-file-buffer))
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

;; Integration with rgrep

(defun nei-next-error-hook ()
  (if (s-equals? (nei--visited-file-type) "IPYNB") 
      (nei--ipynb-buffer)
    )
  )

(defun nei-rgrep-integration () ;; Make into toggle
  "Sets up custom next error hook for use from rgrep"
  (interactive)
  (add-hook 'next-error-hook #'nei-next-error-hook)
)


;; Integration with desktop mode

(defun nei--desktop-restore-file-buffer (desktop-buffer-filename
                                         desktop-buffer-name
                                         desktop-ipynb-filename)
  "Function to restore NEI buffers visiting IPYNB files when using desktop mode"
  (if (and (s-ends-with? ".ipynb" desktop-buffer-name) desktop-ipynb-filename)
      (nei-open-notebook desktop-ipynb-filename)
    (desktop-restore-file-buffer desktop-buffer-filename
                                 desktop-buffer-name
                                 desktop-ipynb-filename)
    )
  )

(defun nei--desktop-save-buffer (desktop-dirname)
  "Integration with desktop mode by saving notebook path to .desktop file"
  (if (boundp 'nei--ipynb-buffer-filename) nei--ipynb-buffer-filename nil)
  )

;; Integration with magit

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


;; Integration with ediff

(defun nei--cleanup-ediff-buffers ()
  "For use with an 'ediff-quit-hook"
  (kill-buffer nei--ediff-unstaged-buffer-name)
  (kill-buffer nei--ediff-revision-buffer-name)
  (setq nei--ediff-unstaged-buffer-name nil)
  (setq nei--ediff-revision-buffer-name nil)
  (set-frame-configuration nei--ediff-window-list)
  )

(provide 'nei-integrations)


;; MISC NOTES/ TODOS:

;; ;; (quelpa '(nei :fetcher github :repo "pyviz/nei" :branch "master" :files ("emacs/*.el")))

;; Binding: M-Enter -> ping or C-L (C-S-L)
;; (file-locked-p) useful?
;; In both server and comms, command list handling could be simplified
;; nei-insert-escaped-triple-quotes could have support for escaping a region
