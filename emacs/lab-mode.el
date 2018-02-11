(require 'cl-lib) ;; For keyword argument support
(require 'websocket)
(require 'json)

(require 'lab-parse)
(require 'lab-interface)
(require 'lab-commands)


(defvar labmode-env "root"
  "Labmode assumes the use of miniconda3 with the stated env if labmode-python-path is nil

To set the env per file set this as a file variable e.g.
# -*- mode: python; labmode-env \"example-env\": nil; eval: (lab-mode)-*-")

(defvar labmode-python-path nil
  "Explicit path to the Python executable used to launch the labmode server.")

(defvar labmode-relative-server-path "../server/labmode.py"
  "Relative path from elisp directory to the labmode server script." )

(defvar ws-connection nil
  "The websocket client connection.")

(defvar ws-messages nil
  "Messages received over the websocket connection.")

(defvar ws-closed nil
  "Flag indicating whether the websocket connection is closed or not")

(defvar labmode--execution-count 0
  "The number of kernel executions invoked from labmode")

(defvar labmode-external-server nil
  "Whether to run the server process in emacs or externally. Used for debugging.")

(defvar labmode-browser "firefox"
  "The browser used by labmode when launch new tabs.")



(defun labmode--open-websocket ()
  (progn
    (setq conn (websocket-open
                "ws://127.0.0.1:9999"
                :on-message (lambda (_websocket frame)
                              (push (websocket-frame-text frame) ws-messages)
                              (message "ws frame: %S" (websocket-frame-text frame))
                              (error "Test error (expected)"))
                :on-close (lambda (_websocket) (setq ws-closed t))
                ;; New connection, reset execution count.
                :on-open (lambda (_websocket) (setq labmode--execution-count 0)))
          )
    (setq ws-connection conn)
    )
  )

(defun labmode--open-connection (&optional quiet)
  "Opens a new websocket connection if needed"
  (if (or (null ws-connection) ws-closed)
      (progn
        (labmode--open-websocket)
        (setq ws-closed nil)
        )
    (if (not quiet)
        (message "Websocket connection already open")
      )
    )
  )

(defun labmode--close-connection ()
  "Close the websocket connection."
  (websocket-close ws-connection)
  )


(defun labmode--wait-connection ()
  "Start server if not already running and loop connection attempts 
   until connection established "
  (start-labmode-server)
  (if ws-closed (setq ws-connection nil))

  (if (and (not labmode-external-server) (get-process "lab-server"))
      (while (null ws-connection)
        (sleep-for 0 200)
        (message "No connection (waiting)")
        (ignore-errors
          (labmode--open-connection)
          )
        )
    (if (not labmode-external-server)
        (message "Error: Server process not running. Is the conda env specified?"))
    )
  )

;;=============================;;
;; Managing the server process ;;
;;=============================;;

(defun start-labmode-server (&optional verbose)
  "Starts the labmode server if it isn't already running using labmode-python-path"
  (interactive)
  (let ((proc (get-process "lab-server")))
    (if (and (null proc) (not labmode-external-server))
        (progn 
          (message "Starting labmode server")
          (let* ((python-path
                 (if (null labmode-python-path)
                     (concat (expand-file-name "~")
                             "/miniconda3/envs/" labmode-env "/bin/python3")
                   labmode-python-path))
                 (new-proc
                 (start-process "lab-server"
                                " *lab server log*" ;; Leading space hides the buffer
                                python-path
                                (expand-file-name labmode-relative-server-path))))
            (set-process-query-on-exit-flag new-proc nil)
            (sleep-for 2)
            )
          )
      (if (and verbose (not labmode-external-server))
          (message "Labmode server already running"))
      )
    )
  )


(defun stop-labmode-server ()
  "Kills the labmode server if it is currently running"
  (interactive)
  (let ((proc (get-process "lab-server")))
    
    (if (not (null proc))
        (let ((proc-buffer  (process-buffer proc))) 
          (kill-process proc)
          (sleep-for 0 500)
          (kill-buffer proc-buffer)
          )
      )
    )
  )



(defun labmode-server-log ()
  "View the server log buffer if the server process is running."
  (interactive)
  (let ((proc (get-process "lab-server")))
    (if (not (null proc))
        (with-current-buffer (process-buffer proc)
          (clone-indirect-buffer " *lab server log*" t)
          )
      )
    )
  )

;;========================;;
;; Sending data to server ;;
;;========================;;

(defun labmode--send-data (text)
  "Send some text/data over the websocket connection if it is open"
  (if (null ws-connection)
      (message "Websocket session has not been initialized")
    (websocket-send-text ws-connection text)
    )
  )

(defun labmode--send-json (obj)
    "JSON encode an object and send it over the websocket connection."
  (labmode--send-data (json-encode obj))
  )



(defun labmode-bindings (map)
  ;; Capitalized commands
  (define-key map (kbd "C-c W") 'labmode-write-notebook)
  (define-key map (kbd "C-c I") 'labmode-insert-notebook)
  (define-key map (kbd "C-c E") 'labmode-exec-by-line)
  (define-key map (kbd "C-c L") 'labmode-clear-all-cell-outputs)
  (define-key map (kbd "C-c C") 'labmode-update-css)

  
  (define-key map (kbd "C-c w") 'labmode-move-cell-up)
  (define-key map (kbd "C-c s") 'labmode-move-cell-down)
  (define-key map (kbd "C-c <down>") 'labmode-move-point-to-next-cell)
  (define-key map (kbd "C-c <up>") 'labmode-move-point-to-previous-cell)
  (define-key map (kbd "C-c c") 'labmode-insert-code-cell)
  (define-key map (kbd "C-c m") 'labmode-insert-markdown-cell)
  (define-key map (kbd "C-c e") 'labmode-exec-by-line-and-move-to-next-cell)
  (define-key map (kbd "C-c i") 'labmode-interrupt-kernel)
  (define-key map (kbd "C-c r") 'labmode-restart-kernel)
  (define-key map (kbd "C-c l") 'labmode-clear-cell-by-line)
  (define-key map (kbd "C-c n") 'labmode-clear-notebook-and-restart)

  (define-key map (kbd "C-c v") 'labmode-view-browser)
  (define-key map (kbd "C-c V") 'labmode-view-notebook)
  map
)



(define-minor-mode lab-mode
  "Labmode for authoring notebooks in Emacs."
  :lighter " LABMODE"
  
  :keymap (let ((map (make-sparse-keymap)))
            (labmode-bindings map))

  (if (not labmode-external-server)
      (progn
        (labmode-update-config)
        (labmode-start-mirroring)
        )
    )
  (labmode-fontify)

  )
  


(defun labmode--enable-python-mode-advice (&optional arg)
  "Enable Python major mode when labmode enabled if necessary"
  (if (not (string= major-mode "python-mode"))
      (python-mode))
  )
(advice-add 'lab-mode :before #'labmode--enable-python-mode-advice)


;; Future ideas
;; C-c f for 'focus on cell'
;; C-c p for 'ping cell' to scroll to cell.

(provide 'lab-mode)
