(require 'cl-lib) ;; For keyword argument support
(require 'websocket)
(require 'json)

(require 'nei-parse)
(require 'nei-edit)
(require 'nei-commands)
(require 'nei-server)


(defvar ws-connection nil
  "The websocket client connection.")

(defvar ws-messages nil
  "Messages received over the websocket connection.")

(defvar ws-closed nil
  "Flag indicating whether the websocket connection is closed or not")

(defvar nei--execution-count 0
  "The number of kernel executions invoked from nei")

(defvar nei-browser "firefox"
  "The browser used by nei when launch new tabs.")



(defun nei--open-websocket ()
  (progn
    (setq conn (websocket-open
                "ws://127.0.0.1:9999"
                :on-message (lambda (_websocket frame)
                              (push (websocket-frame-text frame) ws-messages)
                              (message "ws frame: %S" (websocket-frame-text frame))
                              (error "Test error (expected)"))
                :on-close (lambda (_websocket) (setq ws-closed t))
                ;; New connection, reset execution count.
                :on-open (lambda (_websocket) (setq nei--execution-count 0)))
          )
    (setq ws-connection conn)
    )
  )

(defun nei--open-connection (&optional quiet)
  "Opens a new websocket connection if needed"
  (if (or (null ws-connection) ws-closed)
      (progn
        (nei--open-websocket)
        (setq ws-closed nil)
        )
    (if (not quiet)
        (message "Websocket connection already open")
      )
    )
  )

(defun nei--close-connection ()
  "Close the websocket connection."
  (websocket-close ws-connection)
  )


(defun nei--wait-connection ()
  "Start server if not already running and loop connection attempts 
   until connection established "
  (nei--start-server)
  (if ws-closed (setq ws-connection nil))

  (if ((get-process "nei-server"))
      (while (null ws-connection)
        (sleep-for 0 200)
        (message "No connection (waiting)")
        (ignore-errors
          (nei--open-connection)
          )
        )
    )
  )

;;========================;;
;; Sending data to server ;;
;;========================;;

(defun nei--send-data (text)
  "Send some text/data over the websocket connection if it is open"
  (if (null ws-connection)
      (message "Websocket session has not been initialized")
    (websocket-send-text ws-connection text)
    )
  )

(defun nei--send-json (obj)
    "JSON encode an object and send it over the websocket connection."
  (nei--send-data (json-encode obj))
  )



(defun nei-bindings (map)
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



(define-minor-mode nei-mode
  "Nei for authoring notebooks in Emacs."
  :lighter " NEI"
  
  :keymap (let ((map (make-sparse-keymap)))
            (nei-bindings map))

  (if (not nei-external-server)
      (progn
        (nei-update-config)
        (nei-start-mirroring)
        )
    )
  (nei-fontify)

  )
  


(defun nei--enable-python-mode-advice (&optional arg)
  "Enable Python major mode when nei enabled if necessary"
  (if (not (string= major-mode "python-mode"))
      (python-mode))
  )
(advice-add 'nei-mode :before #'nei--enable-python-mode-advice)


;; Future ideas
;; C-c f for 'focus on cell'
;; C-c p for 'ping cell' to scroll to cell.


(provide 'nei)