(require 'cl-lib) ;; For keyword argument support
(require 'websocket)
(require 'json)

(require 'nei-parse)
(require 'nei-edit)
(require 'nei-commands)
(require 'nei-server)
(require 'nei-bindings)


(defvar ws-connection nil
  "The websocket client connection.")

(defvar ws-messages nil
  "Messages received over the websocket connection.")

(defvar nei--unexpected-disconnect nil
  "Flag indicating whether the websocket connection is closed or not")

(defvar nei--execution-count 0
  "The number of kernel executions invoked from NEI")

(defvar nei-browser "firefox"
  "The browser used by NEI when launch new tabs.")



(defun nei--open-websocket ()
  (progn
    (setq conn (websocket-open
                "ws://127.0.0.1:9999"
                :on-message (lambda (_websocket frame)
                              (push (websocket-frame-text frame) ws-messages)
                              (message "ws frame: %S" (websocket-frame-text frame))
                              (error "Test error (expected)"))
                :on-close (lambda (_websocket) (setq nei--unexpected-disconnect t))
                ;; New connection, reset execution count.
                :on-open (lambda (_websocket) (setq nei--execution-count 0)))
          )
    (setq ws-connection conn)
    )
  )

(defun nei--open-ws-connection (&optional quiet)
  "Opens a new websocket connection if needed"
  (if (or (null ws-connection) nei--unexpected-disconnect)
      (progn
        (setq nei--unexpected-disconnect nil)
        (nei--open-websocket)
        )
    (if (not quiet)
        (message "Websocket connection already open")
      )
    )
  )


(defun nei-connect ()
  "Start the NEI server, establish the websocket connection and begin mirroring"
  (interactive)
  (if (and ws-connection (null nei--unexpected-disconnect))
      (message "Already connected to NEI server")
    (progn 
      (nei--start-server)
      (nei--open-ws-connection)
      (nei-update-config)
      (nei-start-mirroring)
      )
    )
  )

(defun nei-disconnect ()
  "Close the websocket and shutdown the server"
  (interactive)
  (nei--close-ws-connection)
  (nei--stop-nei-server)
  )

(defun nei--close-ws-connection ()
  "Close the websocket connection."
  (websocket-close ws-connection)
  (setq ws-connection nil)
  )


(defun nei--disconnection-error ()
  (nei-stop-mirroring)
  (websocket-close ws-connection)
  (message "Unexpected disconnection")
  (setq nei--unexpected-disconnect nil)
  (nei--close-ws-connection)
  )

;;========================;;
;; Sending data to server ;;
;;========================;;


(defun nei--send-data (text &optional warn-no-connection)
  "Runs the callback if there is a connection and handles unexpected disconnects."
  (cond (nei--unexpected-disconnect (nei--disconnection-error))
        ((null ws-connection) (if warn (message "Not connected to NEI server")))
        (t (progn
             (websocket-send-text ws-connection text)
             (if nei--unexpected-disconnect (nei--disconnection-error)))
           )
        )
  )

(defun nei--send-json (obj &optional warn-no-connection)
    "JSON encode an object and send it over the websocket connection."
    (nei--send-data (json-encode obj) warn-no-connection)
    )


(define-minor-mode nei-mode
  "Nei for authoring notebooks in Emacs."
  :lighter " NEI"
  
  :keymap nei-mode-map
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