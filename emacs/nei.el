;;;  -*- lexical-binding: t; -*-

(require 'cl-lib) ;; For keyword argument support
(require 'websocket)
(require 'json)

(require 'nei-parse)
(require 'nei-edit)
(require 'nei-commands)
(require 'nei-server)
(require 'nei-bindings)
(require 'nei-tools)
(require 'nei-at-point)

(defvar nei-browser "firefox"
  "The browser used by NEI when launch new tabs.")
(defvar nei-autoconnect t
  "Boolean that determines whether to try to autoconnect when a NEI buffer is open")

(defvar nei--ws-connection nil
  "The websocket client connection.")

(defvar nei--unexpected-disconnect nil
  "Flag indicating whether the websocket connection is closed or not")

(defvar nei--last-buffer "Internal variable to keep track of last nei buffer")

(defvar nei-verbose t
  "Whether to output status messages")

(defvar-local nei--execution-count 0
  "Local variable tracking the number of kernel executions invoked from NEI")

(defvar-local nei--ipynb-buffer-filename nil
  "Local variable tracking the original filename for a view on an IPYNB file")

(defvar-local nei--completions nil
  "Local variable (vector) tracking suggested completions at point")


(defun nei--open-websocket (ws-port)
  (progn
    (setq conn (websocket-open
                (format "ws://127.0.0.1:%s" ws-port)
                :on-message (lambda (_websocket frame)
                              (nei--receive-message (websocket-frame-text frame)))
                :on-close (lambda (_websocket) (setq nei--unexpected-disconnect t))
                ;; New connection, reset execution count.
                :on-open (lambda (_websocket) (websocket-send-text
                                   _websocket "{\"init\": \"editor\"}"))
		)
	  )
    (setq nei--ws-connection conn)
    )
  )

(defun nei--open-ws-connection (ws-port)
  "Opens a new websocket connection if needed"
  (if (or (null nei--ws-connection) nei--unexpected-disconnect)
      (progn
        (setq nei--unexpected-disconnect nil)
        (nei--open-websocket ws-port)
        )
    (nei--logging "Websocket connection already open")
    )
  )

(defun nei-connect (&optional ws-port html-port)
  "Start the NEI server, establish the websocket connection and begin mirroring"
  (interactive)
  (let ((ws-port-or-default   (or ws-port 9999))
        (html-port-or-default (or html-port 8000)))
    (if (and nei--ws-connection (null nei--unexpected-disconnect))
        (progn 
          (nei--logging "Already connected to NEI server")
          (nei-update-config)
          )
      (progn
        (if (nei--start-server ws-port-or-default html-port-or-default)
            (progn 
              (nei--open-ws-connection ws-port-or-default)
              (nei-update-config))
        )
      )
    )
    )
  )

(defun nei-disconnect ()
  "Close the websocket and shutdown the server"
  (interactive)
  (nei--close-ws-connection)
  (nei--server-stop)
  (remove-hook 'buffer-list-update-hook 'nei--buffer-switch-hook)
  )

(defun nei--close-ws-connection ()
  "Close the websocket connection."
  (websocket-close nei--ws-connection)
  (setq nei--ws-connection nil)
  )


(defun nei--disconnection-error ()
  (nei-stop-mirroring)
  (websocket-close nei--ws-connection)
  (nei--logging "Unexpected disconnection")
  (setq nei--unexpected-disconnect nil)
  (nei--close-ws-connection)
  )

;;========================;;
;; Sending data to server ;;
;;========================;;


(defun nei--send-data (text &optional warn-no-connection)
  "Runs the callback if there is a connection and handles unexpected disconnects."
  (cond (nei--unexpected-disconnect (nei--disconnection-error))
        ((null nei--ws-connection)
         (if warn-no-connection (nei--logging "Not connected to NEI server")))
        (t (progn
             (websocket-send-text nei--ws-connection text)
             (if nei--unexpected-disconnect (nei--disconnection-error)))
           )
        )
  )

(defun nei--send-json (obj &optional warn-no-connection)
    "JSON encode an object and send it over the websocket connection."
    (nei--send-data (json-encode obj) warn-no-connection)
    )

(defun nei--buffer-switch-hook ()
  (if (not (active-minibuffer-window))
      (if (bound-and-true-p nei-mode)
          (if (not (eq (buffer-name) nei--last-buffer))
              (if (eq (buffer-name) (buffer-name (elt (buffer-list) 0)))
                  (progn
                    ;; Timer used to ensure stack cleared to prevent recursion issues.
                    (run-with-timer 0 nil 'nei-reload-page)
                    (setq nei--last-buffer (buffer-name))
                    (push 'nei--scroll-hook window-scroll-functions)
                    )
                )

            )
          )
    )
  )

;;;###autoload
(define-minor-mode nei-mode
  "Nei for authoring notebooks in Emacs."
  :keymap nei-mode-map
  :lighter (:eval (format " NEI:%s %s"
                          (if nei--ws-connection "Connected" "Disconnected")
                          (if nei--active-kernel
                              (format "Kernel[%s]" nei--execution-count)
                            "[No Kernel]")
                          )
                  )
  (nei--register-things-at-point)
  (nei-fontify)
  ; Need one nice place to set up hooks
  (add-hook 'buffer-list-update-hook 'nei--buffer-switch-hook)

  (if (symbolp 'eldoc-mode)     ;; Disable eldoc mode! Why is is active?
      (eldoc-mode -1)
    )
  (if nei-autoconnect (nei-connect))
  (if (not nei--currently-mirroring)
      (nei-start-mirroring))
  (add-hook 'write-contents-functions 'nei--write-file-hook)

  (setq-local buffer-stale-function #'nei--buffer-stale-function)
  (setq-local revert-buffer-function #'nei--reverter-function)
  (auto-revert-mode t)
  (setq mode-require-final-newline nil) ;; Otherwise Python mode inserts newlines upon save
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
