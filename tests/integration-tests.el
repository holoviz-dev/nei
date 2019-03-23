(require 'ert)
(require 'json)
(require 'websocket)
(require 'nei)

(defvar nei--ws-proxy-browser-connection nil
  "The websocket connection that impersonates a web browser.")

(defvar nei--test-response nil
  "Reponse captured by the proxy web browser proxy")


(defun nei--proxy-browser ()
  (progn
    (setq conn (websocket-open
                "ws://127.0.0.1:8001"
                :on-message (lambda (_websocket frame)
                              (setq nei--test-response  (websocket-frame-text frame)))
                :on-close (lambda (_websocket) (message "ON CLOSE"))
                :on-open (lambda (_websocket) (websocket-send-text
                                   _websocket "{\"init\": \"browser\"}"))
		)
	  )
    (setq nei--ws-proxy-browser-connection conn)
    (sleep-for  0.1)
    )
  )


(defun assert-response-properties (cmd &optional args)
  "Given the response JSON assert the command matches and optionally the args"

  (let
      ((result (wait-for-response)))
    (should (equal (null result) nil)) 
    (should (equal (assoc-value 'cmd result) cmd))
    )
  )


(defun wait-for-response ()
  (let ((result nil) (i 0))
    (while (and (null nei--test-response) (< i 150))
      (sleep-for 0 10)
      (setq i (+ i 1))
      )
    (setq result nei--test-response)
    (setq nei--test-response nil)
    (if (null result) result (json-read-from-string result)) 
    )
  )


(ert-deftest test-server-port-available ()
  (should (equal (nei--server-available 8001) t))
  )


;; (ert-deftest test-update-css ()
;;   (nei-connect 10000 8010 t)
;;   (should (equal nil (null (get-process "nei-server"))))
;;   (sleep-for 0.2)

;;   (with-temp-buffer
;;     (setq nei-autoconnect nil)
;;     (setq python-indent-guess-indent-offset nil)
;;     (nei--proxy-browser)
;;     (should (equal (null nei--ws-proxy-browser-connection) nil))
;;     (nei-mode)
;;     (should (equal nei--currently-mirroring t))
;;     (nei-update-css)
;;     (assert-response-properties "update_style")
;;     )
;;   )
