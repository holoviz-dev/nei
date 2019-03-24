(require 'ert)
(require 'json)
(require 'websocket)
(require 'nei)

(defvar nei--ws-proxy-browser-connection nil
  "The websocket connection that impersonates a web browser.")

(defvar nei--test-response nil
  "Reponse captured by the proxy web browser proxy")



(defmacro with-temp-nei-buffer (&rest body)
  `(with-temp-buffer
     (progn
       (setq default-directory "/tmp") ;; Mystery error if in tests dir
       (setq nei-autoconnect nil)
       (setq python-indent-guess-indent-offset nil)
       (nei--proxy-browser)
       (should (equal (null nei--ws-proxy-browser-connection) nil))
       (nei-mode)
       (should (equal nei--currently-mirroring t))
       (progn ,@body)
       )
     )
  )

(defmacro assert-response-properties (cmd block);; &optional args)
  "Given the response JSON assert the command matches and optionally the args"
  `(progn
     (check-websocket-connections)
     (setq nei--test-response nil)

     (let ((i 0))
       ,block
       (while (and (null nei--test-response) (< i 350))
         (sleep-for 0 10)
         (setq i (+ i 1))
         )
       (should (equal (null nei--test-response) nil))
       (should (equal
                (assoc-value 'cmd (json-read-from-string nei--test-response)) ,cmd))
       )
     )
  )


(defun nei--proxy-browser ()
  (if (null nei--ws-proxy-browser-connection)
      (nei--connect-proxy-browser))
  )


(defun nei--connect-proxy-browser ()
  (progn
    (setq conn (websocket-open
                "ws://127.0.0.1:10000"
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


(defun check-websocket-connections ()
  (should (equal
           (websocket-openp nei--ws-proxy-browser-connection) '(open run)))
  (should (equal
           (websocket-openp nei--ws-connection) '(open run)))
  )

(ert-deftest test-server-port-available ()
  (should (equal (nei--server-available 8001) t))
  )


(ert-deftest test-update-css ()
  (nei-connect 10000 8010 t)
  (should (equal nil (null (get-process "nei-server"))))
  (sleep-for 0.2)

  (with-temp-nei-buffer
    (assert-response-properties "update_style" (nei-update-css))
    )
  )

(ert-deftest test-simple-insert-code-cell ()
  (nei-connect 10000 8010 t)
  (should (equal nil (null (get-process "nei-server"))))
  (sleep-for 0.2)

  (with-temp-nei-buffer
    (assert-response-properties "add_cell"
                                (progn 
                                  (insert "Text before any cell is not part of the notebook\n")
                                  (insert "# In[ ]\n\"But this is now a string in a code cell\"")
                                )
    )
    )
  )

(ert-deftest test-simple-insert-code-cell-and-execute ()
  (nei-connect 10000 8010 t)
  (should (equal nil (null (get-process "nei-server"))))
  (sleep-for 0.2)

  (with-temp-nei-buffer
    (assert-response-properties "add_cell"
                                (progn
                                  (insert "Text before any cell is not part of the notebook\n")
                                  (insert "# In[ ]\n\"But this is now a string in a code cell\"")
      )
    )
    (nei-start-kernel)
    (assert-response-properties "update_cell_input"
                                (progn
                                  (goto-char (point-max))
                                  (nei-exec-by-line)))
    )
  )
