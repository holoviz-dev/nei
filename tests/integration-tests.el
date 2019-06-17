(require 'ert)
(require 'json)
(require 'websocket)
(require 'nei)

(defvar nei--ws-proxy-browser-connection nil
  "The websocket connection that impersonates a web browser.")

(defvar nei--test-response nil
  "Reponse captured by the proxy web browser proxy")


(defvar nei--ws-port 10000)

(setq nei-verbose nil)


(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'nei-connect :around #'disable-y-or-n-p)

(defmacro with-temp-nei-buffer (&rest body)
  "Act in a temporary buffer suitable for testing NEI"
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


(defmacro wait-till-response (block)
  `(let ((i 0))
     ,block
     (while (and (null nei--test-response) (< i 350))
       (sleep-for 0 10)
       (setq i (+ i 1))
       )
     )
  )

(defmacro assert-response-properties (cmd block);; &optional args)
  "Given the response JSON assert the command matches and optionally the args"
  `(progn
     (check-websockets-open)
     (setq nei--test-response nil)
     (wait-till-response ,block)
     (should (equal (null nei--test-response) nil))
     (should (equal
              (assoc-value 'cmd (json-read-from-string nei--test-response)) ,cmd))
     )
  )

(defun query-server-info (level1 &optional level2 level3)
  "Get server state via the server-info command"
  (check-websockets-open)
  (setq nei--test-response nil)
  (wait-till-response (nei--query-server-info))
  (let* ((parsed-json (json-read-from-string nei--test-response))
         (cmd (assoc-value 'cmd parsed-json))
         (info (assoc-value 'args parsed-json)))
    (should (equal cmd "server_info"))
    (cond ((and level1 level2 level3)
           (assoc-value level3 (assoc-value level2 (assoc-value level1 info))))
          ((and level1 level2)
           (assoc-value level2 (assoc-value level1 info)))
           (t (assoc-value level1 info))
           )
    )
  )


(defun nei--proxy-browser ()
  "Connect an emacs websocket emulating the browser if required "
  (if (null nei--ws-proxy-browser-connection)
      (nei--connect-proxy-browser))
  )

(defun nei--connect-proxy-browser ()
  "Connect an emacs websocket emulating the browser"
  (progn
    (setq conn (websocket-open
                (format "ws://127.0.0.1:%d" nei--ws-port)
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


(defun check-websockets-open ()
  (should (equal
           (websocket-openp nei--ws-proxy-browser-connection) '(open run)))
  (should (equal
           (websocket-openp nei--ws-connection) '(open run)))
  )


;; ============== ;;
;; TEST FUNCTIONS ;;
;; ============== ;;

(ert-deftest test-server-port-available ()
  (should (equal (nei--server-available 8001) t))
  )


(ert-deftest test-update-theme ()
  (nei-connect nei--ws-port 8010)
  (should (equal nil (null (get-process "nei-server"))))
  (sleep-for 0.2)

  (with-temp-nei-buffer
    (assert-response-properties "update_theme" (nei-update-theme))
    )
  )

(ert-deftest test-simple-insert-code-cell ()
  (nei-connect nei--ws-port 8010)
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
  (nei-connect nei--ws-port 8010)
  (should (equal nil (null (get-process "nei-server"))))
  (sleep-for 0.2)

  (let ((prelude "Text before any cell is not part of the notebook\n")
        (code-cell-text "# In[ ]\n\"But this is now a string in a code cell\""))
    (with-temp-nei-buffer
     (assert-response-properties "add_cell"
                                 (progn (insert prelude) (insert code-cell-text))
                                 )
     
     (should (equal (query-server-info 'text) code-cell-text))
     (nei-start-kernel)
     (assert-response-properties "update_cell_input"
                                 (progn
                                   (goto-char (point-max))
                                   (nei-exec-by-line)))
     )
    )
  )


(ert-deftest test-simple-insert-mirroring ()
  (nei-connect nei--ws-port 8010)
  (should (equal nil (null (get-process "nei-server"))))
  (sleep-for 0.2)

  (with-temp-nei-buffer
   (let ((text "A simple text insertion to test mirroring"))
     (insert text)
     (should (equal (query-server-info 'last_dispatch 'cmd) "mirror"))
     (should (equal (query-server-info 'last_dispatch 'args 'start) 1))
     (should (equal (query-server-info 'last_dispatch 'args 'added) text))
     (should (equal (query-server-info 'text) "")) ;; As no cells in that one line
     )
   )
  )
