(require 'lab-util)
(require 'json)


(defvar labmode--currently-mirroring nil)
;; Module for commands sent to and from the server

(defun labmode--server-cmd (command args)
  "Given a command string and its assoc list of args, return the JSON command object"
   (list (cons "cmd" command) (cons "args" args))
  )

;;======================;;
;; Interactive commands ;;
;;======================;;


(defun labmode-interrupt-kernel ()
  "Send an interrupt-kernel  message"
  (interactive)
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "interrupt_kernel" (list)))
  (message "Sent interrupt kernel message")
)

(defun labmode-restart-kernel ()
  "Send an restart-kernel  message"
  (interactive)
  (labmode--wait-connection)
  (setq labmode--execution-count 0)
  (labmode--send-json (labmode--server-cmd "restart_kernel" (list)))
  (message "Sent restart kernel message")
)


(defun labmode-clear-all-cell-outputs ()
  "Send a clear_all_cell_outputs message to server"
  (interactive)
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "clear_all_cell_outputs" (list)))
  (message "Cleared all cell outputs")
)


(defun labmode-clear-notebook-and-restart ()
  "Send a clear_notebook message to server followed by a restart_kernel message"
  (interactive)
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "clear_notebook" (list)))
  (labmode-restart-kernel)
  (erase-buffer)
  (message "Cleared notebook and restarted kernel")
)

(defun labmode-view-notebook ()
  "View nbconverted notebook in the browser"
  (interactive)
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "view_notebook" (list)))
  (message "Sent interrupt kernel message")
  )


(defun labmode-exec-silently (code)
  "Send an 'exec_silently' message to server to run the given code for its side-effects"
  (interactive "MCode:")
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "exec_silently" (list (cons "code" code))))
  )

  
(defun labmode-exec-by-line ()
  "Send an 'exec_cell_by_line' message to server at the current line"
  (interactive)
  (labmode--wait-connection)
  (setq labmode--execution-count (1+ labmode--execution-count))
  (labmode--update-exec-prompt labmode--execution-count) ;; TODO: Bump only if in code cell
  (labmode--send-json (labmode--server-cmd "exec_cell_by_line"
                                           (list 
                                              (cons "line_number"
                                                    (line-number-at-pos))
                                              )
                                           )
                      )
  )

(defun labmode-exec-by-line-and-move-to-next-cell ()
  "Executes cell at current line and moves point to next cell"
  (interactive)
  (labmode-exec-by-line)
  (labmode-move-point-to-next-cell) ;; TODO - move into markdown cells too
)


(defun labmode-clear-cell-by-line ()
  (interactive)
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "clear_cell_output_by_line"
                                           (list 
                                              (cons "line_number"
                                                    (line-number-at-pos))
                                              )
                                           )
                      )
  )

(defun labmode-update-css ()
  "Using htmlize update CSS used for syntax highlighting by highlight.js"
  (interactive)
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "update_style"
                                           (list 
                                            (cons "css" (labmode--htmlize-css))
                                            )
                                           )
                      )
)




(defun labmode-update-config ()
  "Set the config dictionary on the notebook"
  (interactive)
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "update_config"
                                           (list 
                                            (cons "config"
                                                  (list (cons 'browser labmode-browser))
                                                  )
                                            )
                                           )
                      )
)



(defun labmode-view-browser ()
  "Open a browser tab to view the output"
  (interactive)
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "view_browser" (list)))
  (sleep-for 2) ;; Let the page load
  (labmode-update-css)
  )

;;==============;;
;; IO commands ;;
;;==============;;



;; Note the mirror buffer isn't the same as output using .text....
(defun labmode-write-notebook (mode)
  (interactive (list (completing-read
                      "Select an output type: "
                      '(("python" "python")
                        ("cleared" "cleared")
                        ("full-notebook" "full-notebook")
                        ("html" "html"))
                      nil t "")))
  (defun labmode--prompt-for-filename (filename)
    (interactive "FNotebook:")
    filename
    )
  (let ((filename (call-interactively 'labmode--prompt-for-filename)))
    (labmode--wait-connection)
    (labmode--send-json (labmode--server-cmd "write_notebook"
                                             (list 
                                              (cons "mode" mode)
                                              (cons "filename" filename)
                                              )
                                             )
                        )
    )
  )


(defun labmode--load-from-file (cells filename)
  "Send a load_from_file message to server with .ipynb parsed cells and filename"
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "load_from_file"
                                           (list 
                                            (cons "json_string"
                                                  (json-encode cells))
                                            (cons "filename" (expand-file-name filename))
                                            )
                                           )
                      )
  )


(defun labmode-load-notebook (filename)
  "Prompt for filename, load it into a new python-modee buffer and start mirroring" 
  (interactive "FNotebook filename:")
  (setq labmode--cells
        (labmode-parse-notebook-file filename))
  (let ((buffer-name 
         (s-concat (s-chop-suffix ".ipynb" 
                                  (file-name-nondirectory filename)) ".py")))
    (generate-new-buffer buffer-name)
    (switch-to-buffer buffer-name)
    (python-mode)
    (insert (labmode--cells-to-text labmode--cells))
    )
  (labmode--load-from-file labmode--cells filename)
  (labmode-start-mirroring)
)


(defun labmode--insert-notebook-command (filename text line-number)
  (labmode--wait-connection)
  (labmode--send-json (labmode--server-cmd "insert_file_at_point"
                                           (list 
                                            (cons "filename" (expand-file-name filename))
                                            (cons "text" text)
                                            (cons "line_number" line-number)
                                            )
                                           )
                      )
  

  )
  
(defun labmode-insert-notebook (filename)
  "Prompt for filename and insert it into the buffer" 
  (interactive "FNotebook filename:")
  (labmode--hold-mode "on")
  (let* ((cells (labmode-parse-notebook-file filename))
         (text (labmode--cells-to-text cells))
         (line-number (line-number-at-pos (point)))
         )
    (insert text)
    (labmode--insert-notebook-command filename text line-number)
    )
  (labmode--hold-mode "off")
  )

;;===========;;
;; MIRRORING ;;
;;===========;;

;; Can try to call labmode--update-highlight-cell in the post-command
;; labmode-mirror hook (within save-match-data) but it does not seem to be
;; worth the slow down.

(defun labmode--mirror (start end length)
  (let* ((src (current-buffer)))
    (with-current-buffer src
      (labmode--send-json (labmode--server-cmd "mirror"
                                               (list 
                                                (cons "start" start)
                                                (cons "end" end)
                                                (cons "length" length)
                                                (cons "added" (buffer-substring start end))
                                                (cons "size" (buffer-size src))
                                                )
                                               )
                        )
      )
    )  
  )


(defun labmode-start-mirroring ()
  (interactive)
  (labmode--wait-connection)
  (let ((text (buffer-substring (point-min)  (point-max))))
    (labmode--send-json (labmode--server-cmd "start_mirror"
                                             (list 
                                              (cons "text"  text)
                                              )
                                             )
                          )
    )
  (add-hook 'after-change-functions #'labmode--mirror nil t)
  (add-hook 'post-command-hook 'labmode--point-move-disable-highlight-hook)
  (run-with-idle-timer 0.2 t 'labmode--update-highlight-cell)
)


(defun labmode-stop-mirroring ()
  (interactive)
  (remove-hook 'after-change-functions #'labmode--mirror t)
  (remove-hook 'post-command-hook 'labmode--point-move-disable-highlight-hook)
  (cancel-function-timers 'labmode--update-highlight-cell)
  (labmode-defontify)
  )

(defun labmode-toggle-mirroring ()
  (interactive)
  (if labmode--currently-mirroring (labmode-stop-mirroring) (labmode-start-mirroring))
  (setq labmode--currently-mirroring (not labmode--currently-mirroring))
  )

(defun labmode--hold-mode (mode)
  "Toggle the 'hold' state of the mirror"
  (labmode--send-json (labmode--server-cmd "hold_mode"
                                           (list 
                                            (cons "mode"  mode)
                                            )
                                           )
                      )
  )

;;========================;;
;; Emacs editing commands ;;
;;========================;;

(defun labmode-insert-code-cell ()
  "Add a new code cell prompt"
  (interactive)
  (if (eq (point) (labmode--start-of-line (point)))    
      (insert "# In[ ]\n") ;; Already at the start of a line
    (progn
      (goto-char (labmode--end-of-line (point)))
      (insert "\n\n# In[ ]\n"))
    )
)

(defun labmode-insert-markdown-cell ()
  "Add a new markdown cell prompt"
  (interactive)
  (let ((head (if (eq (point) (labmode--start-of-line (point)))
                  "\n\"\"\"\n" "\n\n\"\"\"\n")))
    (insert head)
    (let ((pos (point)))
      (insert "\n\"\"\"\n")
      (goto-char pos)
      )
    )
  )

(provide 'lab-commands)
