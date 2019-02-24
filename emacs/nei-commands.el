(require 'nei-util)
(require 'json)


(defvar nei--currently-mirroring nil)
;; Module for commands sent to and from the server

(defvar nei-scroll-pixels 300)


(defun nei--server-cmd (command args)
  "Given a command string and its assoc list of args, return the JSON command object"
   (list (cons "cmd" command) (cons "args" args) (cons "name" (buffer-name)))
  )

(defun nei--scroll-by (offset)
  "Send a scroll-by message"
  (nei--send-json (nei--server-cmd "scroll_by" (list (cons "offset" offset))))
)




(defun nei--terminate-server ()
  "Used to terminate the server remotely- used for debugging"
  (nei--send-json (nei--server-cmd "terminate" (list)))
  )




;;======================;;
;; Interactive commands ;;
;;======================;;

(defun nei-scroll-up ()
  (interactive)
  (nei--scroll-by (- nei-scroll-pixels))
  )

(defun nei-scroll-down ()
  (interactive)
  (nei--scroll-by nei-scroll-pixels)
  )


(defun nei-interrupt-kernel ()
  "Send an interrupt-kernel  message"
  (interactive)  
  (nei--send-json (nei--server-cmd "interrupt_kernel" (list)))
  (message "Sent interrupt kernel message")
)

(defun nei-restart-kernel ()
  "Send an restart-kernel  message"
  (interactive)
  (setq nei--execution-count 0)
  (nei--send-json (nei--server-cmd "restart_kernel" (list)))
  (message "Sent restart kernel message")
)


(defun nei-clear-all-cell-outputs ()
  "Send a clear_all_cell_outputs message to server"
  (interactive)
  (nei--send-json (nei--server-cmd "clear_all_cell_outputs" (list)))
  (message "Cleared all cell outputs")
)


(defun nei-clear-notebook-and-restart ()
  "Send a clear_notebook message to server followed by a restart_kernel message"
  (interactive)
  (nei--send-json (nei--server-cmd "clear_notebook" (list)))
  (nei-restart-kernel)
  (erase-buffer)
  (message "Cleared notebook and restarted kernel")
)

(defun nei-view-notebook ()
  "View nbconverted notebook in the browser"
  (interactive)
  (nei--send-json (nei--server-cmd "view_notebook" (list)))
  (message "Sent interrupt kernel message")
  )


(defun nei-exec-silently (code)
  "Send an 'exec_silently' message to server to run the given code for its side-effects"
  (interactive "MCode:")
  (nei--send-json (nei--server-cmd "exec_silently" (list (cons "code" code))))
  )

  
(defun nei-exec-by-line ()
  "Send an 'exec_cell_by_line' message to server at the current line"
  (interactive)
  (setq nei--execution-count (1+ nei--execution-count))
  (nei--update-exec-prompt nei--execution-count) ;; TODO: Bump only if in code cell
  (nei--send-json (nei--server-cmd "exec_cell_by_line"
                                           (list 
                                              (cons "line_number"
                                                    (line-number-at-pos))
                                              )
                                           )
                      )
  )


(defun nei--scroll-hook (win start-pos)
  "Hook to update scroll position in client via window-scroll-functions"
  (let ((buffer-mode (with-current-buffer 
                         (window-buffer (selected-window)) major-mode)))
    (if (eq buffer-mode 'python-mode) ;; TODO: Needs a better check
        (nei--send-json (nei--server-cmd "scroll_to_line"
                                                 (list 
                                                  (cons "line"
                                                        (line-number-at-pos (window-start))
                                                        )
                                                  )
                                                 )
                            )
      )
    )
)

(defun nei--set-scroll-hook ()
  (push 'nei--scroll-hook window-scroll-functions) 
  )


(defun nei-exec-by-line-and-move-to-next-cell ()
  "Executes cell at current line and moves point to next cell"
  (interactive)
  (nei-exec-by-line)
  (nei-move-point-to-next-cell) ;; TODO - move into markdown cells too
)


(defun nei-clear-cell-by-line ()
  (interactive)
  (nei--send-json (nei--server-cmd "clear_cell_output_by_line"
                                   (list 
                                    (cons "line_number"
                                          (line-number-at-pos))
                                    )
                                   )
                  )
  )


(defun nei-update-css ()
  "Using htmlize update CSS used for syntax highlighting by highlight.js"
  (interactive)
  (nei--send-json (nei--server-cmd "update_style"
                                   (list 
                                    (cons "css" (nei--htmlize-css))
                                    )) t)
  )
  

(defun nei-update-config ()
  "Set the config dictionary on the notebook"
  (interactive)
  (nei--send-json
   (nei--server-cmd "update_config"
                    (list 
                     (cons "config"
                           (list (cons 'browser nei-browser))
                           ))) t)
  )


(defun nei-view-browser ()
  "Open a browser tab to view the output"
  (interactive)
   (progn
     (nei--send-json (nei--server-cmd "view_browser" (list)) t)
     (sleep-for 5) ;; Let the browser open and the page load
     (nei-update-css))
  )

;;==============;;
;; IO commands ;;
;;==============;;



;; Note the mirror buffer isn't the same as output using .text....
(defun nei-write-notebook (mode)
  (interactive (list (completing-read
                      "Select an output type: "
                      '(("python" "python")
                        ("cleared" "cleared")
                        ("full-notebook" "full-notebook")
                        ("html" "html"))
                      nil t "")))
  (defun nei--prompt-for-filename (filename)
    (interactive "FNotebook:")
    filename
    )
  (let ((filename (call-interactively 'nei--prompt-for-filename)))
    
    (nei--send-json (nei--server-cmd "write_notebook"
                                             (list 
                                              (cons "mode" mode)
                                              (cons "filename" filename)
                                              )
                                             )
                        )
    )
  )


(defun nei--load-from-file (cells filename)
  "Send a load_from_file message to server with .ipynb parsed cells and filename"
  
  (nei--send-json (nei--server-cmd "load_from_file"
                                           (list 
                                            (cons "json_string"
                                                  (json-encode cells))
                                            (cons "filename" (expand-file-name filename))
                                            )
                                           )
                      )
  )


(defun nei-load-notebook (filename)
  "Prompt for filename, load it into a new python-mode buffer and start mirroring" 
  (interactive "FNotebook filename:")
  (setq nei--cells
        (nei-parse-notebook-file filename))
  (let ((buffer-name 
         (s-concat (s-chop-suffix ".ipynb" 
                                  (file-name-nondirectory filename)) ".py")))
    (generate-new-buffer buffer-name)
    (switch-to-buffer buffer-name)
    (python-mode)
    (insert (nei--cells-to-text nei--cells))
    )
  (nei--load-from-file nei--cells filename)
  (nei-start-mirroring)
)


(defun nei--insert-notebook-command (filename text line-number)
  (nei--send-json (nei--server-cmd "insert_file_at_point"
                                           (list 
                                            (cons "filename" (expand-file-name filename))
                                            (cons "text" text)
                                            (cons "line_number" line-number)
                                            )
                                           )
                      )
  

  )
  
(defun nei-insert-notebook (filename)
  "Prompt for filename and insert it into the buffer" 
  (interactive "FNotebook filename:")
  (nei--hold-mode "on")
  (let* ((cells (nei-parse-notebook-file filename))
         (text (nei--cells-to-text cells))
         (line-number (line-number-at-pos (point)))
         )
    (insert text)
    (nei--insert-notebook-command filename text line-number)
    )
  (nei--hold-mode "off")
  )

;;===========;;
;; MIRRORING ;;
;;===========;;

;; Can try to call nei--update-highlight-cell in the post-command
;; nei-mirror hook (within save-match-data) but it does not seem to be
;; worth the slow down.

(defun nei--mirror (start end length)
  (let* ((src (current-buffer)))
    (with-current-buffer src
      (nei--send-json (nei--server-cmd "mirror"
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


(defun nei-start-mirroring ()
  (interactive)
  (let ((text (buffer-substring (point-min)  (point-max))))
    (nei--send-json (nei--server-cmd "start_mirror"
                                             (list 
                                              (cons "text"  text)
                                              )
                                             )
                          )
    )
  (add-hook 'after-change-functions #'nei--mirror nil t)
  (add-hook 'post-command-hook 'nei--point-move-disable-highlight-hook)
  (run-with-idle-timer 0.2 t 'nei--update-highlight-cell)
)


(defun nei-stop-mirroring ()
  (interactive)
  (remove-hook 'after-change-functions #'nei--mirror t)
  (remove-hook 'post-command-hook 'nei--point-move-disable-highlight-hook)
  (cancel-function-timers 'nei--update-highlight-cell)
  (nei-defontify)
  )

(defun nei-toggle-mirroring ()
  (interactive)
  (if nei--currently-mirroring (nei-stop-mirroring) (nei-start-mirroring))
  (setq nei--currently-mirroring (not nei--currently-mirroring))
  )

(defun nei--hold-mode (mode)
  "Toggle the 'hold' state of the mirror"
  (nei--send-json (nei--server-cmd "hold_mode"
                                           (list 
                                            (cons "mode"  mode)
                                            )
                                           )
                      )
  )

;;========================;;
;; Emacs editing commands ;;
;;========================;;

(defun nei-insert-code-cell ()
  "Add a new code cell prompt"
  (interactive)
  (if (eq (point) (nei--start-of-line (point)))    
      (insert "# In[ ]\n") ;; Already at the start of a line
    (progn
      (goto-char (nei--end-of-line (point)))
      (insert "\n\n# In[ ]\n"))
    )
)

(defun nei-insert-markdown-cell ()
  "Add a new markdown cell prompt"
  (interactive)
  (let ((head (if (eq (point) (nei--start-of-line (point)))
                  "\n\"\"\"\n" "\n\n\"\"\"\n")))
    (insert head)
    (let ((pos (point)))
      (insert "\n\"\"\"\n")
      (goto-char pos)
      )
    )
  )

(provide 'nei-commands)
