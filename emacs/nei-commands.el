;;;  -*- lexical-binding: t; -*-

;; Module for commands sent to and from the server
(require 'nei-util)
(require 'json)


(defvar-local nei--currently-mirroring nil)
(defvar-local nei--active-kernel nil)

(defvar-local nei-write-cleared-python-prompts t
  "Whether to keep clear numbers when saving python")
(defvar-local nei-write-notebook-output t
  "Whether to write output when saving ipynb")

(defvar nei-scroll-pixels 300)


(defun nei--server-cmd (command args &optional warn-no-connection)
  "Given a command string and its assoc list of args, return the JSON command object"
  (nei--send-json
   (list (cons "cmd" command) (cons "args" args) (cons "name" (buffer-name)))
   warn-no-connection)
  )


(defun nei--exec-cmd (command args &optional warn-no-connection)
  "Similar to nei-server-cmd but warns if there is no buffer kernel"
  (if nei--active-kernel
      (nei--server-cmd command args warn-no-connection)
    (nei--logging "Execution skipped. Start a kernel with %s"
             (mapconcat 'key-description (where-is-internal 'nei-start-kernel) " ")
             )
    )
  )


(defun nei--scroll-by (offset)
  "Send a scroll-by message"
   (nei--server-cmd "scroll_by" (list (cons "offset" offset)))
)


(defun nei--terminate-server ()
  "Used to terminate the server remotely- used for debugging"
  (nei--server-cmd "terminate" (list))
  )

(defun nei--query-server-info ()
  "Used to pass server info to the browser for testing"
  (nei--server-cmd "server_info" (list))
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


(defun nei-start-kernel-with (executable)
  (interactive "FPython executable: ")
  (nei-start-kernel executable)
  )

(defun nei-start-kernel (&optional executable)
  "Send an interrupt-kernel  message"
  (interactive)
  (setq nei--active-kernel t)
  (nei--server-cmd "start_kernel"
                   (list (cons "cwd" default-directory)
                         (cons "executable" executable))
                   )

  (run-with-idle-timer 0.3 nil 'nei-update-theme) ; E.g to update themes via Python
  (nei--logging "Sent start kernel message")
  (nei--update-kernel-menu-entry t)
)


(defun nei-interrupt-kernel ()
  "Send an interrupt-kernel  message"
  (interactive)
  (nei--server-cmd "interrupt_kernel" (list))
  (nei--logging "Sent interrupt kernel message")
)

(defun nei-restart-kernel ()
  "Send an restart-kernel  message"
  (interactive)
  (setq nei--execution-count 0)
  (nei--server-cmd "restart_kernel" (list))
  (nei--logging "Sent restart kernel message")
)

(defun nei-shutdown-kernel ()
  "Send an shutdown-kernel  message"
  (interactive)
  (setq nei--active-kernel nil)
  (nei--logging "Not implemented: shutdown-kernel")
  (nei--update-kernel-menu-entry nil)
  )


(defun nei--receive-message (text)
  "Callback for websocket messages"
  (let* ((parsed-json (json-read-from-string text))
         (cmd (assoc-value 'cmd parsed-json))
         (data (assoc-value 'data parsed-json)))

    (if (s-equals? cmd "completion")
        (setq nei--completions data))

    (if (s-equals? cmd "write_complete")
        (progn
          (with-current-buffer data
            (set-visited-file-modtime (nth 5 (file-attributes nei--ipynb-buffer-filename)))
            (message "Wrote %s" nei--ipynb-buffer-filename)
            )
          )
      )

    (if (s-equals? cmd "user_message")
        (message data)
        )
    
    (if (s-equals? cmd "load_validated")
        (if (eq (assoc-value 'valid data) ':json-false)
          (progn 
            (message "Notebook saving disabled: failed to validate %s. Please report this notebook as a GitHub issue."  (assoc-value 'filename data))
            (setq nei-ipynb-save-enabled nil) 
            )
          )
        )
    
    (if (s-equals? cmd "mirroring_error")
        (nei--server-cmd "mirroring_error"
                         (list (cons "editor_text" (buffer-string))
                               (cons "mirror_text" data)))
      )
    )
  )

(defun nei-complete ()
  "Wait for nei--completions variable to be set after a completion request"
  (let ((line-context
         (buffer-substring-no-properties (line-beginning-position) (point))))
    (if (not (s-equals? line-context ""))
        (progn
          (nei--server-cmd "complete"
                           (list (cons "line_number" (line-number-at-pos))
                                 (cons "line_context" line-context)
                                 (cons "position" (point)))
                           )
          (let ((result nil) (i 0))
            (while (and (null nei--completions) nei--active-kernel (< i 150))
              (sleep-for 0 10)
              (setq i (+ i 1))
              )
            (setq result nei--completions)
            (setq nei--completions nil)
            result
            )
        )
      )
    )
  )

(defun nei-completion-at-point ()
  (let* ((result (nei-complete))
         (cursor-start (assoc-value 'cursor_start result))
         (cursor-end (assoc-value 'cursor_end result))
         (matches (assoc-value 'matches result)))
    (list cursor-start cursor-end (append matches nil))
    )
  )

(defun nei-reload-page ()
  "Send an restart-kernel  message"
  (interactive)
  (nei--server-cmd "reload_page" (list))
)


(defun nei-clear-all-cell-outputs ()
  "Send a clear_all_cell_outputs message to server"
  (interactive)
  (nei--server-cmd "clear_all_cell_outputs" (list))
  (nei--clear-execution-prompts)
  (nei--logging "Cleared all cell outputs and prompts")
)


(defun nei-clear-notebook-and-restart ()
  "Send a clear_notebook message to server followed by a restart_kernel message"
  (interactive)
  (nei--server-cmd "clear_notebook" (list))
  (nei-restart-kernel)
  (erase-buffer)
  (nei--logging "Cleared notebook and restarted kernel")
)

(defun nei-view-notebook ()
  "View nbconverted notebook in the browser"
  (interactive)
  (nei--server-cmd "view_notebook" (list))
  (nei--logging "Sent interrupt kernel message")
  )


(defun nei-exec-silently (code)
  "Send an 'exec_silently' message to server to run the given code for its side-effects"
  (interactive "MCode:")
  (nei--exec-cmd "exec_silently" (list (cons "code" code)))
  )


(defun nei-exec-by-line ()
  "Send an 'exec_cell_by_line' message to server at the current line"
  (interactive)
  (if nei--active-kernel
      (progn
        (setq nei--execution-count (1+ nei--execution-count))
        (nei--update-exec-prompt nei--execution-count) ;; TODO: Bump only if in code cell
        )
    )
  (nei--exec-cmd "exec_cell_by_line"
                 (list
                  (cons "line_number"
                        (line-number-at-pos))
                  )
                 )
  )


(defun nei--scroll-hook (win start-pos)
  "Hook to update scroll position in client via window-scroll-functions"
  (let ((buffer-mode (with-current-buffer
                         (window-buffer (selected-window)) major-mode)))
    (if (eq buffer-mode 'python-mode) ;; TODO: Needs a better check
        (nei--server-cmd "scroll_to_line"
                         (list
                          (cons "line"
                                (line-number-at-pos (window-start))
                                )
                          )
                         )
      )
    )
  )


(defun nei-exec-by-line-and-move-to-next-cell ()
  "Executes cell at current line and moves point to next cell"
  (interactive)
  (nei-exec-by-line)
  (if nei--active-kernel
      (nei-move-point-to-next-cell) ;; TODO - move into markdown cells too
    )
)


(defun nei-clear-cell-by-line ()
  (interactive)
  (nei--server-cmd "clear_cell_output_by_line"
                   (list
                    (cons "line_number"
                          (line-number-at-pos))
                    )
                   )

  )


(defun nei-toggle-display-code ()
  (interactive)
  (nei--server-cmd "display_code"
                   (list
                    (cons "line_number" (line-number-at-pos))
                    (cons "visible" "toggle")
                    ) t)
  )

(defun nei-toggle-display-all-code ()
  (interactive)
  (nei--server-cmd "display_all_code"
                   (list
                    (cons "visible" "toggle")
                    ) t)
  )
  
(defun nei-update-theme ()
  "Using htmlize update CSS used for syntax highlighting by highlight.js"
  (interactive)
  (nei--server-cmd "update_theme"
                   (list
                    (cons "css" (nei--htmlize-css))
                    ) t)
  )


(defun nei-update-config ()
  "Set the config dictionary on the notebook"
  (interactive)
  (nei--server-cmd "update_config"
                   (list
                    (cons "config"
                          (list (cons 'browser nei-browser))
                          )) t)
  )


(defun nei-view-browser ()
  "Open a browser tab to view the output"
  (interactive)
   (progn
     (nei--server-cmd "view_browser" (list) t)
     (run-with-idle-timer 1 nil 'nei-update-theme))
   )

;;==============;;
;; IO commands ;;
;;==============;;



;; Note the mirror buffer isn't the same as output using .text....
(defun nei-write-notebook (mode)
  "Interactive command that prompts for the mode and filename for writing the notebook"
  (interactive (list (completing-read
                      "Select an output type: "
                      '(("cleared" "cleared")
                        ("full-notebook" "full-notebook"))
                      nil t "")))
  (defun nei--prompt-for-filename (filename)
    (interactive "FNotebook: ")
    filename
    )
  (let ((filename (call-interactively 'nei--prompt-for-filename)))
    (nei--write-notebook mode filename)
    )
  )

(defun nei-export-to-html (filename)
  (interactive "FExport to HTML: ")
  (nei--write-notebook "html" filename)
  )


(defun nei--write-notebook (mode filename)
  "Mode can be one of \"python\", \"cleared\" or \"full-notebook\" "
  (nei--server-cmd "write_notebook"
                   (list
                    (cons "mode" mode)
                    (cons "filename" filename)
                    )
                   )
  )


(defun nei--load-from-file (cells filename buffer-text)
  "Send a load_from_file message to server with .ipynb parsed cells and filename"
  (nei--server-cmd "load_from_file"
                   (list
                    (cons "json_string"
                          (json-encode cells))
                    (cons "filename" (expand-file-name filename))
                    (cons "buffer_text" buffer-text)
                    )
                   )
  )


(defun nei-open-notebook (filename)
  "Prompt for filename, load it into a new python-mode buffer and start mirroring"
  (interactive "FFind notebook: ")
  (find-file filename)
  (nei-view-ipynb)
)


(defun nei--insert-notebook-command (filename text line-number)
  (nei--server-cmd "insert_file_at_point"
                   (list
                    (cons "filename" (expand-file-name filename))
                    (cons "text" text)
                    (cons "line_number" line-number)
                    )
                   )
  )

(defun nei-insert-notebook (filename)
  "Prompt for filename and insert it into the buffer"
  (interactive "FFind notebook: ")
  (message "WARNING: nei-insert-notebook function needs updating")
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
      (nei--server-cmd "mirror"
                       (list
                        (cons "start" start)
                        (cons "end" end)
                        (cons "length" length)
                        (cons "added" (buffer-substring start end))
                        (cons "size" (buffer-size src))
                        (cons "md5"  (secure-hash 'md5 (buffer-string)))
                        )
                       )
      )
    )
  )


(defun nei-start-mirroring ()
  (interactive)
  (let ((text (buffer-substring (point-min)  (point-max))))
    (setq nei--currently-mirroring t)
    (nei--server-cmd "start_mirror"
                     (list
                      (cons "text"  text)
                      )
                     )
    )
  (add-hook 'after-change-functions #'nei--mirror nil t)
  (add-hook 'post-command-hook 'nei--point-move-disable-highlight-hook)
  (run-with-idle-timer 0.2 t 'nei--update-highlight-cell)
)


(defun nei-stop-mirroring ()
  (interactive)
  (setq nei--currently-mirroring nil)
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
  (nei--server-cmd "hold_mode"
                   (list
                    (cons "mode"  mode)
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
      (insert "\n\"\"\" #:md:\n")
      (goto-char pos)
      )
    )
  )

(provide 'nei-commands)
