;;;  -*- lexical-binding: t; -*-

;; For starting up the server
(require 's)

;; ========= ;;
;; Utilities ;;
;; ========= ;;

(defun nei--cmd-stdout (cmd)
  "Run a shell command with shell-command-to-string and trim"
  (s-trim-right (shell-command-to-string cmd)))


(defun nei--get-exit-code (program &rest args) ;
  "Run PROGRAM with ARGS and return the exit code."
  (with-temp-buffer
    (apply 'call-process program nil (current-buffer) nil args)))

;; ===================================== ;;
;; Querying and managing the environment ;;
;; ===================================== ;;


(defun nei--server-python-path ()
  "Get the path associated with the current 'python' command"
  (nei--cmd-stdout "python -c 'import sys;print(sys.executable)'")
  )

(defun nei--server-available (&optional port)
  "Returns t if NEI available in Python, the string 'port
  unavailable' if the port is unavailable, otherwise nil if nei
  is unavailable"
  (if (eq (nei--get-exit-code "python") 0)
      (let* ((port (or port 9999))
               (cmd (format "python -c 'import nei;nei.server_status(%s)'" port))
               (stdout (nei--cmd-stdout cmd)))
        (cond
         ((s-equals? stdout "port unavailable") stdout)
         ((s-equals? stdout "") t)
         (t nil) ; Output will be an import error
         )
        )
    )
  )


(defun nei-server-pip-install ()
  "Installs NEI with pip in the current Python environment"
  (interactive)
  (if (yes-or-no-p (format "Pip install NEI in %s?"  (nei--server-python-path)))
      (let* ((template "import sys, subprocess;subprocess.call(%s)")
             (arglist "[sys.executable, '-m', 'pip', 'install', 'nei']")
             (cmd (format template arglist))
         )
        (start-process "nei-server-pip-install"
                       " *nei pip install log*" ;; Leading space hides the buffer
                       "python" "-c" cmd)
        (switch-to-buffer " *nei pip install log*")
        )
    )
  )

;; ==================== ;;
;; Launching the server ;;
;; ==================== ;;


(defun nei--server-process-sentinel (process event)
  "Sentinel to show the server log if it exits unexpectedly"
  (if (s-starts-with? "exited abnormally with code" event)
      (nei-server-log t))
  )

(defun nei--server-insertion-filter (proc string)
  "Process filter to monitor the server process for errors"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)                    
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))
      ;; Only non-standard part of the process filter - show buffer if
      ;; it contains the string 'ERROR'
      (if (s-contains? "ERROR" (buffer-string))
          (nei-server-log))
      )))


(defun nei--server-launch-process ()
  "Starts the nei server as an emacs process if not already running"
  (interactive)
  (let ((proc (get-process "nei-server")))
    (if (null proc)
        (progn
          (message "Starting nei server")
          (let ((new-proc
                 (start-process "nei-server"
                                " *nei server log*" ;; Leading space hides the buffer
                                "python" "-c" "import nei;nei.serve()")))

            (set-process-filter new-proc 'nei--server-insertion-filter) 
            (set-process-query-on-exit-flag new-proc nil)
            (set-process-sentinel new-proc 'nei--server-process-sentinel)             
            (sleep-for 2)
            (message "Started")
            )
          )
      (message "Nei server already running")
      )
    )
  )


(defun nei--start-server (&optional port) ;; Make port mandatory here?
  "Check if NEI is importable in Python after optionally activating a
   conda environment (if conda-mode available). If the check fails and
   it is not due to a port conflict, open a help window with information
   to help diagnose and fix the problem."

  (let ((status (nei--server-available (or port 9999))))
    (cond ((null status) (nei--server-diagnose))
          ((and (s-equals? status "port unavailable")
           (y-or-n-p (format "Port %s unavailable. Attempt external server connection?"
                             (or port 9999)))) (message "Connecting to external server..."))
          ( t (progn
                (message "Launching NEI server.")
                (nei--server-launch-process)
                )
              )
          )
    )
  )

;;=============================;;
;; Managing the server process ;;
;;=============================;;

(defun nei--server-stop ()
  "Kills the nei server if it is currently running"
  (interactive)
  (let ((proc (get-process "nei-server")))

    (if (not (null proc))
        (let ((proc-buffer  (process-buffer proc)))
          (kill-process proc)
          (sleep-for 0 500)
          (kill-buffer proc-buffer)
          )
      )
    )
  )


(defun nei-server-log (&optional terminated)
  "View the server log buffer if the server process is running."
  (interactive)
  (let ((proc (get-process "nei-server")))
    (if (or (not (null proc)) terminated)
        (progn 
          (if (null (get-buffer "NEI server log"))
              (progn 
                (make-indirect-buffer (get-buffer " *nei server log*") "NEI server log")
                (with-current-buffer (get-buffer "NEI server log")
                  (special-mode)
                  )
                )
            )
          (switch-to-buffer-other-window "NEI server log")
          ) 
      (message "NEI server not currently running as emacs process")
      )
    )
  )


;; ================ ;;
;; Help information ;;
;; ================ ;;


(defvar nei--server-info-msg
      "NEI is available in the current Python environment:

NEI version:      %s
NEI install path: %s
Python path:      %s

If you are using conda.el, you may change your current Python
environment using the conda-env-activate command.
")


(defvar nei--server-diagnose-msg
      "Could not import nei in the following Python environment:

%s

This issue can be addressed as follows:

1. Manually run 'pip install nei' to make the nei package importable by
the Python executable above.

2. If the python path above is inappropriate and you are setting your
default Python in your shell (e.g via .bash_profile) you can install the
exec-path-from-shell package from MELPA. Then by running
(exec-path-from-shell-initialize) in your emacs session (e.g your
.emacs) you can make this default Python environment available to
NEI. Commonly applicable on MacOS.

3. If you are using either Anaconda distribution or Miniconda, you can
install conda-mode from MELPA. You can then use the conda-env-activate
function to select an appropriate environment before running the NEI
server (e.g using a file local variable).

If you are happy to install NEI in the Python environment listed above,
you can now run the nei-server-pip-install command.
")


(defun nei--server-diagnose ()
  "Present a buffer with help information if the server does not start"
  (with-output-to-temp-buffer "NEI server configuration"
    (let ((executable
           (if (null (nei--get-exit-code "python"))
               "Python executable not found"
             (nei--server-python-path))))
      (princ (format  nei--server-diagnose-msg executable))
      )
    )
  )

(defun nei-server-info ()
  (interactive)
  (let ((status (nei--server-available)))
    (if (null status)
        (nei--server-diagnose)
      (let ((py-path (nei--server-python-path))
            (nei-path (nei--cmd-stdout "python -c 'import nei;print(nei.__file__)'"))
            (nei-version (nei--cmd-stdout "python -c 'import nei;print(nei.__version__)'")))
        (with-output-to-temp-buffer "NEI server info"
          (princ (format nei--server-info-msg nei-version nei-path py-path)))))
      )
    )



(provide 'nei-server)
