;; For starting up the server

(defvar nei--diagnose-server-msg
      "Could not import nei in the following Python environment:

%s

This issue can be addressed as follows:

1. Manually run 'pip install nei' to make the nei package importable by
the Python executable above.

2. If the path above is inappropriate and you are setting your default
Python in your shell (e.g via .bash_profile) you can install the
exec-path-from-shell package from MELPA. Then by running
(exec-path-from-shell-initialize) in your emacs session you can select a
more appropriate Python environment for NEI to use by default. Commonly
applicable on MacOS.

3. If you are using either Anaconda distribution or Miniconda, you can
install conda-mode from MELPA. You can then use the conda-env-activate
function to select an appropriate environment before running the NEI
server. To select a default conda environment for running the NEI
server, you can customize the nei-default-conda-env variable.

If you are happy to install NEI in the Python environment listed above,
you can now run the nei-pip-install-server command.
")


(defvar nei-default-conda-env "nei")

(defun start-nei-server (&optional verbose)
  "Starts the nei server if it isn't already running using nei-python-path"
  (interactive)
  (let ((proc (get-process "nei-server")))
    (if (null proc)
        (progn
          (message "Starting nei server")
          (let ((new-proc
                 (start-process "nei-server"
                                " *nei server log*" ;; Leading space hides the buffer
                                "python" "-c" "import nei;nei.serve()")))
            (set-process-query-on-exit-flag new-proc nil)
            (sleep-for 2))
          )
      (if verbose (message "Nei server already running"))
      )
    )
  )



(defun nei--launch-server (&optional env)
  "Check if nei is importable in Python after optionally activating a
   conda environment (if conda-mode available). If the check fails, open
   a help window with information to help diagnose and fix the problem."
  
  (if (and (fboundp 'conda-env-activate) (or env nei-default-conda-env))
      (progn
        (message "HELLO")
        (conda-env-activate (or env nei-default-conda-env))
        )

    )

  (let ((version (nei--server-version)))
    (if (null version) (nei--diagnose-missing-server)
      (progn
        (message "Launching NEI server version %s" version)
        (start-nei-server)
       )
      )
    )
  )

(defun nei--server-version ()
  "Returns the nei version string is available in Python, otherwise nil"
  (if (eq (get-exit-code "python") 0)
      (let ((stdout (nei--cmd-stdout "python -c 'import nei;print(nei.__version__)'")))
        (if (s-starts-with? "v" stdout)
            stdout))))


(defun nei--diagnose-missing-server ()
  (with-output-to-temp-buffer "NEI server configuration"
    (let ((executable
           (if (null (get-exit-code "python")) "Python executable not found"
             (nei--cmd-stdout "python -c 'import sys;print(sys.executable)'"))))
      (princ (format  nei--diagnose-server-msg executable))
      )
    )
  )


;;=============================;;
;; Managing the server process ;;
;;=============================;;

(defun stop-nei-server ()
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

(defun nei-server-log ()
  "View the server log buffer if the server process is running."
  (interactive)
  (let ((proc (get-process "nei-server")))
    (if (not (null proc))
        (with-current-buffer (process-buffer proc)
          (clone-indirect-buffer " *nei server log*" t)
          )
      )
    )
  )


(defun nei--cmd-stdout (cmd)
  (string-trim-right (shell-command-to-string cmd)))


(defun get-exit-code (program &rest args) ;
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (apply 'call-process program nil (current-buffer) nil args)))


(provide 'nei-server)
