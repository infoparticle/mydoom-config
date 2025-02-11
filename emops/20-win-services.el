(defun powershell (&optional buffer)
  "Launches a powershell in buffer *powershell* and switches to it."
  (interactive)
  (let ((buffer (or buffer "*powershell*"))
        (powershell-prog "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe"))
    (make-comint-in-buffer "shell" "*powershell*" powershell-prog)
    (switch-to-buffer buffer)))

(defcustom emops-default-service "rumengine"
  "Default service name for operations."
  :type 'string
  :group 'emops)

(defun emops/execute-ps-command (command)
  "Execute a PowerShell command and return the output buffer.
COMMAND is the PowerShell command to execute."
  (let ((buffer (get-buffer-create "*emops-service-output*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (concat (format-time-string "%Y-%m-%d %H:%M:%S") " - running: " command "\n\n"))
      (let ((exit-code
             (call-process "powershell" nil buffer t "-Command" command)))
        (list :exit-code exit-code :buffer buffer)))))

(defun emops/start-service (&optional service-name)
  "Start a Windows service using PowerShell.
If SERVICE-NAME is not provided, uses the default service name."
  (interactive
   (list (read-string
          (format "Service name (default %s): " emops-default-service)
          nil nil emops-default-service)))
  (let* ((service (or service-name emops-default-service))
         (cmd (format "Start-Service -Name '%s'; if ($?) { 'SUCCESS' } else { 'FAILED' }"
                     service))
         (result (emops/execute-ps-command cmd)))
    (with-current-buffer (plist-get result :buffer)
      (goto-char (point-min))
      (if (search-forward "SUCCESS" nil t)
          (message "Service '%s' started successfully" service)
        (error "Failed to start service '%s'. Check output buffer for details"
               service)))))

(defun emops/stop-service (&optional service-name)
  "Stop a Windows service using PowerShell.
If SERVICE-NAME is not provided, uses the default service name."
  (interactive
   (list (read-string
          (format "Service name (default %s): " emops-default-service)
          nil nil emops-default-service)))
  (let* ((service (or service-name emops-default-service))
         (cmd (format "Stop-Service -Name '%s'; if ($?) { 'SUCCESS' } else { 'FAILED' }"
                     service))
         (result (emops/execute-ps-command cmd)))
    (with-current-buffer (plist-get result :buffer)
      (goto-char (point-min))
      (if (search-forward "SUCCESS" nil t)
          (message "Service '%s' stopped successfully" service)
        (error "Failed to stop service '%s'. Check output buffer for details"
               service)))))

(defun emops/get-service-status (&optional service-name)
  "Get the status of a Windows service using PowerShell.
If SERVICE-NAME is not provided, uses the default service name.
Returns the status as a string and displays it in the minibuffer."
  (interactive
   (list (read-string
          (format "Service name (default %s): " emops-default-service)
          nil nil emops-default-service)))
  (let* ((service (or service-name emops-default-service))
         (cmd (format "(Get-Service -Name '%s').Status" service))
         (result (emops/execute-ps-command cmd)))
    (with-current-buffer (plist-get result :buffer)
      (let ((status (string-trim (buffer-string))))
        (message "Service '%s' status: %s" service status)
        status))))

(defun emops/display-service-info (&optional service-name)
  "Display detailed information about a Windows service using PowerShell.
If SERVICE-NAME is not provided, uses the default service name."
  (interactive
   (list (read-string
          (format "Service name (default %s): " emops-default-service)
          nil nil emops-default-service)))
  (let* ((service (or service-name emops-default-service))
         (cmd (format "Get-Service -Name '%s' | Format-List *" service))
         (result (emops/execute-ps-command cmd)))
    (display-buffer (plist-get result :buffer))))

(defun emops/rum/stop-trunc-start-rum()
  (interactive)
  (emops/stop-service "rumengine")
  (find-file "c:/RUM/log/jboss_boot.log")
  (emops/truncate-buffer )
  (emops/start-service "rumengine")
  )
