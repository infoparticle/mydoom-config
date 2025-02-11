
;; Windows Service Management Functions using PowerShell
(defun emops/rum/enable-remote-debugging ()
  "Add JAVA_OPTS debug settings to run.bat if not present."
  (interactive)
  (let ((file "c:/RUM/EJBContainer/bin/run.bat")
        (debug-line "set \"JAVA_OPTS=%JAVA_OPTS% -Xrunjdwp:transport=dt_socket,address=8787,server=y,suspend=n\""))
    (with-temp-buffer
      (if (file-exists-p file)
          (progn
            (insert-file-contents file)
            ;; Check if line already exists
            (goto-char (point-min))
            (if (not (search-forward debug-line nil t))
                (progn
                  ;; Add line after existing JAVA_OPTS if found
                  (goto-char (point-min))
                  (if (search-forward "JAVA_OPTS" nil t)
                      (progn
                        (forward-line)
                        (insert debug-line "\n"))
                    ;; If no JAVA_OPTS found, add at the start
                    (goto-char (point-min))
                    (insert debug-line "\n"))
                  ;; Save the changes
                  (write-region (point-min) (point-max) file)
                  (message "Added Java debug options to run.bat"))
              (message "Debug options already present in run.bat")))
        (message "File %s does not exist" file)))))

;; Run it with:
;; (add-java-debug-opts)
(provide 'emops-service)
