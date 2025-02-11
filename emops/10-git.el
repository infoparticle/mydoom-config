
;; Update PATH environment variable
(setenv "PATH" (concat
                "C:\\Program Files\\Git\\usr\\bin;"
                (getenv "PATH")))

;; Add Git binary path to exec-path
(add-to-list 'exec-path "C:/Program Files/Git/usr/bin")


;; Optional: Function to verify git is accessible
(defun emops/verify-git-path ()
  "Verify that git is accessible from Emacs."
  (interactive)
  (let ((git-path (executable-find "git")))
    (if git-path
        (message "Git found at: %s" git-path)
      (message "Git not found in exec-path!"))))
