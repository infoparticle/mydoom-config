(let ((modules (directory-files
               (expand-file-name "emops" doom-user-dir)
               nil
               "^[0-9]+.*\\.el$")))
 (dolist (module modules)
   (load (expand-file-name (concat "emops/" module) doom-user-dir))))
