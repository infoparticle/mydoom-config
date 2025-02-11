(defun emops/truncate-buffer ()
  "Truncate current buffer and save it."
  (interactive)
  (erase-buffer)
  (save-buffer))
