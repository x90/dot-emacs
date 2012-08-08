(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files
taken from http://www.emacswiki.org/emacs/RevertBuffer#toc4"
  (interactive)
  (let* ((list (buffer-list))
	 (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer) (not (buffer-modified-p buffer)))
	(set-buffer buffer)
	(revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshing open files"))
