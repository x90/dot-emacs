(defun fill-or-unfill-paragraph (&optional justify region)
  "Fill paragraph at or after point (see `fill-paragraph').
   Does nothing if `visual-line-mode' is on.
   From http://stackoverflow.com/questions/1416171/emacs-visual-line-mode-and-fill-paragraph"
  (interactive (progn
         (barf-if-buffer-read-only)
         (list (if current-prefix-arg 'full) t)))
  (if visual-line-mode
      (unfill-paragraph)
    (fill-paragraph justify region)))

(defun unfill-buffer ()
  "Undo filling for all paragraphs."
  (interactive)
  (goto-char (point-min))
  (let ((fill-column 99999))
    (fill-paragraph nil)
    (while (< (point) (point-max))
      (forward-paragraph)
      (fill-paragraph nil))))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unwrap-line ()
  "Remove all newlines until we get to two consecutive ones.
Or until we reach the end of the buffer.
Great for unwrapping quotes before sending them on IRC."
  (interactive)
  (let ((start (point))
        (end (copy-marker (or (search-forward "\n\n" nil t)
                              (point-max))))
        (fill-column (point-max)))
    (fill-region start end)
    (goto-char end)
    (newline)
    (goto-char start)))

;; http://xahlee.org/emacs/elisp_examples.html
(defun remove-line-breaks ()
  "Remove line endings in a paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; mine

(defun maybe-fill-paragraph (&optional justify region)
  "Fill paragraph at or after point (see `fill-paragraph').
   Does nothing if `visual-line-mode' is on.
   From http://stackoverflow.com/questions/1416171/emacs-visual-line-mode-and-fill-paragraph"
  (interactive (progn
         (barf-if-buffer-read-only)
         (list (if current-prefix-arg 'full) t)))
  (or visual-line-mode
      (fill-paragraph justify region)))

