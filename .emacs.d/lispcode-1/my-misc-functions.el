(defun kill-other-buffers ()
    "Kill all other buffers."
    ;; from stackoverflow
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

(defun my-kill-region ()
  (interactive)
  ;; from stackoverflow
  (if (region-active-p)
      (call-interactively 'kill-region)
    (message "Region not active, didn't kill")))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(defun copy-pwd (&optional arg)
  "Copies (pwd) to kill-ring. arg can be used to for argument to other-window."
  (interactive "P")
  (flet ((copyfn () (kill-new 
		    (replace-regexp-in-string "^Directory[ ]" "" (pwd)))))
    (if arg
	(save-excursion
	  (save-window-excursion
	    (other-window (if (equal '(4) arg) 1 arg))
	    (copyfn)))
      (copyfn))))

(defun add-column-offset (&optional goal-column)
  (interactive)
  (let ( n )
    (if (not goal-column)
	(setq goal-column 40))
    (beginning-of-line)
    (goto-char (1- (search-forward "#")))
    (if (< (current-column) goal-column)
	(progn (setq n (- goal-column (current-column)))
	       (insert 
		(reduce '(lambda (x y) (concat x " ")) 
			(number-sequence 1 n)
			:initial-value ""))))))

(defun string-repeat (str n)
  ;; http://curiousprogrammer.wordpress.com/2009/07/26/emacs-utility-functions/
  (let ((retval ""))
    (dotimes (i n)
      (setq retval (concat retval str)))
    retval))

(defun number-column (from to fmt)
  (interactive)
  (if (not fmt) (setq fmt "%d"))
  (kill-new (mapconcat (lambda (x) (format "%03d" x))
		       (number-sequence from to) "\n")))


(defun clean-non-ascii ()
  "Replaces all non-ascii characters or sequence of characters with space
http://www.gnu.org/s/emacs/manual/html_node/elisp/Regexp-Special.html
Can be re-written to handle regions and save excursion, etc."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (re-search-forward "[^[:ascii:]]")
    (replace-match " ")))

(defun downcap (&optional arg)
  (interactive "P")
  (when (region-active-p)
    (save-excursion
      (let (pos1 pos2 patt p)
	(setq patt (mapcar (lambda (x) (format "[ :]+%s[ :]+" x))
			   '("A" "The" "Of" "With" "In" "And" "Or" "But")))
	(setq pos1 (region-beginning) pos2 (region-end))
	(downcase-region pos1 pos2)
	(capitalize-region pos1 pos2)
	(when (not arg) ;; downcase articles and prepositions
	  (dolist (p patt)
	    (goto-char pos1)
	    (while (re-search-forward p pos2 t)
	      (replace-match (downcase (match-string-no-properties 0))))))))))
