(defun LaTeX-wrap-environment-around-thing-or-region (env &optional arg)
  (interactive "sEnvironment: " "P")
  (require 'latex)
  (let (pos1 pos2 bds myText)
    (if (and transient-mark-mode
  	     mark-active)
  	(setq pos1 (region-beginning) pos2 (region-end))
    (progn
      (setq bds (LaTeX-bounds-of-expression))
      (setq pos1 (car bds) pos2 (cdr bds))))
    ;; do the thing
    (setq myText (buffer-substring-no-properties pos1 pos2))
    (goto-char pos1)
    (delete-region pos1 pos2)
    (LaTeX-insert-environment env)
    (setq pos1 (point))
    (insert myText)
    (indent-region pos1 (point))))

(defun LaTeX-insert-item-no-newline ()
  (interactive)
  (insert "\\item "))


(defun LaTeX-enclose-expression (start-char &optional end-char)
  (lexical-let ((start-char start-char)
		(end-char (or end-char start-char)))
    (labels ((safe-char-after (&optional x) 
			      (if (equal (point-max) (point)) 0 (char-after x)))
	     (wrap-region ()
			  (let ((pos1 (region-beginning))
				(pos2 (region-end)))
			    (goto-char pos2)
			    (insert end-char)
			    (save-excursion
			      (goto-char pos1)
			      (insert start-char))))
	     (wrap-backwards (search-chars)
			     (let* ((pos1 (if visual-line-mode
					      (save-excursion
						(beginning-of-visual-line)
						(point))
					    (line-beginning-position)))
				    (pos2 (save-excursion
					    (if (re-search-backward search-chars () t)
						(if (equal " " (match-string-no-properties 0))
						    (match-end 0)
						  (match-beginning 0))
					      pos1))))
			       (save-excursion
				 (goto-char (max pos1 pos2))
				 (insert start-char))
			       (insert end-char))))
      (lambda (&optional arg)
	(interactive "P")
	(if (region-active-p)
	    ;; highlighted region
	    (wrap-region)
	  ;; not highlighted
	  (if arg
	      ;; search forward and backward
	      (progn
		(if (not (char-equal (string-to-char " ") 
				     (safe-char-after (point))))
		    ;; find end of word
		    (goto-char 
		     (if (re-search-forward "[ \n]" () t)
			 (match-beginning 0)
		       (if visual-line-mode
		       	   (save-excursion
		       	     (end-of-visual-line)
		       	     (point))
		       	 (line-end-position)))))
		  (wrap-backwards "[ ]"))
	    ;; search only backwards
	    (wrap-backwards "[ _^\\\\]")))))))


(defun LaTeX-match-pattern (re-pattern match-num outputbuffer &optional skipsort-p)
  (lexical-let ((re-pattern re-pattern)
		(match-num match-num)
		(outputbuffer outputbuffer)
		(skipsort-p skipsort-p))
    (lambda (&optional arg)
      (interactive "P")
      (let (matches)
	(save-excursion 
	  (goto-char (point-min))
	  (while (re-search-forward re-pattern () t)
	    (setq matches (append matches (list (match-string-no-properties match-num))))))
	(setq matches (delete-dups matches))
	(when (not skipsort-p)
	  (setq matches (sort matches 'string-lessp)))
	(get-buffer-create outputbuffer)
	(save-excursion
	  (set-buffer outputbuffer)
	  (when (not (equal (point) (point-min)))
	    (goto-char (point-max))
	    (insert "\n"))
	  (insert "--- new list ---\n")
	  (insert (mapconcat 'identity matches "\n"))
	  (insert "\n"))
	(if arg
	    (switch-to-buffer outputbuffer))))))
    
(fset 'latex-find-citations
      (LaTeX-match-pattern "\\\\\\(citet\\|citep\\|citealp\\){\\([^}]+\\)}" 
			  2 "*citations*"))

(fset 'latex-find-figrefs
      (LaTeX-match-pattern "\\\\ref{\\(fig[:][^}]+\\)}" 
			   1 "*figlist*" t))

