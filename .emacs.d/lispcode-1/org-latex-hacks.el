;;;_* begin-end

(defun org-begin-or-end ()
  (interactive)
  (flet ((fn (str &optional suffix) 
	     (save-excursion 
	       ;; env is dynamically-scoped
	       (let (pos) ;; (match-beginning 0) will find last match if nil
		 (setq pos (re-search-backward 
			    (concat 
			     (replace-regexp-in-string "\\#\\+" "\\\\#\\\\+" str) 
			     suffix) () t))
		 (if suffix 
		     (setq env (match-string-no-properties 1)))
		 (or pos 0))))
	 (upcase-add (str &optional env) 
		     (save-excursion 
		       (beginning-of-line)
		       (insert (concat str env))
		       (if (< (point) (line-end-position))
			   (upcase-word 1)))))
    (let ((st "#+BEGIN_")
	  (en "#+END_")
	  env)
      (if (<= (fn st "\\([A-Z]+\\)") (fn en))
	  (upcase-add st)
	(progn
	  (upcase-add en env)
	  (end-of-line))))))

;;;_* customization
(add-hook 'org-mode-hook
	  '(lambda()
	     (local-set-key [(shift f5)] 'org-export-as-latex)
	     (custom-set-variables
	      '(org-export-latex-date-format "")
	      '(org-export-latex-image-default-option "width=20em")
	      ;;'(org-export-latex-title-command "Week of %s")
	      )))


