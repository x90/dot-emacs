;;;_* begin-end

(defun org-begin-or-end ()
  (interactive)
  (flet ((fn (s) (save-excursion 
		   (or (search-backward s () t) 0)))
	 (upcase-add (s) (save-excursion 
			   (beginning-of-line)
			   (insert s)
			   (if (< (point) (line-end-position))
			       (upcase-word 1))
			   )))
    (let ((st "#+BEGIN_")
	  (en "#+END_"))
      (if (<= (fn st) (fn en))
	  (upcase-add st)
	(upcase-add en)))))

;;;_* customization
(add-hook 'org-mode-hook
	  '(lambda()
	     (local-set-key [(shift f5)] 'org-export-as-latex)
	     (custom-set-variables
	      '(org-export-latex-date-format "")
	      '(org-export-latex-image-default-option "width=20em")
	      ;;'(org-export-latex-title-command "Week of %s")
	      )))


