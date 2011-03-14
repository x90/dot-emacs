(require 'cl)
(defun* pdf-extractor (urlname dirname &optional (loadpage t))
  (require 'w3m)
  ;; set parms
  (setq w3m-async-exec t
	w3m-command-arguments '("-o" "HTTP_PROXY=http://webproxy.ucsd.edu:3128"))
  ;; open page
  (when loadpage 
    (w3m-browse-url urlname))
  (lexical-let ((dirname (file-name-as-directory (expand-file-name dirname))))
    ;; create directory
    (when (not (file-exists-p dirname) )
      (mkdir dirname t))
    (labels ((in-folder (filename) (concat dirname filename)))
      ;; side effect is function creation to which local dirname is attached
      (defun extract-pdf-links ()
	(interactive)
	;; local variables
	(let (ix url-file local-file ex testvar)
	  (goto-char (point-min))
	  (setq ix 0)
	  (while (<= (point) (point-max));; (< ix 3)
	    ;; wait
	    (sleep-for 10)
	    ;; search for term
	    (setq testvar (search-forward "Download PDF" nil t))
	    ;; get link information
	    (setq url-file (get-text-property 0 'w3m-href-anchor (match-string 0)))
	    ;; get page numbers
	    (setq local-file
		  (let ((basename (file-name-nondirectory url-file)))
		    (if (or (string-match "front\\-matter" basename)
			    (string-match "back\\-matter" basename))
			(in-folder basename)
		      (save-excursion
			(re-search-backward "\\* \\([0-9]+\\)\\-\\([0-9]+\\)" nil t)
			(setq local-file 
			      (in-folder (format "pp%04d-%04d_%s.pdf" 
						 (string-to-int (match-string-no-properties 1))
						 (string-to-int (match-string-no-properties 2))
						 (file-name-sans-extension basename))))))))
	    ;; copy file
	    (when (not (file-exists-p local-file))
	      (princ (format "(w3m-download \"%s\" \"%s\")\n" url-file local-file))
	      (condition-case ex
		  (w3m-download url-file local-file)
		('error (message (format "Caught exception: [%s]" ex)))))
	      ;; (sleep-for 20)) ;; doesn't work?
	    (setq ix (1+ ix))))))))
