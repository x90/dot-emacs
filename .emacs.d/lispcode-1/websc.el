(require 'cl)
(defun* pdf-extractor (urlname dirname &optional (loadpage t))
  "Creates a function, extract-pdf-links
Usage: (pdf-extractor 
        \"http://www/contents/\"
        \"Directory\""
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
    (labels ((in-folder (filename) (concat dirname filename))
	     (maybe (matchval) 
		    (let ((val (string-to-int matchval)))
		      (if (= val 0) matchval (format "%04d" val)))))
      ;; side effect is function creation to which local dirname is attached
      (defun extract-pdf-links ()
	(interactive)
	;; local variables
	;; (setq w3m-async-exec nil)
	(let (ix url-file local-file ex testvar)
	  (goto-char (point-min))
	  (setq ix 0)
	  (while (<= (point) (point-max));; (< ix 3)
	    ;; wait
	    ;; search for term
	    (setq testvar (search-forward "Download PDF" nil t))
	    ;; get link information
	    (setq url-file (get-text-property 0 'w3m-href-anchor (match-string 0)))
	    ;; get page numbers
	    (setq local-file
		  (let ((basename (file-name-nondirectory url-file)))
		    ;; (if (or (string-match "front\\-matter" basename)
		    ;; 	    (string-match "back\\-matter" basename))
		    ;; 	(in-folder basename)
		    (save-excursion
		      (re-search-backward "\\* \\([0-9]+\\|I\\)\\-\\([0-9]+\\|[IXV]+\\)" nil t)
		      (setq local-file 
			    (in-folder (format "pp%s-%s_%s.pdf" 
					       (maybe (match-string-no-properties 1))
					       (maybe (match-string-no-properties 2))
					       (file-name-sans-extension basename)))))))
		  ;; )
	    ;; copy file
	    (when (not (file-exists-p local-file))
	      (princ (format "(w3m-download \"%s\" \"%s\")\n" url-file local-file))
	      (condition-case ex
		  (w3m-download url-file local-file)
		('error (message (format "Caught exception: [%s]" ex))))
	      (sleep-for 10)) ;; doesn't work?
	    (setq ix (1+ ix))))))))

