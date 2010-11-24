;;
;; ST customizations for org-mode to export logbook entries
;;

;;;_* customization
(add-hook 'org-mode-hook
	  '(lambda()
	     (local-set-key [(shift f5)] 'org-export-as-latex)
	     (custom-set-variables
	      '(org-export-latex-date-format "")
	      '(org-export-latex-image-default-option "width=20em")
	      ;;'(org-export-latex-title-command "Week of %s")
	      ))
)

;;;_* preamble modifications
;; redefine preamble for 'article' class
;; why use push: http://blog.plover.com/prog/elisp.html
;; 2010-08-28 changed \usepackage{fullpage} to \usepackage{savetrees}
(push '("article"
	"\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage{savetrees}
\\parindent 0pt"
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	("\\paragraph{%s}" . "\\paragraph*{%s}")
	("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
      org-export-latex-classes)

;;;_* header modifications
;; modify the header to say "Week of ..."  
;; turned on by default; can be unactivated for other documents
(defadvice org-export-latex-make-header 
  (before org-export-latex-make-header-format-title activate)
  "This advice function formats the title of the exported org
   document to be read, \"Week of %s\" before it is
   processed/formatted by org-export-latex-make-header. Disable
   with the following lines:
   (ad-disable-advice 'org-export-latex-make-header 'before
  		      'org-export-latex-make-header-format-title)
   (ad-activate 'org-export-latex-make-header)"
  (ad-set-arg 0 (format "Week of %s" (ad-get-arg 0))))

(defun disable-advice-org-export-latex-make-header ()
  (interactive)
  (ad-disable-advice 'org-export-latex-make-header 'before
		     'org-export-latex-make-header-format-title)
  (ad-activate 'org-export-latex-make-header))

;;;_* formatting options
;; primarily to remove "$" as math expression
;; 'matcher'.
(setq org-format-latex-options 
      '(:foreground default 
		    :background default 
		    :scale 1.0
		    :html-foreground "Black" 
		    :html-background "Transparent" 
		    :html-scale 1.0
		    ;;:date ""
		    :matchers ("begin" "$$" "\\(" "\\[")))


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

;;;_* postprocessing

(defadvice org-export-as-latex
  (after org-export-as-latex-format-output activate)
  (let ((texfilename (concat (file-name-sans-extension (buffer-file-name)) ".tex")))
    (my-org-latex-postprocess texfilename)))

(defun my-org-latex-postprocess (&optional filename)
  (flet ((replace-includegraphics-underscore ()
	  "temp function. Returns a string based on current regex match."
	  (let (matchedText replacements)
	    (setq replacements '(("\\\\_" . "_") ("\\\\" . "\\\\\\\\")))
	    (setq matchedText
		  (buffer-substring (match-beginning 0) (match-end 0)))
	    (dolist (elem replacements matchedText)
	      (setq matchedText (replace-regexp-in-string (car elem) (cdr elem)  matchedText))))))
  (let ((tmpbuffer " myTemp"))
    ;; test if buffer is open
    (setq mybuffer (or (get-file-buffer filename)
		       (get-buffer-create tmpbuffer)))
    ;; do the stuff
    (set-buffer mybuffer)
    (insert-file-contents filename nil nil nil t)
    ;; section headings
    (goto-char (point-min))
    (while (re-search-forward 
	    "\\\\section\\([*]?\\){<\\([0-9]+-[0-9]+-[0-9]+ [A-Za-z]+\\)>}" nil t)
      (replace-match "\\\\section\\1{$\\\\langle${\\2}$\\\\rangle$}"))
    ;; graphics underscore
    (goto-char (point-min))
    (while (re-search-forward 
	    "\\\\includegraphics.*{.+}" nil t)
      (replace-match (replace-includegraphics-underscore)))
    (if (equal tmpbuffer (buffer-name mybuffer))
	(progn
	  (write-file filename)
	  (kill-buffer mybuffer))
      (save-buffer)))))

;;{{{
  ;; (let ()
  ;;   ;; create temp buffer without undo record.
  ;;   ;; first space in temp buff name is necessary
  ;;   (set-buffer (get-buffer-create " myTemp"))
  ;;   (insert-file-contents filename nil nil nil t)
  ;;   (goto-char (point-min))
  ;;   (while (re-search-forward 
  ;; 	    "\\\\section{<\\([0-9]+-[0-9]+-[0-9]+ [A-Za-z]+\\)>}")
  ;;     (replace-match "\\\\section{\\1}"))
  ;;   (write-file filename) ;; write back to the file
  ;;   (kill-buffer " myTemp")))
;;}}}
;;{{{
;; (defun  org-latex-replace-includegraphics-underscore ()
;;   "temp function. Returns a string based on current regex match."
;;   (let (matchedText replacements)
;;     (setq replacements '(("\\\\_" . "_") ("\\\\" . "\\\\\\\\")))
;;     (setq matchedText
;; 	  (buffer-substring (match-beginning 0) (match-end 0)))
;;     (dolist (elem replacements matchedText)
;;       (setq matchedText (replace-regexp-in-string (car elem) (cdr elem)  matchedText)))))
;;}}}
