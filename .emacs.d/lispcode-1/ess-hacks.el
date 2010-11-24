;;;_* general
(defadvice ess-eval-region
  (after ess-eval-region-deactivate-mark activate)
  "Deactivate mark after executing region 
   (region is preserved after mark is deactivated)"
  (deactivate-mark))

(defun my-start-R-ESS (&optional arg)
  (interactive "P")
  (let ((this-buffer (buffer-name))
	(r-proc nil)
	(r-buffer nil))
    ;;{{{

    ;; the notany expression originally was
    ;; (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))

    ;;}}}
    (if (or (notany '(lambda (x) (string-match "*R" (buffer-name x))) 
		    (buffer-list))
	    arg)
	(progn
	  (condition-case nil
	      (delete-other-windows-vertically)
	    (error (delete-other-windows)))
	  (R)
	  (setq r-proc (buffer-name))
	  (setq r-buffer 
		(concat r-proc 
			(format "<%s>" (file-name-sans-extension this-buffer))))
		;; (replace-regexp-in-string 
		;;  "\\*$" 
		;;  (format "<%s>*" (file-name-sans-extension this-buffer))
		;;  r-proc))
	  (rename-buffer r-buffer)
	  (split-window-vertically)
	  (switch-to-buffer this-buffer)
	  (enlarge-window 10)
	  (setq ess-current-process-name 
		(replace-regexp-in-string "*" "" r-proc)))
      (message "An instance of R is already running"))))

(defun ess-set-proc-name (R-name)
 (interactive "sEnter R process name: ")
 (setq ess-current-process-name R-name)
 (setq ess-local-process-name R-name))

(defun ess-setwd ()
  (interactive)
  (ess-eval-linewise (format "setwd(\"%s\")" 
			     (file-name-directory (buffer-file-name)))
		     nil nil nil 'wait))

;;;_* send to ess-line (help)

(defun ess-send-to-function (ess-fn)
  (interactive)
  (let (fn-name)
    (setq fn-name
	  (if (region-active-p)
	      (buffer-substring-no-properties
	       (region-beginning) (region-end))
	    (let ((charset "A-Za-z0-9._")
		  pos1 pos2)
	      (save-excursion
		(skip-chars-backward charset)
		(setq pos1 (point))
		(forward-char)
		(skip-chars-forward charset)
		(setq pos2 (point))
		(buffer-substring-no-properties pos1 pos2)))))
    (ess-eval-linewise (format "%s(\"%s\")" ess-fn fn-name)
		       nil nil nil 'wait)))

(defun ess-seek-help () 
  (interactive)
  (ess-send-to-function "help"))
(defun ess-seek-args () 
  (interactive)
  (ess-send-to-function "args"))

;;;_* OS X (PDF functions)
(defun operate-on-pdf (fn &optional arg)
  (let (pdf-file pos1 pos2)
    (setq pdf-file
	  (if (region-active-p) ;; pdf name high-lighted
	      (buffer-substring-no-properties
	       (region-beginning) (region-end))
	    (if (or arg
		    (progn
		      (setq pos1 (point)
			    pos2 (save-excursion
				   (previous-line)
				   (line-beginning-position)))
		      (string-match "dev\\.off()" (buffer-substring-no-properties pos1 pos2))))
		  (save-excursion ;; point after dev.off()
		    (search-backward-regexp "[^a-zA-Z0-9]pdf[(,].*\"\\(.+\\.pdf\\)\".*)")
		    (match-string-no-properties 1))
		(save-excursion ;; point over pdf name
		  (search-backward "\"")
		  (setq pos1 (point))
		  (forward-char)
		  (search-forward "\"")
		  (setq pos2 (point))
		  (buffer-substring-no-properties pos1 pos2)))))
    (setq pdf-file (replace-regexp-in-string "\"?\\([^\"]+\\)\"?" 
					     "\\1" pdf-file))
    (if (string-match "\\.pdf$" pdf-file)
	(funcall fn pdf-file)
      (message (format "%s is not a pdf file." pdf-file)))))

(defun preview (filename)
 (start-process-shell-command "preview" nil
  (format "open -a \"Preview\" %s" filename)))

(defun compresspdf (filename)
  (interactive)
  (flet ((escape-space (x) (replace-regexp-in-string "[ ]" "\\\\ " x)))
    (let ((tmpfile (concat filename "~"))
	  origin haserror)
      (shell-command (format "pdftk %s cat output %s compress dont_ask"
			     (escape-space filename) (escape-space tmpfile)))
      (setq iserror
	    (save-excursion
	      (set-buffer "*Shell Command Output*")
	      (search-backward "error" () t)))
      (if haserror 
	  (if (file-exists-p tmpfile)
	      (delete-file tmpfile nil))
	(rename-file tmpfile filename t))
      nil)))

(defun point-and-compresspdf (&optional arg)
  (interactive)
  (operate-on-pdf 'compresspdf 'arg))

(defun send-to-preview (&optional arg)
  (interactive)
  (operate-on-pdf 'preview 'arg))
