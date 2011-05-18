;;;_* keybindings

(add-hook 'ess-mode-hook
	  '(lambda()
	     (local-set-key (kbd "C-c R") 'ess-start-R)
	     (local-set-key (kbd "C-c s") 'ess-set-proc-name)
	     (local-set-key (kbd "C-c z") 'ess-setwd)))

;;;_* general

(defadvice ess-eval-region
  (after ess-eval-region-deactivate-mark activate)
  "Deactivate mark after executing region 
   (region is preserved after mark is deactivated)"
  (deactivate-mark))

(defun ess-start-R (&optional arg)
  "Runs R interpreter in inferior ESS buffer. Usage: if an R
  script open, e.g., 'script.r', invoke ess-start-R when this
  script buffer is active. If an inferior ESS buffer already
  exists, must use prefix key (e.g., if ess-start-R is bound to
  `C-c R`, the first instance is invoked by `C-c R`; subsequent
  inferior ESS buffers invoked with `C-u C-c R` -- otherwise `C-c
  R` will switch to top exising inferior ESS buffer). If
  'script.r' is the name of the R script, the buffer will be
  named *R<script.r>* or *R:2<script.r>*, depending on existing R
  processes.  Inspired by `my-ess-start-R` function in ESS Emacs
  Wiki <http://www.emacswiki.org/emacs/EmacsSpeaksStatistics> and
  implemented to mimic behavior of emacs inferior shell."
  (interactive "P")
  (flet ((find-R-buffers () ;; return top R buffer name or nil
			 (let ((buflist (reverse (buffer-list)))
			       (x nil)
			       (out nil))
			   (dolist (x buflist out)
			     (let ((bufname (buffer-name x)))
			       (if (string-match "*R" bufname)
				   (setq out bufname))))))
	 (is-wide-p ()
		    (let ((thres 160))
		      (> (frame-width) thres))))
	 (let ((this-buffer (buffer-name))
	       (r-proc nil)
	       (r-buffer nil))
	   ;; function body:
	   (setq r-buffer (find-R-buffers))
	   (if (is-wide-p) 
	       (delete-other-windows)
	     (condition-case nil
		 (delete-other-windows-vertically)
	       (error (delete-other-windows))))
	   (if (or (not r-buffer) arg)
	       (progn
		 (R)
		 (setq r-proc (buffer-name))
		 (setq r-buffer 
		       (concat r-proc 
			       (format "<%s>" 
				       (file-name-sans-extension 
					this-buffer))))
		 (rename-buffer r-buffer))
	     (switch-to-buffer r-buffer))
	   (if (is-wide-p) 
	       (split-window-horizontally)
	     (split-window-vertically))
	   (switch-to-buffer this-buffer)
	  (enlarge-window 10)
	  (setq ess-current-process-name 
		(if r-proc
		    (replace-regexp-in-string "\\*" "" r-proc)
		  (replace-regexp-in-string "\\*\\(R[:0-9]*\\)\\*(<.+>)?"
					    "\\1" 
					    r-buffer))))))

(defun ess-set-proc-name (R-name)
 (interactive "sEnter R process name: ")
 (setq ess-current-process-name R-name)
 (setq ess-local-process-name R-name))

(defun ess-setwd ()
  (interactive)
  (ess-eval-linewise (format "setwd(\"%s\")" 
			     (file-name-directory (buffer-file-name)))
		     nil nil nil 'wait))

(defun ess-inferior-scrub-name (&optional arg)
  "remove file name from inferior ESS buffer name"
  (interactive "P")
  (let ((iESS-buffer-name (buffer-name)))
    (if (string-match "\\*R" iESS-buffer-name)
	(rename-buffer 
	 (replace-regexp-in-string "<.+>" "" "*R*<read2_2011-05>"))
      (message "Not iESS buffer"))))

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
		    ;; (search-backward-regexp "[^a-zA-Z0-9]pdf[(,].*\"\\(.+\\.pdf\\)\".*)")
		    (search-backward-regexp "^.*pdf[(,].*\"\\(.+\\.pdf\\)\".*)")
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
