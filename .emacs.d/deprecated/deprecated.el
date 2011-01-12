;;;_* ===== Shell here =====

;; don't need I think - this is shell's default behavior
;;http://atomized.org/2008/07/emacs-open-a-shell-in-the-current-directory/
;; (global-set-key (kbd "C-c !") 'shell-here)
;; (defun shell-here ()
;;   "Open a shell in `default-directory'."
;;   (interactive)
;;   (let ((dir (expand-file-name default-directory))
;;         (buf (or (get-buffer "*shell*") (shell))))
;;     (goto-char (point-max))
;;     (if (not (string= (buffer-name) "*shell*"))
;;         (switch-to-buffer-other-window buf))
;;     (message list-buffers-directory)
;;     (if (not (string= (expand-file-name list-buffers-directory) dir))
;;         (progn (comint-send-string (get-buffer-process buf)
;;                                    (concat "cd \"" dir "\"\r"))
;;                (setq list-buffers-directory dir)))))

;;;_* ===== Tabbar mode =====
;; (require 'tabbar)
;; (tabbar-mode t)
;; (global-set-key (kbd "S-<s-left>") 'tabbar-backward) 
;; (global-set-key (kbd "S-<s-right>") 'tabbar-forward) 

;;;_* ===== Elscreen =====
;; (load "elscreen" "ElScreen" t)
;; ;; F9 creates a new elscreen, shift-F9 kills it
;; (global-set-key (kbd "<f9>"    ) 'elscreen-create)
;; (global-set-key (kbd "S-<f9>"  ) 'elscreen-kill)  
;; ;; Windowskey+PgUP/PgDown switches between elscreens
;; (global-set-key (kbd "S-<s-up>") 'elscreen-previous) 
;; (global-set-key (kbd "S-<s-down>")  'elscreen-next) 

;;;_* ===== Temporary =====

;; hide comments for c

;; (setq hs-special-modes-alist-default hs-special-modes-alist)
;; (setq hs-special-modes-alist
;;       (let ( (list hs-special-modes-alist-default) 
;; 	     (replacement '((1 . "/[*]{{{") (2 . "}}}[*]/"))) )
;; 	(cons (reduce '(lambda (init x) (setf (nth (car x) init) (cdr x)) init)
;; 		      replacement :initial-value (car list))
;; 	      (cdr list))))

;;(add-to-list 'load-path "~/.emacs.d/org-mode/lisp") ;; add to top of path
;; (add-to-list 
;;  'load-path "/Applications/MATLAB_R2008a/java/extern/EmacsLink/lisp" t) 
;; (add-to-list 'load-path "/usr/share/emacs/22.1/lisp/textmodes" t)

;; additional packages:

;; (add-to-list 'load-path "~/elisp/apel")
;; (add-to-list 'load-path "~/elisp/elscreen")

;;;_* ===== folding =====

;;{{{--- original version ---
;; (defun folding-insert-mode-mark ()
;;   (interactive)
;;   (let (string-to-insert)
;;     (destructuring-bind (st en) 
;; 	(cdr (assoc major-mode folding-mode-marks-alist))
;;       (save-excursion
;; 	(setq string-to-insert
;; 	      (let ((here (point))
;; 		    sp ep)
;; 		(setq sp (search-backward st nil t))
;; 		(goto-char here)
;; 		(setq ep (search-backward en nil t))
;; 		(if (and (equal sp nil) (equal ep nil))
;; 		    st (if (and (not (equal sp nil)) (equal ep nil))
;; 			   en (if (< sp ep)
;; 				  st en))))))
;;       (insert string-to-insert))))
;;}}}

;;;_ . outline mode
;; (add-hook 'outline-minor-mode-hook 
;; 	  (lambda () (setq outline-minor-mode-prefix "\C-c<space>")))

;;{{{

;; (global-set-key [M-up] 'outline-previous-heading)
;; (global-set-key [M-down] 'outline-next-heading)
;; (global-set-key [M-left] 'hide-entry)
;; (global-set-key [M-right] 'show-entry)
;; (global-set-key [M-s-up] 'hide-body)
;; (global-set-key [M-s-down] 'show-all)
;; (global-set-key [M-s-left] 'hide-sublevels)
;; (global-set-key [M-s-right] 'show-children)

;;}}}

;; (defun outline-generate-level (commentchar)
;;   (lambda ()
;;     (let (buffer-invisibility-spec)
;;       (save-excursion
;; 	(search-forward-regexp 
;; 	 (format "^%s%s%s_\\(\\*\\|[ ]+[+-=>()[{}&!?#%\"X@$~_\\:;^]\\)"
;; 		 commentchar)
;; 	 (- (current-column) 3))))))
;; (defun outline-generate-hook (commentchar)
;;   (lambda ()
;;     (setq outline-regexp (format "^%s%s%s_" commentchar))
;;     (setq outline-level (outline-generate-level commentchar))
;;     (outline-minor-mode t)
;;     (hide-body)))

;; (add-hook 'outline-minor-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key (kbd "C-c i") 'outline-insert-bullet)))

;;;### old draft for outline-mode
;;{{{

;; (add-hook 'emacs-lisp-mode-hook 
;; 	  '(lambda ()
;; 	     (flet ((elisp-outline-level ()
;; 					 (let (buffer-invisibility-spec)
;; 					   (save-excursion
;; 					     (skip-chars-forward "  ")
;; 					     (current-column)))))
;; 	       (setq outline-regexp "[ \\t]*;;  ")
;; 	       (setq outline-level 'elisp-outline-level)
;; 	       (outline-minor-mode t)
;; 	       (hide-body)
;; 	       )))

;;}}}

;;;_ . allout-mode 

;; (require 'allout)
;; (allout-init t)
;; ;; (require 'allout-widgets)
;; ;; (allout-mode-widgets-init)
;; (defun my-allout-settings ()
;;   (setq allout-layout t)
;;   ;; (setq allout-plain-bullets-string "*+")
;;   ;; (setq allout-command-prefix "\C-c")
;;   ;;st (setq include-trailing-blank t)
;;   )
;; (defadvice allout-end-of-subtree 
;;   (before allout-end-of-subtree-incl-blank activate)
;;   (ad-set-arg 1 t))
;; (defadvice allout-end-of-current-subtree 
;;   (before allout-end-of-current-subtree-incl-blank activate)
;;   (ad-set-arg 0 t))

;; (dolist (hook (list 'emacs-lisp-mode-hook
;; 		    'ess-mode-hook))
;;   (add-hook hook 'my-allout-settings)) ; (-1 () : 1 0))))

;;;_ . hide-show 

;; unused
;; (add-to-list 'hs-special-modes-alist
;; 	     '(ess-mode "##{{{" "##}}}" "#[ #]?"
;; 			forward-sexp ;; can use nil
;; 			hs-c-like-adjust-block-beginning ;; can use nil
;; 			))
;; (add-hook 'ess-mode-hook '(lambda()
;; 			    (hs-minor-mode 1)
;; 			    (hs-hide-all)
;; 			    (custom-set-variables
;; 			     (hs-hide-comments-when-hiding-all 0)
;; 			     )
;; ))

;;;_  : folding-mode 
;; (add-hook 'emacs-lisp-mode-hook 'turn-on-folding-mode)

;; ;; ;;;_  : outline-mode in the style of allout 
;; (add-hook 'emacs-lisp-mode-hook 'my-elisp-outline-hook)
;; (defun my-elisp-outline-level ()
;;   (let (buffer-invisibility-spec)
;;     (save-excursion
;;       (search-forward-regexp 
;;        "^;;;_\\(\\*\\|[ ]+[+-=>()[{}&!?#%\"X@$~_\\:;^]\\)")
;;       (- (current-column) 3))))
;; (defun my-elisp-outline-hook ()
;;   (setq outline-regexp "^;;;_")
;;   (setq outline-level 'my-elisp-outline-level)
;;   (outline-minor-mode t)
;;   (hide-body))


;;;_* ===== python-mode =====

;; doesn't work
;; (defadvice py-execute-region
;;   (after py-execute-region-other-window activate)
;;   """ After execution, return to script buffer """
;;   (other-window 1)
;; )
;; (defadvice py-execute-region
;;    (around preserve-window-configuration activate)
;;    "After execution, return cursor to script buffer
;; http://stackoverflow.com/questions/1416882/emacs-defadvice-on-python-mode-function"
;;    (save-window-excursion ad-do-it))

;;;_* ===== org-mode =====

;;{{{---fix---
;; (defun org-begin-or-end ()
;;   (interactive)
;;   (flet ((find-previous (s) 
;; 		       (save-excursion 
;; 			 (let ((pos (search-backward s () t)))
;; 			   (if pos
;; 			       (let ((init 0)
;; 				     (end 0)
;; 				     (val ""))
;; 				 (forward-word)
;; 				 (forward-word)
;; 				 (setq end (point))
;; 				 (backward-word)
;; 				 (setq init (point))
;; 				 (setq val (buffer-substring-no-properties 
;; 					    init end))
;; 				 '(pos . val))
;; 			     '(0 . "")))))
;; 	 (upcase-add (s) 
;; 		     (save-excursion 
;; 		       (beginning-of-line)
;; 		       (insert s)
;; 		       (if (< (point) (line-end-position))
;; 			   (upcase-word 1)))))
;;     (let* ((ststr "#+BEGIN_")
;; 	   (enstr "#+END_")
;; 	   (stmatch (find-previous ststr))
;; 	   (enmatch (find-previous enstr)))
;;       (message (format "%s %s %s %s" 
;; 		       (number-to-string (car stmatch)) (cdr stmatch)
;; 		       (number-to-string (car enmatch)) (cdr enmatch)))
;;       (if (<= (car stmatch) (car enmatch))
;; 	  (upcase-add ststr)
;; 	(upcase-add (concat enstr (cdr stmatch)))))))
;;}}}

;;;_* ===== shell-mode =====

;; (add-hook 'shell-mode-hook
;; 	  '(lambda()
;; 	     (local-set-key (kbd "C-a") 'beginning-of-shell-line)))
;; (defun beginning-of-shell-line ()
;;   (interactive)
;;   (search-backward-regexp "@[^$]*[$]")
;;   (search-forward "$")
;;   (forward-char))

;;;_* ===== line-wrapping =====

;; (defadvice folding-end-of-line (around folding-end-of-visual-line activate)
;;   " end-of-line is end-of-visual-line if visual-line-mode is t"
;;   (let ((end-of-line end-of-line))
;;     (if visual-line-mode
;; 	(end-of-line end-of-visual-line))
;;     ad-do-it)
;; )
;; (setq visual-line-mode-map
;;       (funcall 
;;        (lambda ()
;; 	 (let ( (vismap visual-line-mode-map)
;; 		(eolmap '(folding-end-of-line . end-of-visual-line)) )
;; 	   (cons (car vismap)
;; 		 (cons (append (car (cdr vismap)) (list eolmap))
;; 		       nil))))))

;;;_* ===== frame-functions =====
;; (defun resize-sixty () 
;;   (interactive) 
;;   (sizetw 80 65))
;; (defun double-wide ()
;;   (interactive)
;;   (sizetw 160 60)
;;   (if (< (count-windows) 2)
;;       (split-window-horizontally)))


;;;_* ===== misc-functions =====

;; (defun insert-pwd ()
;;   (interactive)
;;   (insert (replace-regexp-in-string "^Directory[ ]" "" (pwd))))

;; (defun copy-pwd ()
;;   (interactive)
  ;;{{{
  ;; (let ( (string (replace-regexp-in-string "^Directory[ ]" "" (pwd))) )
  ;;   ;;http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/kill_002dnew-function.html
  ;;   (push string kill-ring)
  ;;   (setq kill-ring (cons string kill-ring))
  ;;   (if (> (length kill-ring) kill-ring-max)
  ;; 	;; avoid overly long kill ring
  ;; 	(setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
  ;; (setq kill-ring-yank-pointer kill-ring))
  ;;}}}
;;   (kill-new (replace-regexp-in-string "^Directory[ ]" "" (pwd))))

;;{{{
;; (defun send-to-preview ()
;;   (interactive)
;;   (let (pdf-file)
;;     (setq pdf-file
;; 	  (if (region-active-p)
;; 	      (buffer-substring-no-properties
;; 	       (region-beginning) (region-end))
;; 	    (let (pos1 pos2)
;; 	      (save-excursion
;; 		(search-backward "\"")
;; 		(setq pos1 (point))
;; 		(forward-char)
;; 		(search-forward "\"")
;; 		(setq pos2 (point))
;; 		(buffer-substring-no-properties pos1 pos2)))))
;;     (setq pdf-file (replace-regexp-in-string "\"?\\([^\"]+\\)\"?" 
;; 					     "\\1" pdf-file))
;;     (if (string-match "\\.pdf$" pdf-file)
;; 	(preview pdf-file)
;;       (message (format "%s is not a pdf file." pdf-file)))))
;;}}}

;; http://www.emacswiki.org/emacs/EmacsApp
;; (setenv "PATH" 
;; 	(let ((defaultpath (getenv "PATH"))
;; 	      (pythonpath "/Library/Frameworks/Python.framework/Versions/Current/bin")
;; 	      (addpath "/usr/local/bin:/usr/texbin:/usr/X11/bin:/opt/local/bin:/opt/local/sbin:/Users/stakahama/bin")
;; 	      (newpath nil))
;; 	  (setq newpath (concat pythonpath ":" defaultpath ":" addpath))
;; 	  (mapconcat 'identity (delete-dups (split-string newpath ":")) ":")))

;; (setenv "PATH" (concat (getenv "PATH") ":"
;; "/Applications/Emacs.app/Contents/Resources/lisp/textmodes/"))

;;{{{

;; (defun my-ess-start-R ()
;;   (interactive)
;;   (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
;;       (progn
;; 	(delete-other-windows)
;; 	(setq w1 (selected-window))
;; 	(setq w1name (buffer-name))
;; 	(setq w2 (split-window w1))
;; 	(R)
;; 	(set-window-buffer w2 "*R*")
;; 	(set-window-buffer w1 w1name))))

;; (defun my-ess-eval ()
;;   (interactive)
;;   (my-ess-start-R)
;;   (if (and transient-mark-mode mark-active)
;;       (call-interactively 'ess-eval-region)
;;     (call-interactively 'ess-eval-line-and-step)))

;;}}}

;;;_* ===== Nav-mode =====
;;(add-to-list 'load-path (concat emacs-root "nav-mode"))
;; (require 'nav)


;; ;;;_* ===== cua mode =====

;; ;; http://trey-jackson.blogspot.com/2008/10/emacs-tip-26-cua-mode-specifically.html
;; ;; (setq cua-highlight-region-shift-only t) ;; no transient mark mode
;; ;; (setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
;; (cua-mode -1)
;; ;; (setq cua-enable-cua-keys nil) ;; only for rectangles
;; ;; (setq cua-delete-selection nil) ;; only for rectangles
;;;_* ===== Tex-mode =====

;; (defun LaTeX-bounds-of-expression ()
;;   "looks backwards"
;;   (let ((expr-chars "-_A-Za-z0-9[]\\\\={},.^*+"))
;;     (if (not (char-equal (string-to-char " ") (char-after (point))))
;; 	(skip-chars-forward expr-chars))
;;     (save-excursion
;;       (setq pos2 (point))
;;       (skip-chars-backward expr-chars)
;;       (setq pos1 (point))
;;       (cons pos1 pos2))))

;; (defun LaTeX-thing-or-region-as-math-expression ()
;;   (interactive)
;;   (let (pos1 pos2 bds this-expr start-char end-char)
;;   (if (and transient-mark-mode
;;            mark-active)
;;       (setq pos1 (region-beginning) pos2 (region-end))
;;     (progn
;;       (setq bds (LaTeX-bounds-of-expression))
;;       (setq pos1 (car bds) pos2 (cdr bds))))
;;   (if (string-match "LaTeX" mode-name)
;;       (setq start-char "$" end-char "$")
;;     (if (string-match "Org" mode-name)
;; 	(setq start-char "\\(" end-char "\\)")
;;       (setq start-char "" end-char "")))
;;   (setq this-expr (buffer-substring-no-properties pos1 pos2))
;;   (delete-region pos1 pos2)
;;   (goto-char pos1)
;;   (insert (concat start-char this-expr end-char))))


;; (defadvice LaTeX-insert-item 
;;   (after LaTeX-insert-item-no-newline activate)
;;   (beginning-of-line)
;;   (forward-line -1)
;;   (kill-line)
;;   (end-of-line)
;; )


;; (defun LaTeX-enclose-expression (start-char &optional end-char)
;;   (lexical-let ((start-char start-char)
;; 		(end-char (if end-char end-char start-char)))
;;     (lambda (&optional arg)
;;       (interactive "P")
;;       "needs work: want prefix case to enclose everything between spaces"
;;       (flet ((safe-backward-char () (if (not (equal (point-min) (point)))
;; 					(backward-char)))
;; 	     (safe-char-after (&optional x) (if (equal (point-max) (point))
;; 				      0 (char-after x)))
;; 	     (place-chars (expr-chars)
;; 			  (let ((origin (point)) 
;; 				(subs '(("\\\\" . "\\\\\\\\")
;; 					("\\^" . "\\\\^")))
;; 				firstchar)
;; 			    (insert end-char)
;; 			    (save-excursion
;; 			      (goto-char origin)
;; 			      (skip-chars-backward 
;; 			       (concat alpha-num expr-chars))
;; 			      (setq firstchar (char-to-string (char-before)))
;; 			      (dolist (s subs firstchar)
;; 				(setq firstchar (replace-regexp-in-string
;; 				       (car s) (cdr s) firstchar)))
;; 			      (if (not (string-match firstchar alpha-num))
;; 				  (safe-backward-char))
;; 			      (insert start-char)))))
;; 	(let ((alpha-num "A-Za-z0-9")
;; 	      (expr-chars-full "<>[]{},.^*+-=_\\\\")
;; 	      (expr-chars-partial "[]{},.*+-="))
;; 	  (if (region-active-p)
;; 	      ;; highlighted region
;; 	      (let ((pos1 (region-beginning))
;; 		    (pos2 (region-end)))
;; 		(goto-char pos2)
;; 		(insert end-char)
;; 		(save-excursion
;; 		  (goto-char pos1)
;; 		  (insert start-char)))
;; 	    ;; not highlighted
;; 	    (if arg
;; 		;; search forward and backward
;; 		(progn
;; 		  (if (not (char-equal (string-to-char " ") 
;; 				       (safe-char-after (point))))
;; 		      (skip-chars-forward (concat alpha-num expr-chars-full)))
;; 		  (place-chars expr-chars-full))
;; 	      ;; search backward
;; 		(place-chars expr-chars-partial))))))))
