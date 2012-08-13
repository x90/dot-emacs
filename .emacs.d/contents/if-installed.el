;;;_* ===== Key bindings =====

;;;_ . --- rebindings ---
(global-set-key (kbd "M-/") 'hippie-expand)		;; built-in
(global-set-key (kbd "C-w") 'my-kill-region)		;; lispcode-1/my-misc-functions.el
(global-set-key "\M-q" 'fill-or-unfill-paragraph)	;; lispcode-1/unfill-functions.el

;;;_ . --- unset ns-keys ---
(global-unset-key (kbd "s-p")) 
(global-unset-key (kbd "s-q")) 
(global-unset-key (kbd "s-w")) 
(global-unset-key (kbd "s-t")) 

;;;_  : to user-defined functions or packages

(global-set-key (kbd "C-c %") 'match-paren)              ;; lispcode-1/my-misc-functions.el
(global-set-key (kbd "C-x W") 'rename-file-and-buffer)   ;; lispcode-2/move-rename-yegge.el
(global-set-key (kbd "C-c Q") 'fix-horizontal-size)      ;; lispcode-2/frame-resizing-functions.el
(global-set-key [(shift f1)] 'find-first-non-ascii-char) ;; lispcode-2/unsafechars.el

;;;_* ===== Functions =====

;;;_ . general
(load "unsafechars")
(load "count-words")
(load "my-misc-functions")

;;;_ . unfill functions

(load "unfill-functions")

;;;_ . files and buffers move/rename

(load "move-rename-yegge")
(load "frame-window-buffer-functions")

;;;_ . revert all buffers

(load "revert-all-buffers")

;;;_* ===== Frame-resizing-functions

(load "frame-resizing-functions")

;;;_* ===== Python-mode =====
(when (or t (file-exists-p (concat local-packages "python-mode"))) ;; using archived
  (add-to-list 'load-path (concat local-packages "python-mode"))
  (require 'python-mode)
  (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
  (autoload 'python-mode "python-mode" "Python editing mode." t)
  ;; (defun load-python (&optional arg)
  ;;   (interactive)
  ;;   (load "python-mode" t)
  ;;   (if arg (python-mode)))
  ;; (load-python)

;;;_ . hook
  (add-hook 'python-mode-hook 
	    '(lambda () 
	       (local-set-key (kbd "C-c z") 'py-oschdir)
	       (local-set-key (kbd "C-c C-j") 'py-execute-line)
	       (local-set-key (kbd "C-c C-p") 'py-execute-paragraph)
	       (local-set-key (kbd "<C-return>") 'py-execute-region)))

;;;_ . functions
  (defun py-mark-line ()
    (interactive)
    (end-of-line)
    (push-mark (point))
    (beginning-of-line)
    (exchange-point-and-mark)
    (py-keep-region-active))
  (defun py-execute-line (&optional async)
    (interactive "P")
    (save-excursion
      (py-mark-line)
      (py-execute-region (mark) (point) async)))
  (defun py-mark-paragraph ()
    (interactive)
    (forward-paragraph)
    (push-mark (point))
    (backward-paragraph)
    (exchange-point-and-mark)
    (py-keep-region-active))
  (defun py-execute-paragraph (&optional async)
    (interactive "P")
    (save-excursion
      (py-mark-paragraph)
      (py-execute-region (mark) (point) async)))
  (defun py-oschdir ()
    (interactive)
    (let ((txt (format "import os; os.chdir(\"%s\")" 
		       (file-name-directory (buffer-file-name)))))
      (save-window-excursion
	(with-temp-buffer
	  (insert txt)
	  (py-execute-buffer))))))

;;;_* ===== iPython =====

(when (or t (file-exists-p (concat local-packages "ipython"))) ;; using archived
  (add-to-list 'load-path (concat local-packages "ipython"))
  (require 'ipython)
  ;; (setq ipython-command "/Library/Frameworks/Python.framework/Versions/2.7/bin/ipython")
  ;; (setq ipython-command "/usr/local/bin/ipython")
  ;; (setq ipython-command "ipython")
  ;; (setq py-python-command-args '("--pylab"))

;;;_ . clear screen

  (defun comint-clear-screen ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

;;;_ . additions
;; from 
;; http://mail.scipy.org/pipermail/ipython-user/2008-September/005791.html

;; (require 'comint)
;; (define-key comint-mode-map [(meta p)]
;;    'comint-previous-matching-input-from-input)
;; (define-key comint-mode-map [(meta n)]
;;    'comint-next-matching-input-from-input)
;; (define-key comint-mode-map [(control meta n)]
;;     'comint-next-input)
;; (define-key comint-mode-map [(control meta p)]
;;     'comint-previous-input)

;; (setq comint-completion-autolist t	;list possibilities on partial
;; 					;completion
;;        comint-completion-recexact nil	;use shortest compl. if
;; 					;characters cannot be added
;;        ;; how many history items are stored in comint-buffers (e.g. py- 
;; shell)
;;        ;; use the HISTSIZE environment variable that shells use (if  
;; avail.)
;;        ;; (default is 32)
;;        comint-input-ring-size (string-to-number (or (getenv  
;; "HISTSIZE") "100")))

;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))


;; from 
;; http://mail.python.org/pipermail/python-mode/2011-January/000888.html

;; (setq ipython-completion-command-string
;;       "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n")

)

;;;_* ===== Pylookup =====

;; ;;;_ . load package
;; ;; add pylookup to your loadpath, ex) "~/.lisp/addons/pylookup"
;; (setq pylookup-dir "~/lisp/local-packages/pylookup")
;; (add-to-list 'load-path pylookup-dir)
;; ;; load pylookup when compile time
;; (eval-when-compile (require 'pylookup))

;; ;; set executable file and db file
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; ;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)
;; (autoload 'pylookup-update "pylookup" 
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)

;; ;;;_ . customizations

;; (require 'w3m)
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (global-set-key "\C-ch" 'pylookup-lookup)

;;;_* ===== R/ESS =====
(when (file-exists-p (concat local-packages "ess"))
  (add-to-list 'load-path (concat local-packages "ess/lisp"))
  ;; (global-set-key (kbd "C-c R") 'my-start-R-ESS);'my-ess-start-R)

;;;_ . recommended
  ;;
  (require 'ess-site)
  ;; (require 'ess-eldoc)
  (setq-default inferior-R-args "--no-restore-history --no-save ")
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq ansi-color-for-comint-mode 'filter)
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  (autoload 'ess-rdired "ess-rdired" "View R objects in a dired-like buffer." t)
  ;; (setq-default ess-default-style 'C++)
  ;; (setq inferior-ess-r-help-command "utils::help(\"%s\", help_type=\"html\")\n") 

  (if (eq system-type 'darwin)
      (setq inferior-R-args "--arch x86_64"))

;;;_ . suppress printing of sent commands for speedup

  (setq ess-eval-visibly-p nil) ;; from http://www.damtp.cam.ac.uk/user/sje30/ess11

  (if (not ess-eval-visibly-p)
      (defun inferior-ess-output-filter (proc string)
	"print newline after each evaluation when ess-eval-visibly-p is true
works only with R
from http://old.nabble.com/cat-a-%22%5Cn%22-when-ess-eval-visibly-p-is-nil--td32684429.html"
	(let ((pbuf (process-buffer proc))
	      (pmark (process-mark proc))
	      (prompt-regexp "^>\\( [>+]\\)*\\( \\)$")
	      (prompt-replace-regexp "^>\\( [>+]\\)*\\( \\)[^>+\n]"))
	  (setq string (replace-regexp-in-string prompt-replace-regexp " \n"
						 string nil nil 2))
	  (with-current-buffer pbuf
	    (goto-char pmark)
	    (beginning-of-line)
	    (when (looking-at prompt-regexp)
	      (goto-char pmark)
	      (insert "\n")
	      (set-marker pmark (point)))
	    ))
	(comint-output-filter proc (inferior-ess-strip-ctrl-g string))))

;;;_ . functions

  (load "ess-hacks")

;;;_ . hooks
  (add-hook 'ess-mode-hook
	    '(lambda()
	       (local-set-key (kbd "<C-return>") 'ess-eval-region)
	       (local-set-key (kbd "C-c d") 'ess-rdired)
	       (local-set-key (kbd "C-c 9") 'add-column-offset)))

  (add-hook 'inferior-ess-mode-hook
	    '(lambda()
	       (local-set-key [C-up] 'comint-previous-input)
	       (local-set-key [C-down] 'comint-next-input)))

;;;_* ===== Sweave =====
  ;; http://www.mail-archive.com/auctex@gnu.org/msg03386.html
  (add-hook 'Rnw-mode-hook
	    (lambda ()
	      (add-to-list 'TeX-command-list
			   '(;;"Sweave" "/usr/bin/R --no-save < %s"
			     "Sweave" "R CMD BATCH --no-save %s /dev/tty"
			     TeX-run-command nil (latex-mode) :help "Run Sweave") t)
	      (add-to-list 'TeX-command-list
			   '("LatexSweave" "%l %(mode) \\input{%s}"
			     TeX-run-TeX nil (latex-mode) :help "Run Latex after Sweave") t)
	      (setq TeX-command-default "Sweave")))

  (add-hook 'noweb-minor-mode-hook 
	    '(lambda () 
	       (scroll-conservatively 10000)
	       (visual-line-mode 1))))

;;;_* ===== Elscreen ====
(when (and (concat local-packages "apel")
	   (concat local-packages "elscreen"))
  (add-to-list 'load-path (concat local-packages "apel"))
  (add-to-list 'load-path (concat local-packages "elscreen"))
  (setq dir nil line nil column nil)
  (load "elscreen" "ElScreen" t)
  ;; F9 creates a new elscreen, shift-F9 kills it <f8>

;;;_ --- key bindings ---
  (global-set-key (kbd "<f6>"  ) 'elscreen-reset)
  (global-set-key (kbd "<f7>") 'elscreen-next)
  (global-set-key (kbd "S-<f7>") 'elscreen-previous)
  (global-set-key (kbd "<f8>"    ) 'elscreen-create)
  (global-set-key (kbd "S-<f8>"  ) 'elscreen-kill)
  (define-key elscreen-map "f" 'elscreen-find-file)
  (define-key elscreen-map "r" 'elscreen-reset)
  (define-key elscreen-map "l" 'elscreen-create)
  (global-set-key (kbd "S-C-<left>") 'elscreen-previous)
  (global-set-key (kbd "S-C-<right>") 'elscreen-right)

;;;_ ... functions ---
  (defun elscreen-reset ()
    "Cycles through screens so that window configurations are reset (prevents flashing from redraw-frame[?] after each keystroke)"
  (interactive)
  ;; only happens when number of screens > 1
  (when (> (elscreen-get-number-of-screens) 1)
    (let* ((screen-list (elscreen-get-screen-list))
	   (current-screen (elscreen-get-current-screen))
	   (screen-seq (append (delq current-screen screen-list)
			       (list current-screen))))
      (mapc 'elscreen-goto screen-seq))))

(defadvice set-frame-size (after elscreen-set-frame-size activate)
  (elscreen-reset))

;; (defadvice toggle-fullscreen (after elscreen-toggle-fullscreen activate)
;;   (elscreen-reset))

;; (let ()
;;   (ad-disable-advice 'set-frame-size 'after 'elscreen-set-frame-size)
;;   (ad-activate 'set-frame-size))
)
;;;_* ===== Matlab =====

(when (file-exists-p (concat local-packages "matlab-emacs"))
;;;_ . load-path
  ;; (add-to-list 'load-path 
  ;; 	     "/Applications/MATLAB_R2010a.app/java/extern/EmacsLink/lisp" t)
  (add-to-list 'load-path 
	       (concat local-packages "matlab-emacs") t)

;;;_ . my functions

  (defun matlab-shell-execute-line ()
    (interactive)
    (save-excursion
      (end-of-line)
      (push-mark (point))
      (beginning-of-line)
      (exchange-point-and-mark)
      (matlab-shell-run-region (mark) (point))))

  (defun matlab-shell-open ()
    (interactive)
    (flet ((is-wide-p (thres)
		      (> (frame-width) thres)))
      (let ((this-buffer (buffer-name))
	    (maxwidth 160))
	;; function body:
	;; delete other windows
	(if (is-wide-p maxwidth)
	    (delete-other-windows)
	  (condition-case nil
	      (delete-other-windows-vertically)
	    (error (delete-other-windows))))
	;; shell
	(matlab-shell)
	;; split window between script and inferior shell
	(if (is-wide-p maxwidth)
	    (split-window-horizontally)
	  (split-window-vertically))
	(switch-to-buffer this-buffer)
	(enlarge-window 10))))

  (add-hook 'matlab-mode-hook 
	    (lambda ()
	      (local-set-key (kbd "C-c R") 'matlab-shell-open)
	      (local-set-key (kbd "C-c C-j") 'matlab-shell-execute-line)
	      (local-set-key (kbd "<C-return>") 'matlab-shell-run-region)))

;;;_ . everything else

  ;; http://www.andrew.cmu.edu/course/16-720/extras/matlab_in_emacs/
  (require 'matlab-load)
  (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
  (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
  (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)

  (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
  (defun my-matlab-mode-hook ()
    (setq fill-column 76))		; where auto-fill should wrap
  ;; (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
  ;; (defun my-matlab-shell-mode-hook ()
  ;;   '())
  ;; (add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)

  ;;(setq matlab-shell-command "/Applications/MATLAB_R2008a/bin/matlab")
  (setq matlab-shell-command (if (eq system-type 'darwin)
				 (format "/Applications/%s/bin/matlab"
					 (car (remove-if-not 
					       (lambda (x) (string-match "MATLAB" x)) 
					       (directory-files "/Applications"))))
			       (if (eq system-type 'gnu/linux)
				   "/usr/local/bin/matlab")))
  (setq matlab-shell-command-switches '("-nojvm" "-nosplash"))

  ;; defadvice is awesome
  (defadvice matlab-shell-run-region
    (before matlab-shell-run-region-last-point activate)
    "Deactivate mark before executing region 
   (region is preserved after mark is deactivated)"
    (deactivate-mark)
    )

  ;; (matlab-cedet-setup)
  ;; http://www.mathworks.de/matlabcentral/newsreader/view_thread/160303
  (autoload 'matlab-eei-connect "matlab-eei"
    "Connects Emacs to MATLAB's external editor interface.")

  (setq matlab-verify-on-save-flag nil)	; turn off auto-verify on save
  ;; using the function keys to control matlab debugging.
  (defun my-matlab-mode-hook ()
    (define-key matlab-mode-map [f5] 'matlab-eei-run-continue)
    (define-key matlab-mode-map [f9] 'matlab-shell-run-region)
    (define-key matlab-mode-map [f10] 'matlab-eei-step)
    (define-key matlab-mode-map [f11] 'matlab-eei-step-in)
    (define-key matlab-mode-map [f12] 'matlab-eei-breakpoint-set-clear)
    (define-key matlab-mode-map [f1] ' matlab-eei-exit-debug)
    (setq fill-column 76)
    (imenu-add-to-menubar "Find"))	; where auto-fill should wrap
  (add-hook 'matlab-mode-hook 'my-matlab-mode-hook))

;;;_* ===== Emacs Color Theme =====

(when (file-exists-p (concat local-packages "color-theme"))
  ;; commented out for aquamacs
  (add-to-list 'load-path (concat local-packages "color-theme"))
  (require 'color-theme)
  (setq color-theme-load-all-themes nil)
  (color-theme-initialize))

;;;_* ===== Undo-tree-mode =====

;;;_ . load
;; (require 'undo-tree)

;;;_* ===== CEDET + ECB =====
;;(add-to-list 'load-path (concat local-packages "cedet"))
;;(add-to-list 'load-path (concat local-packages "ecb"))

;;; ========================================================
;;; CEDET
;; See cedet/common/cedet.info for configuration details.
;; (load-file "~/elisp/cedet/common/cedet.el")

;; Enable EDE (Project Management) features
;; (global-ede-mode 1)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;; (semantic-load-enable-code-helpers)

;;;
;; (require 'ecb-autoloads)
;;; ========================================================
;;;


;;;_* ===== AUCTeX =====
(when (file-exists-p (concat local-packages "auctex"))
  (add-to-list 'load-path (concat local-packages "auctex"))
  (add-to-list 'load-path (concat local-packages "auctex/preview"))

;;;_ . AUCTeX

  ;; Load AUCTeX and preview-latex.
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)

  ;; Minimal OS X-friendly configuration of AUCTeX. Since there is no
  ;; DVI viewer for the platform, use pdftex/pdflatex by default for
  ;; compilation. Furthermore, use 'open' to view the resulting PDF.
  ;; Until Preview learns to refresh automatically on file updates, Skim
  ;; (http://skim-app.sourceforge.net) is a nice PDF viewer.
  (setq TeX-PDF-mode t)
  (setq TeX-output-view-style
	'(("^dvi$" "^xdvi$" "xdvi %dS %d")
	  ("^dvi$" "." "open %dS %d")
	  ;;("^pdf$" "." "open %o")
	  ("^pdf$" "." "open -a \"Preview\" %o")
	  ("^html?$" "." "open %o")))

  ;; Add standard Sweave file extensions to the list of files recognized
  ;; by AUCTeX.
  (setq TeX-file-extensions
	'("Snw" "Rnw" "snw" "rnw" "tex" "sty" "cls" "ltx" "texi" "texinfo" "dtx"))


;;;_ . universal

  (load "latex-hacks-st")

;;;_ . hooks
  (setq TeX-mode-hook '(lambda ()
			 (local-set-key (kbd "C-c e") 
					(LaTeX-enclose-expression "$"))
			 (local-set-key (kbd "C-c r")
					'LaTeX-wrap-environment-around-thing-or-region)
			 (local-set-key (kbd "C-c j") 
					'LaTeX-insert-item-no-newline)))
  ;; (local-set-key (kbd "M-[ t") 'hide-body)
  ;; (local-set-key (kbd "M-[ a") 'show-all)))

  ;; (setq PDFLaTeX-mode-hook '(lambda ()
  ;;       (local-set-key (kbd "C-c e") 
  ;; 		     (LaTeX-enclose-expression "$"))
  ;;       (local-set-key (kbd "C-c r")
  ;; 		     'LaTeX-wrap-environment-around-thing-or-region)
  ;;       (local-set-key (kbd "C-c j") 
  ;; 		     'LaTeX-insert-item-no-newline)))
)
;;;_* ===== w3m and Google Apps: blogger, calendar... =====

(when (file-exists-p (concat local-packages "emacs-w3m"))
  ;; http://www.emacswiki.org/emacs/emacs-w3m#toc13
  ;; % cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot login
  ;; CVS password: # No password is set.  Just hit Enter/Return key.
  ;; % cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m
  ;; C-u 0 M-x byte-recompile-directory

  ;; (setq warning-suppress-types nil)
  ;; uncomment for aquamacs
  (add-to-list 'load-path (concat local-packages "emacs-w3m"))
  (add-to-list 'load-path (concat local-packages "emacspeak/lisp/g-client"))
  ;; (load "gblogger.el") ;; no longer exists?

  ;; st
  (load "websc.el")
  ;; http://web.mit.edu/nelhage/Public/dot-elisp/w3m/w3m-e23.el
  ;; http://stuff.mit.edu/afs/sipb/contrib/emacs/packages/emacs-w3m-1.4.4/w3m-load.el

  ;; (defun charset-id (arg) 
  ;;   nil)
  ;; (require 'w3m-e21)
  ;; (provide 'w3m-e23)

  ;; (if (= emacs-major-version 23)
  ;;     (progn
  ;;       (require 'w3m-load)
  ;;       (require 'w3m-ems))
  ;;   (require 'w3m))

  (require 'w3m-load)
  (require 'w3m-ems))

;;;_* ===== nXML mode and html functions =====
(when (file-exists-p (concat local-packages "nxml-mode"))
  (add-to-list 'load-path (concat local-packages "nxml-mode"))
  ;; (load "~/.emacs.d/nxml-mode-20041004/rng-auto.el")
  ;; (global-set-key (kbd "C-c C-w") 'w3m-goto-url-new-session)
					; for xml files, use nxml-mode instead of sgml-mode
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
  (load "htmlize")
  (load "htmlize-xahlee")
  (setq htmlize-convert-nonascii-to-entities nil)
  (setq htmlize-html-charset "utf-8"))


;;;_* ===== Vim emulation =====
;;;_ . --- Viper-vimpulse ---

;; (when (file-exists-p (concat local-packages "vimpulse/vimpulse.el"))
;;   (add-to-list 'load-path (concat local-packages "vimpulse"))
;;   ;; (add-hook 'viper-mode-hook (lambda () (require 'vimpulse)))
;;   (defun vimpulse-on ()
;;     (interactive)
;;     (require 'vimpulse)))
    ;; (vimpulse-imap "\C-o" 'viper-escape-to-vi)))

;;;_ . --- Evil-mode ---

(when (file-exists-p (concat local-packages "evil"))
;;;_  : keybindings/toggle
  ;; (global-set-key (kbd "S-<f6>") 'evil-mode-toggle)
  (defun evil-mode-toggle ()
    (interactive)
    (if evil-mode
	(progn
	  (evil-mode -1)
	  (global-undo-tree-mode -1)
	  (show-paren-mode 1))
      (evil-mode 1)))

;;;_  : load
  ;; Evil requires undo-tree.el in the load-path for linear undo and undo branches.
  ;; undo-tree.el is in ~/.emacs.d/contributed/
  (add-to-list 'load-path (concat local-packages "evil"))
  (require 'undo-tree)
  (require 'evil)
  (global-undo-tree-mode -1)
  (evil-mode -1)

;;;_  : visual-line-mode
(defun evil-follow-emacs-visual-line ()
  (interactive)
  (define-key evil-motion-state-map "j" #'evil-next-visual-line)
  (define-key evil-motion-state-map "k" #'evil-previous-visual-line)
  (define-key evil-motion-state-map "$" #'evil-end-of-visual-line)
  (define-key evil-motion-state-map "^" #'evil-first-non-blank-of-visual-line)
  (define-key evil-motion-state-map "0" #'evil-beginning-of-visual-line))

(add-hook 'evil-mode 
	  (lambda ()
	    (when visual-line-mode
	      (evil-follow-emacs-visual-line)))))

;;;_* ===== Google Weather =====

(when (file-exists-p (concat local-packages "google-weather-el/google-weather.el"))
  (add-to-list 'load-path (concat local-packages "google-weather-el"))
  (require 'google-weather))

;;;_* ===== Multi-term=====

;;Elscreen has to be loaded first, or will have conflicts(?)
;;http://emacs-fu.blogspot.com/2009/07/keeping-related-buffers-together-with.html
;;comments section

;;;_ . load

(require 'multi-term)
(setq multi-term-program "/bin/bash")
;; (setq term-term-name "xterm-color")
(defalias 'mterm 'multi-term)
(defalias 'mtn 'multi-term-next)

;;;_ . custom set 1

(defun term-mode-change-directory ())
(fset 'term-mode-change-directory (shell-mode-change-directory 'term-mode))

;;http://code.google.com/p/dea/source/browse/trunk/my-lisps/multi-term-settings.el
(defun term-my-send-kill-line ()
  "Kill line in term mode."
  (interactive)
  (call-interactively 'kill-line)
  (term-send-raw-string "\C-k"))

(defun term-my-send-yank ()
  "Yank in term mode."
  (interactive)
  (yank)
  (term-send-raw-string (current-kill 0)))

(defun term-my-send-backward-kill-word ()
  (interactive)
  (term-send-raw-string "\e\C-H"))

(defun term-my-send-undo ()
  (interactive)
  (term-send-raw-string "\C-_"))

(let ((termkeys '(("M-DEL"		.	term-my-send-backward-kill-word)
		  ("C-<backspace>"	.	term-my-send-backward-kill-word)
		  ("M-d"		.	term-send-forward-kill-word)
		  ("C-c C-j"		.	term-line-mode)
		  ("C-c C-k"		.	term-char-mode)
		  ("C-c C-c"		.	term-stop-subjob)
		  ("C-x u"              .       term-my-send-undo)
		  ;; ("C-z"             . term-stop-subjob)
		  ("C-k" 		.       term-my-send-kill-line) ;; was (term-send-raw) by default
		  ("C-y" 		.       term-my-send-yank)
		  ("C-c c"		.	term-mode-change-directory))))
  (dolist (elem termkeys nil)
    (add-to-list 'term-bind-key-alist elem)))

;;;_ . custom set 2

;;http://www.reddit.com/r/emacs/comments/mdxdn/help_me_make_multiterm_more_like_shell_or_eshell/

;; (eval-after-load "multi-term"
;;   (progn
;;     (defun term-send-quote ()
;;       (interactive)
;;       (term-send-raw-string "\C-v"))

;;     (defun term-send-M-x ()
;;       (interactive)
;;       (term-send-raw-string "\ex"))

;;     (defun term-send-backward-kill-word ()
;;       (interactive)
;;       (term-send-raw-string "\C-H"))

;;     (dolist
;; 	(bind '(("C-<right>"     . term-send-forward-word)
;; 		("C-<left>"      . term-send-backward-word)
;; 		("C-<backspace>" . term-send-backward-kill-word)
;; 		("C-<delete>"    . term-send-forward-kill-word)
;; 		("C-k"           . term-send-raw)
;; 		("C-y"           . term-send-raw)
;; 		("C-c C-z"       . term-stop-subjob)
;; 		("C-z"           . term-stop-subjob)))
;;       (add-to-list 'term-bind-key-alist bind))))

;;;_ . customize
;; (custom-set-variables
;;  '(term-default-bg-color "#000000")        ;; background color (black)
;;  '(term-default-fg-color "#dddd00"))       ;; foreground color (yellow)

;;;_* ===== code-folding =====

;;;_ . --- outline-mode ---

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

(load "outline-mode-customizations-st")

;;;_ . --- folding-mode ---

(load "folding-mode-customizations-st")

;;;_* ===== RTF mode =====

;; (autoload 'rtf-mode "rtf-mode" "RTF mode" t)
;; (add-to-list 'auto-mode-alist
;;   '("\\.rtf$" . rtf-mode))

;;;_* ===== Org-mode ! =====

(add-to-list 'load-path (concat local-packages "org-mode/lisp"))
(add-to-list 'load-path (concat local-packages "org-mode/contrib/lisp"))
(require 'org-install)
(require 'org-latex)
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;;{{{--- original keybindings ---
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)
;;}}}
(global-set-key "\C-c8" 'org-store-link)
(global-set-key "\C-c9" 'org-agenda)
(global-set-key "\C-c0" 'org-iswitchb)

;;;_ . my customizations for export logbook entries
(defvar org-logbook-mode-p nil)
(defvar org-default-vars nil)
(load "org-logbook-mode")
(load "org-latex-hacks")
;; (load "org-latex-hacked")

;;;_ . defaults

(defun org-get-defaults ()
  (let ((vars '(org-agenda-files
	       org-format-latex-options
	       org-export-latex-date-format
	       org-export-latex-image-default-option
	       org-export-latex-classes)))
    (mapcar (lambda (x) (cons x (eval x))) vars)))

(defun org-restore-defaults (default-vars)
  (interactive)
  (mapc (lambda (x) 
	  (set (car x) (cdr x)))
	default-vars))

(setq org-default-vars (org-get-defaults))
;; restore with 
;; (org-restore-defaults org-default-vars)

;;;_ . further customizations
(add-hook 'org-mode-hook
	  '(lambda ()
	     ;;(auto-fill-mode 1)
	     ;;(org-indent-mode t)
	     (local-set-key (kbd "C-c e") 
			    (LaTeX-enclose-expression "\\(" "\\)"))
	     (local-set-key (kbd "C-c r")
			    'LaTeX-wrap-environment-around-thing-or-region)
	     (local-set-key (kbd "C-c s")
			    'org-begin-or-end)
	     (define-key org-mode-map "\M-q" 'fill-paragraph)
	     (local-set-key [(shift f6)] 'org-export-as-html)))

;;;_* ===== tbe-mode (Thunderbird) =====

(require 'tbemail)
(add-hook 'tbemail-mode-hook 'visual-line-mode)

;;;_* ===== ispell =====

;; (when (file-exists-p "/Applications/Emacs.app/Contents/Resources/lisp/textmodes")
;;   (add-to-list 'load-path
;; 	       "/Applications/Emacs.app/Contents/Resources/lisp/textmodes")
;;   (load "ispell"))
;; (setq ispell-program-name "/opt/local/bin/ispell")
;;;_* ===== magit =====

(when (file-exists-p (concat local-packages "magit"))
  (add-to-list 'load-path (concat local-packages "magit"))
  (require 'magit))

;;;_* ===== project navigation manager =====

;; nav-mode is too simple: shows only top-level directory and forces its own window layout. 
;; dirtree doesn't follow directory path of opened buffer and also forces its own window layout. (toggle?)
;; so far sr-speedbar looks good...

;;;_ . nav-mode

;; (when (file-exists-p (concat local-packages "nav-mode"))
;;   (add-to-list 'load-path (concat local-packages "nav-mode"))
;;   (require 'nav)
;;   (nav-disable-overeager-window-splitting)
;;   (global-set-key (kbd "C-c n") 'nav))

;;;_ . dirtree

;; (add-to-list 'load-path (concat local-packages "dirtree"))
;; (autoload 'dirtree "dirtree" "Add directory to tree view" t)

;;;_ . sr-speedbar 

(when (file-exists-p (concat local-packages "sr-speedbar"))
  (add-to-list 'load-path (concat local-packages "sr-speedbar"))
  (require 'sr-speedbar)
  (global-set-key (kbd "C-c n") 'sr-speedbar-toggle))

