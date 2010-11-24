;;; .emacs - an emacs initialization file created by Bill Clementson
(require 'cl)

;;__________________________________________________________________________
;;;;    Site-Specific Variables 

;; See if we're on MS Windows or some other OS
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))

;; Some file locations are relative to the HOME or the BIN directory
(defvar use-home)
(setq use-home (concat (expand-file-name "~") "/"))
(defvar use-bin
  (if mswindows-p
      "c:/bin/"
    (concat (expand-file-name "~") "/bin/")))

;; Setup for PLT Scheme
(defvar plt-dir (concat use-bin "plt/"))
(defvar mzscheme-program (concat plt-dir "mzscheme.exe"))
(setenv "path" (concat plt-dir ";" (getenv "path")))
(setenv "PLTHOME" plt-dir)
(setenv "PLTCOLLECTS" (concat plt-dir "collects"))
(if mswindows-p
    (progn
      (setenv "HOMEDRIVE" (subseq use-home 0 2))
      (setenv "HOMEPATH" (subseq use-home 2))))
(setq quack-pltcollect-dirs (directory-files (concat plt-dir "collects") t))

;; Set up load path 
(setq load-path (append (list (concat use-home "")
			      (concat use-home "site/ecb")
			      (concat use-home "site/eieio")
			      (concat use-home "site/semantic")
			      (concat use-home "site/speedbar")
			      (concat use-home "site"))
                        load-path))

;; Specify where backup files are stored
(setq backup-directory-alist (quote ((".*" . "~/.backups"))))

;; Location of Info documentation
(setq-default Info-default-directory-list
	      (list (expand-file-name (concat use-home "info"))
		    (expand-file-name (concat (getenv "EMACS_DIR") "/info"))))

;;__________________________________________________________________________
;;;;    Initial Code Load

(require 'dired)
(require 'font-lock)
(require 'lazy-lock)
(require 'recentf)
(require 'mouse-sel)
(require 'hippie-exp)
(require 'browse-url)
(require 'comint)
(ignore-errors (require 'color-theme))
(ignore-errors (require 'ecb))

;;__________________________________________________________________________
;;;;    System Customizations 

;; Set buffer behaviour
(setq next-line-add-newlines nil)
(setq scroll-step 1)
(setq scroll-conservatively 5)

;; Enable emacs functionality that is disabled by default
(put 'eval-expression 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq enable-recursive-minibuffers t)

;; Misc customizations
(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s by y
(setq inhibit-startup-message t)        ;no splash screen
(defconst use-backup-dir t)             ;use backup directory
(defconst query-replace-highlight t)    ;highlight during query
(defconst search-highlight t)           ;highlight incremental search
(setq ls-lisp-dirs-first t)             ;display dirs first in dired
(global-font-lock-mode t)               ;colorize all buffers
(setq ecb-tip-of-the-day nil)           ;turn off ECB tips
(recentf-mode 1)                        ;recently edited files in menu

;; Conventional mouse/arrow movement & selection
(pc-selection-mode)                 
(delete-selection-mode t)           

(defun maximize-frame (&optional frame)
  "Maximize the selected FRAME."
  (interactive)
  (or frame
      (setq frame (selected-frame)))
  (let ((pixels-per-col (/ (float (frame-pixel-width))
			   (frame-width)))
	(pixels-per-row (/ (float
			    (frame-pixel-height)) (frame-height))))
    (set-frame-size frame
		    (if (string= "w32" window-system)
			(+ (truncate (/ (x-display-pixel-width) pixels-per-col)) 2)
		      (truncate (/ (x-display-pixel-width) pixels-per-col)))
		    (if (string= "w32" window-system)
			(- (truncate (/ (x-display-pixel-height) pixels-per-row)) 2)
		      (- (truncate (/ (x-display-pixel-height) pixels-per-row)) 7)))
    (set-frame-position frame 0 0)))

(tool-bar-mode -1)

(if (string= "w32" window-system)
    (if (string-match "GNU Emacs 21.3.1" (emacs-version))
	(w32-send-sys-command 61488)
      (maximize-frame))
  (maximize-frame))

;; Use ESC to exit popups
(defvar running-xemacs (if (string-match "XEmacs" emacs-version) t nil))
(defvar running-fsf    (and (not running-xemacs)
                            (string-match "^\\([0-9]+\\)\\." emacs-version)
                            (string-to-number (match-string 1 emacs-version))))
(defvar running-fsf21  (and running-fsf (>= running-fsf 21)))

(ignore-errors
  (load-library "electric-not.el"))

;; Always re-indent the top-level sexp
(defadvice indent-sexp (around indent-defun (&optional endpos))
  "Indent the enclosing defun (or top-level sexp)."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    ad-do-it))

(ad-activate 'indent-sexp)

;; Ediff customizations
(defconst ediff-ignore-similar-regions t)
(defconst ediff-use-last-dir t)
(defconst ediff-diff-options " -b ")

;; Dired customizations
(setq dired-listing-switches "-l")

(defun dired-mouse-find-file (event)
  "In dired, visit the file or directory name you double-click on (EVENT)."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

(defun my-dired-find-file ()
  "In dired, visit the file or directory name you are on (in the same window)."
  (interactive)
  (let (file)
    (save-excursion
      (setq file (dired-get-filename))
      (find-file (file-name-sans-versions file t)))))

(add-hook 'dired-mode-hook
	  '(lambda()
	     (define-key dired-mode-map [delete] 'dired-do-delete)
	     (define-key dired-mode-map [C-return] 'dired-find-file-other-window)
	     (define-key dired-mode-map [C-down-mouse-1] 'mouse-buffer-menu)
	     (define-key dired-mode-map [double-down-mouse-1] 'dired-mouse-find-file)	     
	     (define-key dired-mode-map [return] 'my-dired-find-file)))

;; Word completion customizations
(defconst dabbrev-always-check-other-buffers t)
(defconst dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
	try-expand-whole-kill))

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Column & line numbers in mode bar
(column-number-mode t)
(line-number-mode t)	

;; Code display options (highlight parens & colorize)
(show-paren-mode 1)
(setq font-lock-support-mode 'lazy-lock-mode)

;; Font lock colorization customizations
(defun color-theme-billc ()
  "Bill Clementson's custom color theme."
  (interactive)
  (color-theme-install
   '(color-theme-billc
     ((foreground-color . "black")
      (background-color . "white")
      (mouse-color . "sienna3")
      (cursor-color . "black")
      (border-color . "Blue")
      (background-mode . light))
     (default ((t (nil))))
     (modeline ((t (:background "dark gray" :foreground "black"))))
     (modeline-buffer-id ((t (:background "dark gray" :foreground "black"))))
     (modeline-mousable ((t (:background "dark gray" :foreground "black"))))
     (modeline-mousable-minor-mode ((t (:background "dark gray" :foreground "black"))))
     (highlight ((t (:foreground "black" :background "darkseagreen2"))))
     (bold ((t (:bold t))))
     (italic ((t (:italic t))))
     (bold-italic ((t (:bold t :italic t))))
     (region ((t (:foreground "black" :background "snow3"))))
     (secondary-selection ((t (:background "paleturquoise"))))
     (underline ((t (:underline t))))
     (lazy-highlight-face ((t (:foreground "dark blue" :bold t))))
     (font-lock-comment-face ((t (:foreground "dark green" :bold t :italic t))))
     (font-lock-string-face ((t (:foreground "SlateGray4" :bold t))))
     (font-lock-keyword-face ((t (:foreground "black" :bold t))))
     (font-lock-builtin-face ((t (:bold t :foreground "black"))))
     (font-lock-function-name-face ((t (:foreground "dark blue" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "black"))))
     (font-lock-type-face ((t (:foreground "blue"))))
     (font-lock-constant-face ((t (:foreground "dark blue"))))
     (font-lock-warning-face ((t (:foreground "red" :bold t))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-button-face ((t (:bold t))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-single-line-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))
     (custom-rogue-face ((t (:foreground "pink" :background "black"))))
     (custom-modified-face ((t (:foreground "white" :background "blue"))))
     (custom-set-face ((t (:foreground "blue" :background "white"))))
     (custom-changed-face ((t (:foreground "white" :background "blue"))))
     (custom-saved-face ((t (:underline t))))
     (custom-button-face ((t (nil))))
     (custom-documentation-face ((t (nil))))
     (custom-state-face ((t (:foreground "dark green"))))
     (custom-variable-tag-face ((t (:foreground "blue" :underline t))))
     (custom-variable-button-face ((t (:bold t :underline t))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face-1 ((t (:foreground "red" :underline t))))
     (custom-group-tag-face ((t (:foreground "blue" :underline t))))
     (speedbar-button-face ((t (:foreground "green4"))))
     (speedbar-file-face ((t (:foreground "cyan4"))))
     (speedbar-directory-face ((t (:foreground "blue4"))))
     (speedbar-tag-face ((t (:foreground "brown"))))
     (speedbar-selected-face ((t (:foreground "red"))))
     (speedbar-highlight-face ((t (:background "green"))))
     (ff-paths-non-existant-file-face ((t (:foreground "NavyBlue" :bold t))))
     (show-paren-match-face ((t (:background "light blue"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "purple")))))))

(ignore-errors
  (color-theme-billc))

;; Set fonts up on MS Windows
(if mswindows-p
    (progn
      ;; Show as much as we can using fonts bundled with IE5
      (setq w32-standard-fontset-spec
	    "-*-Courier New-normal-r-*-*-*-100-*-*-c-*-fontset-courier,
   ascii:-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-1,
   latin-iso8859-1:-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-1,
   latin-iso8859-2:-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-2,
   latin-iso8859-3:-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-3,
   latin-iso8859-4:-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-4,
   latin-iso8859-7:-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-7,
   latin-iso8859-9:-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-9,
   cyrillic-iso8859-5:-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-5,
   greek-iso8859-7:-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-7,
   hebrew-iso8859-8:-*-Rod-normal-r-*-*-*-100-*-*-c-*-iso8859-8,
   ipa:-*-Lucida Sans Unicode-normal-r-*-*-*-100-*-*-c-*-muleipa*-*,
   thai-tis620:-*-Tahoma-normal-r-*-*-*-100-*-*-c-*-tis620-*,
   latin-jisx0201:-*-MS Gothic-normal-r-*-*-*-100-*-*-c-*-jisx0208-sjis,
   katakana-jisx0201:-*-MS Gothic-normal-r-*-*-*-100-*-*-c-*-jisx0208-sjis,
   japanese-jisx0208:-*-MS Gothic-normal-r-*-*-*-100-*-*-c-*-jisx0208-sjis,
   japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-*-100-*-*-c-*-jisx0208-sjis,
   japanese-jisx0212:-*-MS Gothic-normal-r-*-*-*-100-*-*-c-*-jisx0212-sjis,
   korean-ksc5601:-*-Gulim-normal-r-*-*-*-100-*-*-c-*-ksc5601-*,
   chinese-gb2312:-*-MS Song-normal-r-*-*-*-100-*-*-c-*-gb2312-*,
   chinese-big5-1:-*-MingLiU-normal-r-*-*-*-100-*-*-c-*-big5-*,
   chinese-big5-2:-*-MingLiU-normal-r-*-*-*-100-*-*-c-*-big5-*")

      (setq w32-enable-italics t)
      (create-fontset-from-fontset-spec w32-standard-fontset-spec t)

      (setq default-frame-alist '((font . "fontset-courier")))

      (setq initial-frame-alist default-frame-alist)))

;;__________________________________________________________________________
;;;;    Programming - Scheme

;; Specify modes for Scheme file extensions
(setq auto-mode-alist
      (append '(
		("\\.emacs$" . emacs-lisp-mode)
		("\\.scm$" . scheme-mode)
		("\\.ss$" . scheme-mode)
		("\\.sch$" . scheme-mode)
		)auto-mode-alist))

;; Start up Scheme
(global-set-key [(f5)]
		'(lambda ()
		   (interactive)
		   (require 'quack)
		   (run-scheme mzscheme-program)))

;; Scheme Hooks
(add-hook 'scheme-mode-hook
	  (lambda ()
	    (define-key scheme-mode-map [f1]
	      '(lambda ()
		 (interactive)
		 (ignore-errors
		   (let ((symbol (thing-at-point 'symbol)))
		     (info "(r5rs)")
		     (Info-index symbol)))))
	    (mapc (lambda (key-arg)
                    (define-key scheme-mode-map (car key-arg)
                      (eval `(lambda ()
                               (interactive)
                               (-test ,(cadr key-arg))))))
                  '(([(control c) (control m)] nil)
                    ([(control c) (h)]         :this)
                    ([(control c) (e)]         :expand)
                    ([(control c) (o)]         :expand-once)
                    ([(control c) (*)]         :expand*)
                    ([(control c) (p)]         :pp)))
	    (define-key scheme-mode-map [(control c) (x)] 'scheme-send-dwim)
	    (define-key scheme-mode-map [(control c) (\;)] 'insert-balanced-comments)
	    (define-key scheme-mode-map [(control c) (:)] 'remove-balanced-comments)
	    (define-key scheme-mode-map [(control c) (t)]
	      (lambda (prefix)
		(interactive "P")
		(-trace "trace" prefix)))
	    (define-key scheme-mode-map [(control c) (T)]
	      (lambda (prefix)
		(interactive "P")
		(-trace "trace-all" prefix)))
	    (imenu-add-to-menubar "Symbols")
	    (outline-minor-mode)
	    (make-local-variable 'outline-regexp)
	    (setq outline-regexp "^(.*")))

(add-hook 'Info-mode-hook
	  (lambda ()
	    (interactive)
	    (define-key Info-mode-map [(control c) (x)] 'scheme-send-dwim)))

;; Scheme-specific Functions
(defun insert-balanced-comments (arg)
  "Insert a set of balanced comments around the s-expression 
containing the point.  If this command is invoked repeatedly
(without any other command occurring between invocations), the 
comment progressively moves outward over enclosing expressions."
  (interactive "*p")
  (save-excursion
    (when (eq last-command this-command)
      (when (search-backward "#|" nil t)
        (save-excursion
          (delete-char 2)
          (while (and (< (point) (point-max)) (not (looking-at " *|#")))
            (forward-sexp))
          (replace-match ""))))
    (while (> arg 0)
      (backward-char 1)
      (cond ((looking-at ")") (incf arg))
            ((looking-at "(") (decf arg))))
    (insert "#|")
    (forward-sexp)
    (insert "|#")))

(defun remove-balanced-comments ()
  "Remove a set of balanced comments enclosing point."
  (interactive "*")
  (save-excursion
    (when (search-backward "#|" nil t)
      (delete-char 2)
      (while (and (< (point) (point-max)) (not (looking-at " *|#")))
	(forward-sexp))
      (replace-match ""))))

(defun kill-this-buffer-lisp ()
  (interactive)
  (cond
   ((eq (current-buffer) (get-buffer "*scheme*"))
    (let ((process (get-buffer "*scheme*")))
      (comint-snapshot-last-prompt)
      (process-send-string process "(exit)"))
    (sleep-for .1)
    (kill-this-buffer))
   (t (kill-this-buffer))))

(defun kill-all-process-buffers ()
  (mapc (lambda (buffer)
	  (if (get-buffer buffer)
	      (progn
		(pop-to-buffer buffer)
		(kill-this-buffer-lisp))))
	'("*scheme*"))
  (if mswindows-p
      (ignore-errors
	(progn 
	  (require 'gnuserv) 
	  (gnuserv-start t))))) 

(defun scheme-send-dwim (arg)
  "Send the appropriate forms to Scheme to be evaluated."
  (interactive "P")
  (save-excursion
    (cond 
     ;;Region selected - evaluate region
     ((not (equal mark-active nil))
      (scheme-send-region (mark) (point)))
     ;; At/after sexp - evaluate last sexp
     ((or (looking-at "\\s\)")
	  (save-excursion
	    (backward-char 1)
	    (looking-at "\\s\)")))
      (if (looking-at "\\s\)")
	  (forward-char 1)) 
      (scheme-send-last-sexp))
     ;; At/before sexp - evaluate next sexp
     ((or (looking-at "\\s\(")
	  (save-excursion
	    (forward-char 1)
	    (looking-at "\\s\(")))
      (if (looking-at "\\s\(")
	  (forward-list 1)) 
      (scheme-send-last-sexp))
     ;; Default - evaluate enclosing top-level sexp
     (t (scheme-send-definition)))
    (if arg (switch-to-scheme t))))

;; MzScheme Macro expansion
(defvar mzexpand-actions
  '(nil :this :expand :expand-once :expand* :pp))

(defvar mzexpand-cache nil)

(defun mzexpand-get-action ()
  (unless (eq (car mzexpand-cache) mzexpand-actions)
    (setq mzexpand-cache
          (cons mzexpand-actions
                (mapcar (lambda (a)
                          (list (replace-regexp-in-string
                                 "^:" "" (format "%s" a))
                                a))
                        mzexpand-actions))))
  (cdr (assoc (completing-read "Action? " mzexpand-cache nil t)
              (cdr mzexpand-cache))))

(defun -test (action)
  "Scheme syntax debugging. Uses Scheme code originally developed by
Eli Barzilay.  Actions: nil set current using sexp at point
 :this        show current
 :expand      expand current (possibly in a context)
 :expand-once expand one step
 :expand*     expand one step repeatedly
 :pp          pprint current"
  (interactive (mzexpand-get-action))
  (comint-send-string (get-buffer-process "*scheme*")
                      (format "(-test %S)" (or action 
					       (sexp-at-point))))
  (pop-to-buffer "*scheme*" t)
  (other-window 1))

;; MzScheme Trace
(defun -trace (action &optional prefix)
  (interactive)
  (let ((symb nil))
    (if (or (equal action "trace")
	    (equal action "untrace"))
	(setq symb (symbol-at-point)))
    (if prefix
	(setq action (concat "un" action)))
    (comint-send-string (get-buffer-process "*scheme*")
			(if symb
			    (format "(%s %S)" action symb)
			  (format "(%s)" action))))
  (pop-to-buffer "*scheme*" t)
  (other-window 1))

;;__________________________________________________________________________
;;;;    Programming - Elisp

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (interactive)
	     (require 'eldoc)
	     (turn-on-eldoc-mode)
	     (pretty-lambdas)
	     (define-key emacs-lisp-mode-map [(control c) (x)] 'copy-eval-dwim-lisp)
	     ;; Default to auto-indent on Enter
	     (define-key emacs-lisp-mode-map [(control j)] 'newline)
	     (define-key emacs-lisp-mode-map [(control m)] 'newline-and-indent)))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
	  (0 (progn (compose-region (match-beginning 1) (match-end 1)
				    ,(make-char 'greek-iso8859-7 107))
		    nil))))))

;;__________________________________________________________________________
;;;;    Standard Key Overrides

;; Completions in minibuffer 
(define-key minibuffer-local-map [tab] 'comint-dynamic-complete)

;; Mouse 
(global-set-key [down-mouse-2] 'imenu)

;; Disable mouse-2 event that was appending text into documents
(global-set-key [mouse-2] nil)

;; Prevent accidentally killing emacs.
(global-set-key [(control x) (control c)]
		'(lambda ()
		   (interactive)
		   (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ? " 4 nil)
		       (save-buffers-kill-emacs))))

;; Close down Lisp before killing buffer
(global-set-key [f4] 'kill-this-buffer-lisp)

;; Better buffer list.
(global-set-key [(control x) (control b)] 'electric-buffer-list)

;; Common buffer/window control shortcuts
(global-set-key [f6] 'other-window)
(global-set-key [f7] 'delete-other-windows)
(global-set-key [(control f7)] 'ecb-toggle-ecb-windows)
(global-set-key [(meta f7)] 'ecb-toggle-layout)

;; Shells
(global-set-key [f12]
		'(lambda ()
		   (interactive)
		   (eshell)))

(global-set-key [(control f12)]
		'(lambda ()
		   (interactive)
		   (cond
		    (mswindows-p
		     (let ((explicit-shell-file-name
			    (expand-file-name (concat (getenv "EMACS_DIR") "/bin/cmdproxy.exe")))
			   (shell-file-name "cmdproxy.exe"))
		       (shell)))
		    (t (shell)))))

(global-set-key [(meta f12)]
		'(lambda ()
		   (interactive)
		   (let ((explicit-shell-file-name
			  (if mswindows-p
			      "bash.exe"
			    "bash"))
			 (shell-file-name
			  (if mswindows-p
			      "bash.exe"
			    "bash")))
		     (shell))))

;; Shortcuts to common functions
(global-set-key [(control c) (f)] 'find-function-at-point)
(global-set-key [(control c) (F)] 'ffap)

(global-set-key [(control c) (j)] 'join-line)
(global-set-key [(control c) (s)]
		(function
		 (lambda ()
		   (interactive)
		   (let ((arg (thing-at-point 'symbol)))
		     (search-forward arg)))))

(global-set-key [(control c) (r)]
		(function
		 (lambda ()
		  (interactive)
		  (let ((arg (thing-at-point 'symbol)))
		    (search-backward arg)))))

(global-set-key [(control c) (/)] 'hippie-expand)
(global-set-key [(control c) (g)] 'goto-line)	
(global-set-key [(control c) (a)] 'mark-whole-buffer)
  
;;__________________________________________________________________________
;;;;    MS Windows Customizations

;; Note that the cua-emul, gnuserve & cua libraries are optional
(if mswindows-p
    (progn
      ;; Ctrl-tab, Ctrl-F4, etc like Windows
      (ignore-errors
	(progn
	  (require 'cua-emul)
	  (setq cua-emul-force t)
	  (turn-on-cua-emul-mode)))

      ;; Grep equivalent on Windows
      ;;(setq grep-command "c:/cygwin/bin/grep -n -a -e ")
      (setq grep-command "findstr /n /s ")

      ;; Windows Execute from dired
      (define-key dired-mode-map "w"
	(function
	 (lambda ()
	   (interactive)
	   (setq w32-shellex-no-dired-hook t)
	   (require 'w32-shellex)
	   (w32-shellex-dired-on-objects))))

      ;; Start gnuserv on Windows 
      (ignore-errors
	(progn 
	  (require 'gnuserv) 
	  (setq server-done-function 'bury-buffer 
		gnuserv-frame (car (frame-list))) 
	  (gnuserv-start) 
	  ;; Open buffer in existing frame instead of creating new one... 
	  (setq gnuserv-frame (selected-frame)) 
	  (message "gnuserv started.")))

      ;; C-z=Undo, C-c=Copy, C-x=Cut, C-v=Paste
      (ignore-errors
	(progn
	  (if (string-match "GNU Emacs 21.3.1" (emacs-version))
	      (progn
		(ignore-errors (require 'cua))
		(CUA-mode t))
	    (ignore-errors (require 'cua-base))
	    (cua-mode t))))

      ;; Kill any active lisp before killing frame
      (global-set-key [(meta f4)]
		      '(lambda ()
			 (interactive)
			 (kill-all-process-buffers)
			 (cua-emul-kill-frame)))))

;;__________________________________________________________________________
;;;;    Mac OS X Customizations

(if macosx-p
    (progn
      ;; Set some common keys
      (global-set-key [kp-delete] 'delete-char)
      (global-set-key [(control kp-home)] 'beginning-of-buffer)
      (global-set-key [(control kp-end)] 'end-of-buffer)
      ;; Custom code to open browser on Mac OS X
      (setq browse-url-browser-function
	    '(lambda (url &optional new-win)
	       (do-applescript (concat "open location \""
				       url "\""))))))

;;__________________________________________________________________________
;;;;    Start Directory

(find-file "~/")

;;__________________________________________________________________________
;;;;    Customizations

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(max-lisp-eval-depth 10000)
 '(max-specpdl-size 2000)
 '(quack-pretty-lambda-p t)
 '(tool-bar-mode nil nil (tool-bar)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(quack-pltish-comment-face ((((class color) (background light)) (:foreground "green4" :slant italic :weight bold))))
 '(quack-pltish-selfeval-face ((((class color) (background light)) (:foreground "SlateGray4" :weight bold))))
 '(quack-threesemi-semi-face ((((class color) (background light)) (:background "gray88" :foreground "green4" :weight bold))))
 '(quack-threesemi-text-face ((((class color) (background light)) (:background "gray88" :foreground "green4" :weight bold)))))

;; Overrides
(global-set-key [f6] 'other-window)

;; end of emacs.el