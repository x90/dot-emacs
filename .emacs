;; emacs directory on FreeBSD
;; /usr/local/share/emacs/site-lisp
;; emacs directory on OS X
;; /usr/share/emacs/

;; executable file:
;; /Applications/Emacs.app/Contents/MacOS/Emacs

;;;_* ===== Libraries =====
(require 'cl)
(require 'find-lisp)

;;;_* ===== Variables =====

(defvar emacs-root)
(defvar account-username)
(defvar machine-name)
(defvar color-theme-local nil)

;;;_ . --- Machine-specific customizations ---

(load (concat (expand-file-name "~/.emacs.d/") "local-settings.el"))

;;;_ . --- Version number ---

(setq emacs-version-number (string-to-number emacs-version))

;;;_* ===== Paths =====

;; (defvar emacs-root (if (eq system-type 'darwin)
;; 		       (format "/Users/%s/.emacs.d/" account-username)
;; 		     (if (or (eq system-type 'cygwin)
;; 			     (eq system-type 'gnu/linux)
;; 			     (eq system-type 'linux))
;; 			 (format "/home/%s/.emacs.d/" account-username)
;; 		     "c:/emacs/site-lisp/"))
;;  "My home directory — the root of my personal emacs load-path.")

;; ends in slash?
(if (not (equal (substring emacs-root -1) "/"))
    (setq emacs-root (concat (emacs-root "/"))))

(labels ((add-path (p)
	 (add-to-list 'load-path (concat emacs-root p))))
  (let ((folders '("" "lispcode-1" "lispcode-2" "contributed")))
	(mapc 'add-path folders)))

;; add all the elisp subdirectories to my load path
;;{{{

;; (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;     (let* ((my-lisp-dir "~/.emacs.d/aquamacs_edit-modes")
;; 	   (default-directory my-lisp-dir))
;;       (setq load-path (cons my-lisp-dir load-path))
;;       (normal-top-level-add-subdirs-to-load-path)))
;;
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp" t) ;;org-mode

;;}}}

;;;_* ===== Frame =====
;; commented out for aquamacs
(modify-frame-parameters nil '((wait-for-wm . nil)))
(add-to-list 'initial-frame-alist '(left . 55))
(add-to-list 'initial-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(cursor-type . box))

;;(add-to-list 'default-frame-alist '(background-color . "cornsilk"))

;;;_* ===== Settings =====

(if (>= emacs-version-number 23)
    (cua-mode -1)
  (transient-mark-mode t))
(setq delete-active-region nil)
(delete-selection-mode -1)
(normal-erase-is-backspace-mode 0)
(show-paren-mode 1)
(setq ring-bell-function 'ignore)
(blink-cursor-mode -1)
(setq inhibit-splash-screen t)
(setq line-number-mode t)
(setq column-number-mode t)
(mouse-wheel-mode t)			; activate mouse scrolling
(global-font-lock-mode t)		; syntax highlighting
(tool-bar-mode -1)
;; (setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
;; (setq show-paren-delay 0.0)
;; (setq visible-bell t)
;; (require 's-region)
(when (eq system-type 'gnu/linux)
  ;; make emacs use the clipboard
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;;;_ . ===== Backups and autosaves =====

(setq backup-inhibited t)	; disable backup
(setq auto-save-default nil)	; disable auto save
;; (setq make-backup-files nil)

;;;_ . ===== Search highlight =====
(setq search-highlight           t) ; Highlight search object
(setq query-replace-highlight    t) ; Highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse highlighting

;;;_* ===== Global key bindings =====
(setq ns-command-key t)
;; (global-unset-key "%")


;;;_ . --- rebindings ---
;; (global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key (kbd "M-DEL") 'backward-kill-word)

;;;_ . --- my own preference ---

;;;_  : built-ins
;; (global-set-key "\C-x\C-m" 'execute-extended-command) ;; M-x
(global-set-key (kbd "C-c m") 'execute-extended-command)
(global-set-key (kbd "C-c &") 'auto-revert-mode)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c f") 'ido-find-file-other-frame)
(global-set-key (kbd "C-c b") 'ido-switch-buffer-other-frame)
(global-set-key (kbd "C-c q") 'delete-other-windows-vertically)

;;;_* ===== Buffer management =====
;;;_ . --- ibuffer ---
(autoload 'ibuffer "ibuffer" "List buffers." t)

;;;_ . --- uniquify ---
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;_* ===== Dired =====

;;;_ . --- dired customizations ---
;; (load "dired-sort-map")
;; (setq dired-use-ls-dired nil)
;; (setq list-directory-brief-switches "-C")
(setq dired-listing-switches "-alhF")

;;;_ . --- use ls-lisp on OS X and Windows ---
(when (or (eq system-type 'darwin))
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;;;_ . --- buffer name ends in slash ---

;; from Trey Jackson

(defun ensure-buffer-name-ends-in-slash ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat name "/") t))))
(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)

;;;_* ===== Shell-mode =====

(require 'shell)
(define-key minibuffer-local-map (kbd "C-i") 'comint-dynamic-complete)
(add-hook 'shell-mode-hook
	  '(lambda()
	     (local-set-key (kbd "C-c p") 'copy-pwd)
	     (local-set-key (kbd "C-c c") 'shell-mode-change-directory)
	     (local-set-key (kbd "C-c d") 'dirs)))

(defun shell-mode-change-directory (&optional arg)
  (interactive "P")
  (insert (concat "cd " (copy-pwd arg))))

(when (eq system-type 'cygwin)
  (defun cygwin-shell ()
    "Run cygwin bash in shell mode."
    (interactive)
    (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
      (call-interactively 'shell)))
  (add-to-list 'explicit-bash-args "--login"))

;;;_* ===== Term-mode =====

;; (defadvice ansi-term
;;   (before ansi-term-run-bash activate)
;;   (ad-set-arg 0 "/bin/bash"))

(add-hook 'term-mode-hook '(lambda ()
			     (local-set-key (kbd "C-c C-j") 'term-line-mode)
			     (local-set-key (kbd "C-c C-k") 'term-char-mode)))
(defun ash-term-hooks ()
  (setq term-default-bg-color (face-background 'default))
  (setq term-default-fg-color (face-foreground 'default)))
(defun term-colors-emacswiki ()
  (setq term-default-bg-color (face-background 'default))
  (setq term-default-fg-color "#dddd00"))
;; (add-hook 'term-mode-hook 'ash-term-hooks)
(add-hook 'term-mode-hook 'term-colors-emacswiki)

;;;_* ===== Winner mode =====

(require 'winner)
(setq winner-dont-bind-my-keys t) ;; default bindings conflict with org-mode
(winner-mode t) ;; turn on the global minor mode

;;;_* ===== ido mode =====


(if (>= emacs-version-number 22)
    (progn
      (require 'ido)
      (ido-mode t)
      ;; (setq confirm-nonexistent-file-or-buffer nil)
      (add-hook 'ibuffer-mode-hook
		(lambda ()
		  (define-key ibuffer-mode-map [remap ibuffer-visit-buffer] 
		    'ibuffer-like-ido-visit-buffer)))
      ;; make ibuffer like ido-mode
      (defun ibuffer-like-ido-visit-buffer (&optional single)
	"Visit the buffer on this line.
If optional argument SINGLE is non-nil, then also ensure there is only
one window."
	(interactive "P")
	(let ((buf (ibuffer-current-buffer t)))
	  (bury-buffer (current-buffer))
	  (ido-visit-buffer buf ido-default-buffer-method)
	  (when single
	    (delete-other-windows)))))
  (progn
    (iswitchb-mode t)
    (icomplete-mode t)))

;;;_* ===== Line wrapping =====

(if (>= emacs-version-number 23)
    (progn
      ;; Emacs 23.1
      ;; Visual-line-mode
      (global-visual-line-mode -1)
      (add-hook 'text-mode-hook '(lambda () (visual-line-mode 1)))
      (add-hook 'LaTeX-mode-hook '(lambda () (visual-line-mode 1)))
      (add-hook 'PDFLaTeX-mode-hook '(lambda () (visual-line-mode 1))))
  (progn
    ;; Enable longlines mode
    (add-hook 'text-mode-hook 'longlines-mode)
    (add-hook 'latex-mode-hook 'longlines-mode)
    (add-hook 'latex-mode-hook
	      '(lambda () (setq longlines-wrap-follows-window-size t)))))

;;;_* ===== Flyspell mode =====

;; (load "/Applications/Emacs.app/Contents/Resources/lisp/textmodes/ispell")
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;; (add-hook 'message-mode-hook 'turn-on-flyspell)
;; (add-hook 'text-mode-hook 'turn-on-flyspell)
;; (add-hook 'tex-mode-hook 'turn-on-flyspell)
;; (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
;; (add-hook 'tcl-mode-hook 'flyspell-prog-mode)
;; (defun turn-on-flyspell ()
;;    "Force flyspell-mode on using a positive arg.  For use in hooks."
;;    (interactive)
;;    (flyspell-mode 1))

;;;_* ===== Full screen =====

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) 
				 old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
(when (and (eq system-type 'darwin)
	   (>= emacs-version-number 24))
  (require 'ns-platform-support)
  (ns-extended-platform-support-mode 1)
  (fset 'toggle-fullscreen 'ns-toggle-fullscreen))

;;;_* ===== Disabled commands =====

(put 'narrow-to-region 'disabled nil)	; narrowing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;;_* ===== My custom variables =====
;;(defcustom warning-suppress-types nil)
(setq warning-suppress-types nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(ecb-options-version "2.40")
 '(explicit-shell-file-name "/bin/bash")
 '(flyspell-issue-message-flag nil)
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0)
 '(ido-record-commands nil)
 '(ido-save-directory-list-file nil)
 '(indicate-empty-lines t)
 '(nav-resize-frame-p t)
 '(ns-command-modifier (quote control))
 ;; '(org-agenda-files nil)
 ;; '(org-export-latex-date-format "")
 ;; '(org-export-latex-image-default-option "width=20em")
 '(pop-up-windows nil)
 '(py-shell-switch-buffers-on-execute nil)
 '(same-window-buffer-names (quote ("*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*" "*Help*" "*Async Shell Command*" "*grep*" "*rgrep*" "*Directory*")))
 '(warning-suppress-types (quote ((server)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;_* ===== Load extras =====

(load "emacs-local.el")

;;;_* ===== Emacs server start =====
(condition-case nil
    (server-start)
  (error nil))
;; (if (file-exists-p
;;  (concat (getenv "TMPDIR") "emacs"
;;          (number-to-string
;;           (user-real-uid)) "/server"))
;;     nil 
;;   (server-start))
