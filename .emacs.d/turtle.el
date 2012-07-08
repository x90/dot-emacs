;;;_* ===== Paths =====
;;(defvar emacs-root "~/.emacs.d/")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")

;;;_* ===== Environment variables =====

(mapc (lambda (x)
        (setenv (car x) (cdr x)))
      (let ((elem (cons "UCSD" (concat (file-name-as-directory (getenv "HOME"))
                                       "Documents/Work/UCSD"))))
        (append (list elem)
                '(("stelguapo" . "stakahama@elguapo.ucsd.edu")
                  ("stkanaloa" . "st@kanaloa.ucsd.edu")
                  ("taccranger" . "tg804401@tg-login.ranger.tacc.teragrid.org")
                  ("taccwork" . "/work/01159/tg804401")
                  ("triton" . "stakahama@triton-login.sdsc.edu")))))

;;;_* ===== Clojure =====
;; (require 'clojure-mode)
;; (setq swank-clojure-extra-classpaths '())
;; (clojure-slime-config)
;; (setq swank-clojure-extra-classpaths
;;       (cons "~/src/incanter/incanter.jar"
;; 	    (directory-files "~/src/incanter/lib" t ".jar$")))

;;;_* ===== Scheme =====
;; (add-to-list 'load-path "~/.emacs.d/scheme")

;; (setq mzscheme-program "~/bin/mzscheme")
;; (global-set-key [(f5)]
;; 		'(lambda ()
;; 		   (interactive)
;; 		   (require 'quack)
;; 		   (run-scheme mzscheme-program)))

;;;_* ===== Haskell =====
;; (load "haskell-site-file")
;; ;;  adding the following lines according to which modules you want to use:
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'font-lock-mode)
;; (add-hook 'haskell-mode-hook
;;    (function
;;     (lambda ()
;;       (setq haskell-program-name "ghci"))))

;;;_* ===== ebib =====

;; (add-to-list 'load-path "~/elisp/ebib")
;; (autoload ’ebib "ebib" "Ebib, a BibTeX database manager." t)
;; (global-set-key "\C-ce" 'ebib)

; (set-buffer-file-coding-system 'unix) ; or 'mac or 'dos

;;;_* ===== Thunderbird =====
(require 'tbemail)

;;;_* ===== Lilypond-mode =====

(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

;;;_* ===== SVN =====

;;;
;;; SVN
;;;
;; Support for the Subversion version control system. Use 'M-x
;; svn-status RET' on a directory under version control to update,
;; commit changes, revert files, etc., all within Emacs.
;; (add-to-list 'vc-handled-backends 'SVN)
;; (require 'psvn)
;; (add-hook 'svn-log-edit-mode-hook 'turn-off-auto-fill) ; useful option

;;;
;;; Fix path
;;;
;; Emacs does not properly catch the system and user paths at launch
;; on OS X. There are ways to solve this provided with Emacs.app
;; (mac-fix-env and ns-grabenv), but I have not been very successful
;; in using them so far. For the time being, I rely on code lifted
;; from Aquamacs.
;; (if window-system (ns-grabenv "/bin/bash"
;; 			      "source ~/.bashrc"))
;; (require 'fixpath) ; uncomment if not Aquamacs

