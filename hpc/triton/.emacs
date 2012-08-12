;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;; satoshi
(transient-mark-mode t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key (kbd "C-c o") 'other-window-previous)
(global-set-key (kbd "C-c ;") 'comment-region)
(defun other-window-previous
  (interactive)
  (other-window -1))
(defun insert-pwd ()
  (interactive)
  (insert (expand-file-name 
	      (replace-regexp-in-string 
	           "^Directory " "" (pwd))))
)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(iswitchb-mode 1)

