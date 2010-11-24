;;;_ . Initialize

(require 'color-theme)
(setq color-theme-load-all-themes nil)
(color-theme-initialize)

;;;_ . See examples:
;; http://gnuemacscolorthemetest.googlecode.com/svn/html/index-c.html
;; see menu: ‘M-x color-theme-select’

;;;_ . Color-theme choices

;; (color-theme-dark-blue2)
;; (color-theme-euphoria)
;; (color-theme-jonadabian-slate)
;; (color-theme-robin-hood)
;; (color-theme-gnome2)
;; (color-theme-arjen)

;;;_ . Custom choices

;; (color-theme-tango)
;; (color-theme-wombat)
;; (load "zenburn2")
;; (color-theme-zenburn)

;;;_ . Load color theme by frame

;; --- from emacs-fu (superceded by function below) ---
;;{{{
;; (defun test-win-sys (frame)
;;   ;; must be current for local ctheme
;;   (select-frame frame)
;;   ;; test winsystem
;;   (if (window-system frame)
;;       (color-theme-deep-blue)
;;     (color-theme-standard)))
;;}}}

;; select theme - first is for GUI, second is for console
;; (setq color-theme-choices '(color-theme-deep-blue color-theme-standard))
;; ---> set in .emacs
;; test for each frame or console
(defun test-win-sys (&optional frame)
  (let ((color-theme-is-global nil) 
	(win-sys nil)))
    (if frame
	(progn
	  (select-frame frame)
	  (setq win-sys (window-system frame)))
      (setq win-sys (window-system))) ;; must be current for local theme
    ;;(eval (append '(if win-sys) (mapcar 'list color-theme-choices))))
    (if (not win-sys) (cdr color-theme-choices)))

;; hook on after-make-frame-functions
;;(add-hook 'after-make-frame-functions 'test-win-sys)

;;(color-theme-deep-blue)
;;(add-to-list 'default-frame-alist '(cursor-type . (bar . 2)))
