;;;_* Preferences
(when (equal machine-name "elguapo")
  (set-face-attribute 'default nil :height 100)
  (set-scroll-bar-mode 'right))

;; (when (equal machine-name "turtle")
;;   (set-fringe-mode 8))

;;;_* Load lisp files
(load "common")
(if (or (equal machine-name "turtle")
	(equal machine-name "elguapo")
	(equal machine-name "clamshell"))
    (progn
      (load "turtle")
      (load "if-installed")))
(if (or (equal machine-name "aprlmac1")
	(equal machine-name "aprlpc1"))
    (load "if-installed"))

;;;_* Color 
(if (equal color-theme-local 'color-theme-tangotango)
    (load "color-theme-tangotango")
  (if (equal color-theme-local 'color-theme-zenburn)
      (load "zenburn")))
;; (let ((winsys-colorlist-default
;;        (list '("turtle" . color-theme-deep-blue)
;; 	     '("elguapo" . color-theme-charcoal-black)))
;;       (out 'color-theme-standard)
;;       (terminal-color 'color-theme-standard)
;;       (machine-name machine-name)
;;       (winsys-color color-theme-local)
;;       (elem nil))
;;   (if window-system
;;       ;; select window system color
;;       (progn
;; 	(if (not winsys-color)
;; 	    (dolist (elem winsys-colorlist-default winsys-color)
;; 	      (if (equal machine-name (car elem))
;; 		  (setq winsys-color (cdr elem)))))
;; 	;; change color
;; 	(eval (list winsys-color)))))
;;     ;; (eval (list terminal-color))))
(let ((winsys-colorlist-default
       (list '("turtle" . color-theme-deep-blue)
	     '("elguapo" . color-theme-charcoal-black)))
      (out 'color-theme-standard)
      (terminal-color 'color-theme-standard)
      (machine-name machine-name)
      (winsys-color color-theme-local)
      (elem nil))
  (if (and window-system color-theme-local)
      ;; change color
      (eval (list color-theme-local))))
