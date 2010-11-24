;;;_* Preferences
(when (equal machine-name "elguapo")
  (set-face-attribute 'default nil :height 100)
  (set-scroll-bar-mode 'right))

;; (when (equal machine-name "turtle")
;;   (set-fringe-mode 8))

;;;_* Load lisp files
(load "common")
(if (or (equal machine-name "turtle")
	  (equal machine-name "elguapo"))
    (progn
      (load "turtle")
      (load "if-installed")))

;;;_* Color 
(let ((winsys-colorlist 
       (list '("turtle" . color-theme-deep-blue)
	     '("elguapo" . color-theme-charcoal-black)))
      (out 'color-theme-standard)
      (terminal-color 'color-theme-standard)
      (winsys-color nil)
      (elem nil))
  (if window-system
      (progn
	;; select window system color
	(setq winsys-color
	      (dolist (elem winsys-colorlist winsys-color)
		(if (equal machine-name (car elem))
		    (setq winsys-color (cdr elem)))))
	;; change color
	(eval (list winsys-color)))))
    ;; (eval (list terminal-color))))
