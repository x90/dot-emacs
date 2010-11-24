;;;_* keybindings


;;;_ . frames

(global-set-key (kbd "C-c o") 'other-frame-fb)
(global-set-key (kbd "C-`") 'other-frame)
(global-set-key (kbd "C-~") 'other-frame-b)
(global-set-key [(shift f9)] 'place-frame-at-top)
(global-set-key [(f11)] 'toggle-fullscreen)
(global-set-key [(shift f12)] 'sizetw)
(global-set-key [(shift f10)] 'enlarge-frame-vertically)
(global-set-key [(shift f11)] 'enlarge-frame-horizontally)

;;;_ . windows

(global-set-key (kbd "C-c w") 'enlarge-window-by-ten)
(global-set-key (kbd "C-x p") 'other-window-previous)
(global-set-key (kbd "C-c T") 'transpose-windows)

;;;_ . buffer and frames

(global-set-key (kbd "C-x K") 'kill-this-buffer-and-frame)

;;;_* functions

;;;_ . frames

;;;_  : same size (defadvice)

(defadvice make-frame-command
  ;; return ad-return-value ?
  (around make-frame-same-size activate)
  (let ((width (frame-width))
	(height (frame-height)))
    ad-do-it
    (set-frame-size (window-frame (selected-window)) width height)))
    
;;;_  : ?? general

(defun alist-getparm (symbol) 
  (let (value)
    (dolist (elem default-frame-alist value)
      (if (eq (car elem) symbol)
	  (setq value (cdr elem))))))

;;;_  : switch

(defun other-frame-fb (&optional arg)
  (interactive "P")
  (let (x)
    (if arg (setq x -1) (setq x 1))
    (other-frame x)))

(defun other-frame-b ()
  (interactive)
  (other-frame -1))

;;;_  : move

(defun place-frame-at-top ()
  (interactive)
  ;;http://www.gnu.org/s/emacs/manual/html_node/elisp/Frame-Parameters.html#Frame-Parameters
  (let ( (thisframe (selected-frame)) )
    (set-frame-position 
     thisframe
     (frame-parameter thisframe 'left) 
     -1)))

;;;_  : resize

(defun sizetw (&optional width height)
  (interactive)
  (flet ((getparm (symbol) 
		  (let (value)
		    (dolist (elem default-frame-alist value)
		      (if (eq (car elem) symbol)
			  (setq value (cdr elem)))))))
    (if (and width height)
	(set-frame-size (window-frame (selected-window)) width height)
      (set-frame-size (window-frame (selected-window)) 
		      (getparm 'width) (getparm 'height)))))


(labels ((modify-current (arg val default)
			 (if arg 
			     (if (equal arg '(4)) 
				 (- val default)
			       (+ val arg)) 
			   (+ val default))))

  (defun enlarge-frame-vertically (&optional arg)
    (interactive "P")
    (let ( (width (frame-width)) 
	   (height (frame-height))
	   (default 10)
	   (newval 0) )
      (setq newval (modify-current arg height default))
      (set-frame-size (window-frame (selected-window)) width newval)))

  (defun enlarge-frame-horizontally (&optional arg)
    (interactive "P")
    (let ( (width (frame-width)) 
	   (height (frame-height))
	   (default 20)
	   (newval 0) )
      (setq newval (modify-current arg width default))
      (set-frame-size (window-frame (selected-window)) newval height))))

;; do I still need this if I set default?
;; (defadvice make-frame-command
;;   ;;"raise frame to top of screen after new frame is created"
;;   (after make-frame-command-raise activate)
;;   (place-frame-at-top))

;;;_ . windows

;;;_  : switch

(defun other-window-previous ()
  (interactive)
  (other-window -1))

;;;_  : enlarge

(defun enlarge-window-by-ten (&optional minus)
  (interactive "P")
  (if minus
      (enlarge-window -10)
    (enlarge-window 10)))

;;;_  : transpose

;;{{{---from Steve Yegge---
;; (defun swap-windows ()
;;   "If you have 2 windows, it swaps them." 
;;   (interactive)
;;   (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
;;         (t
;;          (let* ((w1 (first (window-list)))
;;                 (w2 (second (window-list)))
;;                 (b1 (window-buffer w1))
;;                 (b2 (window-buffer w2))
;;                 (s1 (window-start w1))
;;                 (s2 (window-start w2)))
;;            (set-window-buffer w1 b2)
;;            (set-window-buffer w2 b1)
;;            (set-window-start w1 s2)
;;            (set-window-start w2 s1)))))
;;}}}

;; from emacs wiki
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;;;_ . buffers and frames

(defun kill-frame-and-buffers ()
  (interactive)
  (let ((wlist (window-list))
	(thisframe (selected-frame))
	(x 1))
    (setq x 0)
    (while (< x (length wlist))
      (kill-buffer (window-buffer (car (nthcdr x wlist)))))
    (delete-frame this-frame)))

(defun kill-this-buffer-and-frame (&optional arg)
  (interactive "P")
  (if arg
      (kill-frame-and-buffers)
    (progn
      (kill-this-buffer)
      (delete-frame))))

