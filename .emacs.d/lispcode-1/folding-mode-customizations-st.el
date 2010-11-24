;;;_* libraries
(require 'cl)
(load "folding" 'nomessage 'noerror)

;;;_* key bindings
(define-key folding-mode-map "\C-e" 'move-end-of-line)
(define-prefix-command	'fm-map nil "Folding-")
(define-key fm-map "f"	'folding-fold-region)
(define-key fm-map ">"	'folding-shift-in)
(define-key fm-map "<"	'folding-shift-out)
(define-key fm-map "t"	'folding-show-all)
(define-key fm-map "s"	'folding-show-current-entry)
(define-key fm-map "x"	'folding-hide-current-entry)
(define-key fm-map "o"	'folding-open-buffer)
(define-key fm-map "w"	'folding-whole-buffer)
(define-key fm-map "r"	'folding-convert-buffer-for-printing)
(define-key fm-map "k"	'folding-marks-kill)
(define-key fm-map "v"	'folding-pick-move)
(define-key fm-map "V"	'folding-previous-visible-heading)
(define-key fm-map " "	'folding-next-visible-heading)
(define-key fm-map "."	'folding-context-next-action)
;;  C-u:  kinda "up" -- "down"
(define-key fm-map "\C-u"	'folding-toggle-enter-exit)
(define-key fm-map "\C-q"	'folding-toggle-show-hide)
;; Think "#" as a 'fence'
(define-key fm-map "#"  'folding-region-open-close)
;; Esc-; is the standard emacs commend add key.
(define-key fm-map ";"  'folding-comment-fold)
(define-key fm-map "%"  'folding-convert-to-major-folds)
(define-key fm-map "/"  'folding-all-comment-blocks-in-region)
(define-key fm-map "y"  'folding-show-current-subtree)
(define-key fm-map "z"  'folding-hide-current-subtree)
(define-key fm-map "n"  'folding-display-name)
(define-key fm-map "I"	'folding-insert-advertise-folding-mode)
(define-key fm-map "g"	'folding-comment-region)
;; (setq folding-mode-prefix-key "\M-p")
;; (folding-bind-default-keys)
(folding-add-to-marks-list 'LilyPond-mode "%%{{{"  "%%}}}" nil t)
(folding-add-to-marks-list 'ess-mode "##{{{"  "##}}}" nil t)
(add-hook 'folding-mode-hook 
	  (lambda ()
	    (local-set-key "\M-]" fm-map)))
	    ;; (local-set-key (kbd "C-c @ S") 'folding-show-current-entry)))

;;;_* functions

(defun folding-insert-mode-mark ()
  (interactive)
  ;;http://stackoverflow.com/questions/2394978/matching-keys-in-association-lists-in-emacs-lisp/2395997#2395997
  (flet ((fn (s) (save-excursion (or (search-backward s () t) 0))))
    (destructuring-bind (mode st en)
        (or (assoc major-mode folding-mode-marks-alist) '(nil "" ""))
      (insert (if (<= (fn st) (fn en)) st en)))))


(defun folding-comment-region ()
  (interactive)
  (if (and transient-mark-mode
           mark-active)
    (destructuring-bind (mode st en)
        (or (assoc major-mode folding-mode-marks-alist) '(nil "" ""))
      (flet ( (newlinep (h) 
			(save-excursion 
			  (equal 1 (- (or (search-forward "\n" () t) 0) h)))) )
	(let ( (here (point) )
	       (pos1 (region-beginning))
	       (pos2 (region-end)) )
	  (save-excursion
	    (comment-region pos1 pos2)
	    (goto-char pos1)
	    (if (not (newlinep here)) 
		(open-line 1))
	    (insert st)
	    (goto-char pos2)
	    (if (not (newlinep here)) 
		(progn (next-line) (insert en) (open-line 1))
	      (insert en)))
	  (folding-hide-current-entry)
	  (search-forward st () t))))
      (folding-insert-mode-mark)))

;;;_* hooks
(add-hook 'tex-mode-hook 'turn-on-folding-mode)
(add-hook 'matlab-mode-hook 'turn-on-folding-mode)
(add-hook 'ess-mode-hook 'turn-on-folding-mode)
(add-hook 'python-mode-hook 'turn-on-folding-mode)
(add-hook 'ess-mode-hook 'turn-on-folding-mode)
(add-hook 'LilyPond-mode-hook 'turn-on-folding-mode)
(add-hook 'matlab-mode-hook 'turn-on-folding-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-folding-mode)
