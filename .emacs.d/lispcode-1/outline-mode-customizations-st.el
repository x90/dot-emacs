;;;_* my outline-mode conventions 

;; Using outline-mode, follow conventions of allout-mode
;; taken from "http://fp-etc.progiciels-bpi.ca/allout-vim/doc.txt"
;;
;; There are four kind of bullets: fixed, floating, numbered or link.  
;;
;; Any bullet may be used at any node level, save for the rule that nodes at ;; level 1 are required to use the `*' bullet.
;;
;; Fixed bullets are `*', `+' and `-'. 
;;
;; Floating bullets are such that each level has its own floating preference: ;; level 2 prefers `.', 
;; level 3 prefers `:', 
;; level 4 prefers `,', 
;; level 5 prefers `;',
;; the cycle repeats afterwards, 
;; so level 5 prefers `.', level 6 prefers ':', etc.  
;;
;; A numbered bullet uses more than one character: it is `#' immediately 
;; followed by a number.  
;;
;; The only link bullet is `@', it is
;; meant to represent a reference to some other document.


;;;_* keybindings

;; see http://www.emacswiki.org/emacs/OutlineMinorMode
					; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")
					; HIDE
(define-key cm-map "q" 'hide-sublevels)	; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)		; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)		; Hide other branches
(define-key cm-map "c" 'hide-entry)		; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)		; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)		; Hide everything in this entry and sub-entries
					; SHOW
(define-key cm-map "a" 'show-all)		; Show (expand) everything
(define-key cm-map "e" 'show-entry)		; Show this heading's body
(define-key cm-map "i" 'show-children)		; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)		; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)		; Show (expand) everything in this heading & below
					; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(define-key cm-map "g" 'outline-insert-bullet)  ; ST function
(add-hook 'outline-minor-mode-hook 
	  (lambda ()
	    (local-set-key "\M-[" cm-map)))

;;;_* functions
(defun outline-insert-bullet ()
  (interactive)
  (flet ((string-repeat (str n)
			(let ((retval ""))
			  (dotimes (i n)
			    (setq retval (concat retval str)))
			  retval)))
  (insert (concat (string-repeat (substring comment-start 0 1) 3) "_"))))

(defun generic-outline-hook (cc)
  (lexical-let* ((cstart (concat "^" cc))
		 (cfull (concat cstart "\\(\\*\\|[ ]+[+-=>()[{}&!?#%\"X@$~_\\:;^]\\)")))
  (labels ((my-outline-level ()
			   (let (buffer-invisibility-spec)
			     (save-excursion
			       (search-forward-regexp cfull)
			       (- (current-column) 3)))))
    (lambda ()
      (setq outline-regexp cstart)
      (setq outline-level 'my-outline-level)
      (outline-minor-mode t)
      (hide-body)))))

;;;_* hooks

(add-hook 'emacs-lisp-mode-hook (generic-outline-hook ";;;_"))
(add-hook 'ess-mode-hook (generic-outline-hook "###_"))
(add-hook 'python-mode-hook (generic-outline-hook "###_"))
(add-hook 'tex-mode-hook (generic-outline-hook "%%%_"))
(add-hook 'matlab-mode-hook (generic-outline-hook "%%%_"))
(add-hook 'sh-mode-hook (generic-outline-hook "###_"))
