#!/bin/usr/emacs --script

;; creates symbolic links to .emacs and .emacs.d/ in $HOME directory
;; *NOTE* untested

;; general function
(defun path-join (dirname filename)
  (concat (file-name-as-directory dirname) filename))

;; 1) define paths; 2) define linking function; 3) create links
(lexical-let ((PWD (pwd))
	      (HOME (getenv "HOME")))
  (labels ((link (filename) 
		  (print (format "ln -svf %s %s" 
			  (path-join PWD filename) 
			  (path-join HOME filename)))
		  (call-process nil nil t "-svf" 
				(path-join PWD filename) 
				(path-join HOME filename))))
    (let ((filelist (list ".emacs" ".emacs.d"))
	  (f nil))
      (dolist (f filelist)
	(link f)))))
