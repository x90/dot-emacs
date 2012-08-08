#!/usr/bin/emacs --script

;; creates symbolic links to .emacs and .emacs.d/ in $HOME directory
;; *NOTE* untested

;;;_* which files to link

(setq filelist '(".emacs" ".emacs.d/contents" ".emacs.d/elpa"))

;;;_* libraries
(require 'cl)

;;;_* general function
(defun path-join (dirname filename)
  (concat (file-name-as-directory dirname) filename))

;; 1) define paths; 2) define linking function; 3) create links
(lexical-let ((PWD default-directory)
	      (HOME (getenv "HOME")))
  (labels ((unlink (filename)
		   (let ((fullfile (path-join HOME filename)))
		     (if (file-exists-p fullfile)
			 (delete-file ))))
	   (link (filename) 
		 (let ((lncmd
			(format "ln -svf %s %s" 
				(path-join PWD filename) 
				(path-join HOME filename))))
		   (call-process "/bin/bash" nil 0 t "-c" lncmd))))
    (let (f)
      (dolist (f filelist)
	(unlink f)
	(link f)))))
