#!/usr/bin/emacs --script

;; creates symbolic links to .emacs and .emacs.d/ in $HOME directory
;; *NOTE* untested

;;;_* which files to link

(setq filelist '(".emacs" ".emacs.d/contents" ".emacs.d/elpa"))
(if (not (file-exists-p "~/.emacs.d/")) (mkdir "~/.emacs.d/"))

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
			 (if (file-symlink-p fullfile)
			     (delete-file filename)
			   (rename-file filename (concat filename "_elsave"))))))
	   (link (filename) 
		 (let* ((localfile (path-join PWD filename))
			(fullfile (path-join HOME filename))
			(lncmd (format "ln -svf %s %s" localfile fullfile)))
		   (call-process "/bin/bash" nil 0 t "-c" lncmd))))
    (let (f)
      (dolist (f filelist)
	(unlink f)
	(link f)))))

