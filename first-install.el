#!/usr/bin/emacs --script

;; OS X:
;;   install wget, bzr with homebrew/macports/fink
;; Ubuntu:
;;   install curl, bzr, git, ftp, cvs with apt/synaptic
;; auctex needs compilation (requires latex)

;; invoke with 
;; $ emacs --script first-install.el

;;;_* set directory
(setq pkg-path "~/lisp/local-packages/")
(when (not (file-exists-p pkg-path))
  (let ((basepath 
	 (file-name-directory (replace-regexp-in-string "/$" "" pkg-path))))
    (when (not (file-exists-p basepath))
      (mkdir basepath)))
    (mkdir pkg-path))
(cd pkg-path)

;;;_* functions

(defun callprc (string)
  (call-process "/bin/bash" nil nil t "-c" string))

(defun retrieve-untar-command (packagename host filename)
  (flet ((join (mylist &optional sep)
	       (if (not sep)
		   (setq sep " "))
	       (mapconcat 'identity mylist sep)))
    (let ((get-command "curl -C - -O") ;(command "wget -c")
	  (uncompress "tar -xzvf"))
      (join (list (join (list get-command (concat host filename)))
		  (join (list uncompress filename))
		  (join (list "rm" filename))
		  (join (list "mv" (replace-regexp-in-string "\\.tar\\.gz" "" filename) packagename))) " && "))))

;;;_* download files

;;;_ . version control repositories
(let ((pkg-list 
       '(("apel" "cvs -z9 -d :pserver:anonymous@cvs.m17n.org:/cvs/root checkout apel")
	 ("ess" "git clone https://github.com/emacs-ess/ESS.git ess")
	 ("python-mode" "bzr branch lp:python-mode")
	 ("ipython" "mkdir ipython && cd ipython && curl -O -C - https://raw.github.com/ipython/ipython/master/docs/emacs/ipython.el")
	 ("evil" "git clone git://gitorious.org/evil/evil.git")
	 ("emacs-w3m" "cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m")
	 ("matlab-emacs" "cvs -z3 -d:pserver:anonymous@matlab-emacs.cvs.sourceforge.net:/cvsroot/matlab-emacs co -P matlab-emacs")
	 ("haskell-mode" "git clone https://github.com/haskell/haskell-mode.git haskell-mode")
	 ("emacspeak" "cvs -z3 -d:pserver:anonymous@emacspeak.cvs.sourceforge.net:/cvsroot/emacspeak co -P emacspeak"))))
  (dolist (pkg pkg-list)
    (callprc (cadr pkg))
    (byte-recompile-directory (concat pkg-path (car pkg)) 0 t)))

;;;_ . tar files
(let ((pkg-list 
       '(("color-theme" "http://ftp.igh.cnrs.fr/pub/nongnu/color-theme/" "color-theme-6.6.0.tar.gz")
	 ;; ("emacs-w3m" "http://emacs-w3m.namazu.org/" "emacs-w3m-1.4.4.tar.gz")
	 ("nxml-mode" "http://www.thaiopensource.com/download/" "nxml-mode-20041004.tar.gz")
	 ("elscreen" "ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/" "elscreen-1.4.6.tar.gz")
	 ;; the following may have newer versions
	 ("auctex" "http://ftp.gnu.org/pub/gnu/auctex/" "auctex-11.86.tar.gz")
	 ("nav-mode" "http://emacs-nav.googlecode.com/files/" "emacs-nav-20110220.tar.gz"))))
  (dolist (pkg pkg-list)
    (callprc (apply 'retrieve-untar-command pkg))
    (byte-recompile-directory (concat pkg-path (car pkg)) 0 t)))

;; http://sourceforge.net/projects/cedet/
;; http://sourceforge.net/projects/ecb/
