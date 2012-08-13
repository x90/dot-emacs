;; #!/usr/bin/emacs --script

;; OS X:
;;   install wget, bzr with homebrew/macports/fink
;; Ubuntu:
;;   install curl, bzr, git, ftp, cvs with apt/synaptic
;; auctex needs compilation (requires latex)

;; OS X: 
;;   may additionally need to fix wget certificates
;;   http://loopkid.net/articles/2011/09/20/ssl-certificate-errors-on-mac-os-x
;;   $ sudo port install curl-ca-bundle
;;   $ echo "CA_CERTIFICATE=/opt/local/share/curl/curl-ca-bundle.crt" >> ~/.wgetrc

;; invoke with 
;; $ emacs -Q --script install-packages.el
;; $ emacs -Q --script install-packages.el arg -> where arg!=-1 etc. to overwrite

;;;_* set user variables
(setq pkg-path "~/lisp/local-packages/")

;;;_* load libraries
(require 'cl)

;;;_* parse script arguments
(setq package-overwrite (if argv t nil))

;;;_* functions

(setq get-command (if (executable-find "wget") "wget -c" "curl -C - -O"))
					; note: curl may not work with https:// domains

(defun callprc (string)
  (call-process "/bin/bash" nil nil t "-c" string))

(defun retrieve-untar-command (packagename host filename)
  (flet ((join (mylist &optional sep)
	       (if (not sep)
		   (setq sep " "))
	       (mapconcat 'identity mylist sep)))
	(let ((uncompress "tar -xzvf"))
      (join (list (join (list get-command (concat host filename)))
		  (join (list uncompress filename))
		  (join (list "rm" filename))
		  (join (list "mv" (replace-regexp-in-string "\\.tar\\.gz" "" filename) 
			      packagename))) " && "))))

(defun try-compile (pathname)
  (condition-case nil
      (byte-recompile-directory pathname 0 t)
    (error nil)))

;;;_* create package directory
(when (not (file-exists-p pkg-path))
  (let ((basepath 
	 (file-name-directory (replace-regexp-in-string "/$" "" pkg-path))))
    (when (not (file-exists-p basepath))
      (mkdir basepath)))
    (mkdir pkg-path))
(cd pkg-path)

;;;_* download files

;;;_ . version control repositories
(let ((pkg nil)
      (pkg-list 
       (append 
	'(("apel" "cvs -z9 -d :pserver:anonymous@cvs.m17n.org:/cvs/root checkout apel")
	  ("ess" "git clone https://github.com/emacs-ess/ESS.git ess")
	  ("org-mode" "git clone git://orgmode.org/org-mode.git")
	  ("python-mode" "bzr branch lp:python-mode")
	  ("evil" "git clone git://gitorious.org/evil/evil.git")
	  ("emacs-w3m" "cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m")
	  ("matlab-emacs" "cvs -z3 -d:pserver:anonymous@matlab-emacs.cvs.sourceforge.net:/cvsroot/matlab-emacs co -P matlab-emacs")
	  ("haskell-mode" "git clone https://github.com/haskell/haskell-mode.git haskell-mode")
	  ("emacspeak" "cvs -z3 -d:pserver:anonymous@emacspeak.cvs.sourceforge.net:/cvsroot/emacspeak co -P emacspeak"))
	(list (list "ipython" (format "mkdir ipython && cd ipython && %s https://raw.github.com/ipython/ipython/master/docs/emacs/ipython.el" 
				      get-command))))))
  (dolist (pkg pkg-list)
    (when (or (not (file-exists-p (car pkg))) package-overwrite)
      (print (format "installing %s" (car pkg)))
      (callprc (cadr pkg))
      (try-compile (concat pkg-path (car pkg))))))

;;;_ . tar files
;;     (may need to update specific versions)
(let ((pkg nil)
      (pkg-list 
       '(("color-theme" "http://ftp.igh.cnrs.fr/pub/nongnu/color-theme/" "color-theme-6.6.0.tar.gz")
	 ;; ("emacs-w3m" "http://emacs-w3m.namazu.org/" "emacs-w3m-1.4.4.tar.gz")
	 ("nxml-mode" "http://www.thaiopensource.com/download/" "nxml-mode-20041004.tar.gz")
	 ("elscreen" "ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/" "elscreen-1.4.6.tar.gz")
	 ;; in case bzr is not installed
	 ("python-mode" "https://launchpad.net/python-mode/trunk/6.0.10/+download/python-mode.el-6.0.10.tar.gz")
	 ;; the following may have newer versions
	 ("auctex" "http://ftp.gnu.org/pub/gnu/auctex/" "auctex-11.86.tar.gz")
	 ;; ("nav-mode" "http://emacs-nav.googlecode.com/files/" "emacs-nav-20110220.tar.gz")
	 ("magit" "http://github.com/downloads/magit/magit/" "magit-1.1.1.tar.gz"))))
          ;; go to https://github.com/magit/magit/
          ;;       http://github.com/magit/magit/downloads
          ;;       http://github.com/magit/magit/tarball/master
  (dolist (pkg pkg-list)
    (when (or (not (file-exists-p (car pkg))) package-overwrite)
      (print (format "installing %s" (car pkg)))
      (callprc (apply 'retrieve-untar-command pkg))
      (try-compile (concat pkg-path (car pkg))))))

;; http://sourceforge.net/projects/cedet/
;; http://sourceforge.net/projects/ecb/

;;;_ . individual files

(let ((pkg nil)
      (pkg-list
       '(("sr-speedbar" "http://emacswiki.org/emacs/download/sr-speedbar.el")
	 ("autopair" "https://raw.github.com/capitaomorte/autopair/master/autopair.el")
	 ;; ("dirtree" ("http://www.emacswiki.org/emacs/download/dirtree.el"
	 ;; 	     "http://www.emacswiki.org/emacs/download/windata.el"
	 ;; 	     "http://www.emacswiki.org/emacs/download/tree-mode.el")))))
	 )))
  (dolist (pkg pkg-list)
    (when (or (not (file-exists-p (car pkg))) package-overwrite)
      (print (format "installing %s" (car pkg)))
      (let ((path (concat pkg-path (car pkg)))
	    (flist (cadr pkg)))
	(flet ((get-move (f p) 
			 (callprc (format "%s %s" get-command f))
			 (rename-file (file-name-nondirectory f) p t)))
	  (if (not (file-exists-p path))
	      (mkdir path))
	  (if (listp flist)
	      (let (f)
		(dolist (f flist)
		  (get-move f path)))
	    (get-move flist path))
	  (try-compile path))))))

;; speedbar.el comes standard with Emacs 24
