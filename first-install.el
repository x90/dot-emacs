;;;_* functions

(defun callprc (string)
  (call-process "/bin/bash" nil nil t "-c" string))

(defun retrieve-untar-command (packagename host filename)
  (let ((command "wget -c"))
    (format "%s %s && tar -xzvf %s && rm %s && mv %s %s"
	    (command (concat host filename) 
		     filename 
		     filename
		     (replace-regexp-in-string "\\.tar\\.gz" "" filename) packagename))))

(defun call-retrieve-untar (packagename host filename)
  (callprc (get-untar-command packagename host filename))
  (byte-recompile-directory packagename 0))

;;;_* set directory

(let ((pkg-path "~/lisp/local-packages"))
  (if (not (file-exists-p pkg-path))
      (mkdir pkg-path))
  (cd pkg-path))

;;;_* download files

;;;_ . version control repositories
(callprc "cvs -z9 -d :pserver:anonymous@cvs.m17n.org:/cvs/root checkout apel")
(byte-recompile-directory "apel" 0)
(callprc "git clone https://github.com/emacs-ess/ESS.git ess")
(byte-recompile-directory "ess" 0)
(callprc "git clone git://gitorious.org/evil/evil.git")
(byte-recompile-directory "evil" 0)
(callprc "cvs -z3 -d:pserver:anonymous@matlab-emacs.cvs.sourceforge.net:/cvsroot/matlab-emacs co -P matlab-emacs")
(byte-recompile-directory "matlab-emacs" 0)
(callprc "git clone https://github.com/haskell/haskell-mode.git haskell-mode")
(byte-recompile-directory "haskell-mode" 0)

;;;_ . tar files
(let ((pkg-list '(("color-theme" "http://download.savannah.gnu.org/releases/color-theme/" "color-theme-6.6.0.tar.gz")
		  ("emacs-w3m" "http://emacs-w3m.namazu.org/" "emacs-w3m-1.4.4.tar.gz")
		  ("nxml-mode" "http://www.thaiopensource.com/download/" "nxml-mode-20041004.tar.gz")
		  ("elscreen" "ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/" "elscreen-1.4.6.tar.gz")
		  ;; the following may have newer versions
		  ("auctex" "http://ftp.gnu.org/pub/gnu/auctex/" "auctex-11.86.tar.gz")
		  ("nav-mode" "http://emacs-nav.googlecode.com/files/" "emacs-nav-20110220.tar.gz")
)))
  (dolist (pkg pkg-list)
    (apply 'call-retrieve-untar pkg)))

;; http://sourceforge.net/projects/emacspeak/
;; http://sourceforge.net/projects/cedet/
;; http://sourceforge.net/projects/ecb/
