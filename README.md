Instructions:
- Download this repository to local machine (to update, use `git pull`)
- `makelinks.el` will save current `~/.emacs` as `~/.emacs_elsave`; create symlink from .emacs to `~/.emacs` and `emacs-root` (`.emacs.d/contents` to `~/.emacs.d/contents`)
- `install-packages.el` will install necessary packages (if not installed already in `local-packages`). Otherwise, package will not be loaded by `if-installed.el`.
- Create or edit ~/.emacs.d/local-settings.el, which accepts values for the following custom variables: `account-username`, `machine-name`,
   `emacs-root`, `local-packages`, `color-theme-local`. For instance,

```emacs-lisp
;; Primary variables
  (setq account-username "stakahama")
  (setq machine-name "turtle")
  (setq emacs-root "~/.emacs.d/contents")
  (setq local-packages "~/lisp/local-packages")
  (setq color-theme-local 'color-theme-tangotango)
;; Also, additional lines for gblogger
  (setq g-user-email "me@gmail.com")
  (setq g-blog-address "http://www.blogger.com/feeds/012345/posts/default")
;; Environment variables can be set in .MacOSX/environment.plist (for OS X) and also as
  ;;(setenv "PATH" (concat "/opt/local/bin:/opt/local/sbin:" (getenv "PATH")))
  (load "~/.emacs.d/bash_env.el")
```

- Regarding the last line above, http://bitbucket.org/stakahama/myshell also contains `makeelfile.py` which will create a file called `bash_env.el` in `myshell/{OSname}/.emacs.d/` (and a symlink to `~/.emacs.d/bash_env.el`) for OSes in which environmental or system variables in `.bashrc`, `.xsession`, or `.MacOSX/environment.plist` cannot be accessed.

Additional notes:
- `~/.emacs` will load `~/.emacs.d/local-settings.el` at the top
  (after definition of these custom variables)
- Packages called in `if-installed.el` and additional machine-specific
  files (`turtle.el`, `elguapo.el`, `clamshell.el`) should be in the
  path specified by the local-packages variable.
- Byte compile for first time ("0" prefix suppresses prompting for each file):
    C-u 0 M-x byte-recompile-directory
- Only byte compile necessary files:
    M-x byte-recompile-directory
- http://bitbucket.org/stakahama/myshell contains emacs and emacsclient aliases

Command line (bash) byte recompilation:
$ emacs --batch --eval '(byte-recompile-directory ".emacs.d/contents" 0)'
$ rm -f .emacs.d/contents/contributed/folding.elc ## never compiles correctly
