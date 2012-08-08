;; as part of emacspeak
(load-library "g")
;; (gcal-emacs-calendar-setup)
(if (>= emacs-major-version 23)
	(require 'w3m-ems)
  (require 'w3m))
;;(require 'w3m-load)
(setq g-html-handler 'w3m-buffer)
;;
(defun st-new-entry ()
  (interactive)
  (gblogger-new-entry g-blog-address))
