;; as part of emacspeak
(load-library "g")
(setq g-user-email "stakahama@gmail.com")
;; (gcal-emacs-calendar-setup)
(if (>= emacs-major-version 23)
	(require 'w3m-ems)
  (require 'w3m))
;;(require 'w3m-load)
(setq g-html-handler 'w3m-buffer)
;;
(setq seaotter-koala
      "http://www.blogger.com/feeds/3323596612080870401/posts/default")
(defun st-new-entry ()
  (interactive)
  (gblogger-new-entry seaotter-koala))
