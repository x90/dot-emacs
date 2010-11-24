;;; emacspeak-finder.el --- Generate a database of keywords and descriptions for all Emacspeak  packages
;;; $Id: emacspeak-finder.el 5798 2008-08-22 17:35:01Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description: Auditory interface
;;; Keywords: Emacspeak, Finder
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2007, T. V. Raman
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Inspired by finder.el
;;; Not to be loaded at runtime.
;;{{{ requires
(require 'cl)
(require 'finder)
(require 'lisp-mnt)

;;}}}
;;{{{ initial

(defvar emacspeak-finder-inf-file
  (concat emacspeak-lisp-directory "/" "emacspeak-finder-inf.el")
  "File where we save the keyword/package associations for Emacspeak")

(defvar emacspeak-finder-preamble
  (concat
   ";;;$Id: emacspeak-finder.el 5798 2008-08-22 17:35:01Z tv.raman.tv $\n"
   ";;; emacspeak-finder-inf.el --- keyword-to-package mapping\n"
   ";; Keywords: help\n"
   ";;; Commentary:\n"
   ";;; Don't edit this file.  It's generated by\n"
   ";;; function emacspeak-finder-compile-keywords\n"
   ";;; Code:\n"
   "(require 'cl)\n"
   "\n(setq emacspeak-finder-package-info '(\n")
  "Preamble inserted at the front of emacspeak-finder-inf.el")

;;}}}
;;{{{ compile emacspeak keywords

(defvar emacspeak-finder-postamble
  (concat
   "))\n\n"
   "(loop for l  in (reverse emacspeak-finder-package-info) do\n (push l finder-package-info))\n"
   "(provide 'emacspeak-finder-inf)\n\n;;; emacspeak-finder-inf.el ends here\n")
  "Text to insert at the end of emacspeak-finder-inf")
(if (=  22 emacs-major-version)
    (defun emacspeak-finder-compile-keywords ()
      "Generate finder keywords for emacspeak."
      (let ((generated-finder-keywords-file
             (expand-file-name
              "emacspeak-finder-inf.el" emacspeak-lisp-directory)))
        (finder-compile-keywords
         emacspeak-lisp-directory
         (expand-file-name "g-client"
                           emacspeak-lisp-directory)
         (expand-file-name "atom-blogger"
                           emacspeak-lisp-directory))))

;;; older version for pre-22
  (defun emacspeak-finder-compile-keywords ()
    "Generate the keywords association list into the file
emacspeak-finder-inf.el."
    (declare (special emacspeak-lisp-directory emacspeak-finder-inf-file
                      emacspeak-finder-preamble))
    (save-excursion
      (let ((processed nil)
            (d emacspeak-lisp-directory)
            (buffer (find-file-noselect  emacspeak-finder-inf-file)))
        (set-buffer buffer)
        (erase-buffer)
        (insert emacspeak-finder-preamble)
        (mapcar
         (lambda (f)
           (if (and (string-match "^[^=.].*\\.el$" f)
                    (not (member f processed)))
               (let (summary keystart keywords)
                 (push f processed)
                 (save-excursion
                   (set-buffer (get-buffer-create "*finder-scratch*"))
                   (buffer-disable-undo (current-buffer))
                   (erase-buffer)
                   (insert-file-contents
                    (concat (file-name-as-directory (or d ".")) f))
                   (setq summary (lm-synopsis))
                   (setq keywords (lm-keywords)))
                 (insert
                  (format "    (\"%s\"\n        " f))
                 (prin1 summary (current-buffer))
                 (insert
                  "\n        ")
                 (setq keystart (point))
                 (insert
                  (if keywords (format "(%s)" keywords) "nil")
                  ")\n")
                 (subst-char-in-region keystart (point) ?, ? )
                 )))
         (directory-files (or d ".")))
        (insert emacspeak-finder-postamble)
        (kill-buffer "*finder-scratch*")
        (eval-buffer) ;; So we get the new keyword list immediately
        (basic-save-buffer)
        (kill-buffer nil)))))

;;}}}
(provide 'emacspeak-finder)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
