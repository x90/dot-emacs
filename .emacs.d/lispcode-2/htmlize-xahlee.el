(defun htmlize-string (sourceCodeStr langModeName)
  "Take SOURCECODESTR and return a htmlized version using LANGMODENAME.
This function requries the htmlize.el by Hrvoje Niksic, 2005"
  (require 'htmlize)
  (let (htmlizeOutputBuf x1 x2 resultS)

    ;; put code in a temp buffer, set the mode, fontify
    (with-temp-buffer
      (insert sourceCodeStr)
      (funcall (intern langModeName))
      (font-lock-fontify-buffer)
      (setq htmlizeOutputBuf (htmlize-buffer))
      )

    ;; extract the fontified source code in htmlize output
    (with-current-buffer htmlizeOutputBuf
      (setq x1 (search-forward "<pre>"))
      (setq x2 (search-forward "</pre>"))
      (setq resultS (buffer-substring-no-properties (+ x1 1) (- x2 6))))

    (kill-buffer htmlizeOutputBuf)
    resultS
    ))

(defun htmlize-block ()
  "Replace the region enclosed by <pre> tag to htmlized code.
For example, if the cursor somewhere inside the pre tags:

<pre class=\"code\">
mySourceCode...
</pre>

after calling, the “mySourceCode...” block of text will be htmlized.
That is, wrapped with many <span> tags.

The opening tag must be of the form <pre class=\"lang-str\">.
The “lang-str” determines what emacs mode is used to colorize
the code.
This function requires htmlize.el by Hrvoje Niksic."

  (interactive)
  (let (mycode tagBegin styclass codeBegin codeEnd tagEnd mymode)

    (setq tagBegin (re-search-backward "<pre class=\"\\([A-z-]+\\)\""))
    (setq styclass (match-string 1))
    (setq codeBegin (search-forward ">"))
    (search-forward "</pre>")
    (setq codeEnd (search-backward "<"))
    (setq tagEnd (search-forward "</pre>"))
    (setq mycode (buffer-substring-no-properties codeBegin codeEnd))

    (cond
     ((string= styclass "haskell") (setq mymode "haskell-mode"))
     ((string= styclass "ocaml") (setq mymode "tuareg-mode"))
     ((string= styclass "elisp") (setq mymode "emacs-lisp-mode"))
     ((string= styclass "scheme") (setq mymode "scheme-mode"))
     ((string= styclass "javascript") (setq mymode "js2-mode"))
     ((string= styclass "python") (setq mymode "python-mode"))
     ((string= styclass "ruby") (setq mymode "ruby-mode"))
     ((string= styclass "perl") (setq mymode "cperl-mode"))
     ((string= styclass "php") (setq mymode "php-mode"))
     ((string= styclass "c") (setq mymode "c-mode"))
     ((string= styclass "java") (setq mymode "java-mode"))
     ((string= styclass "html") (setq mymode "html-mode"))
     ((string= styclass "xml") (setq mymode "xml-mode"))
     ((string= styclass "css") (setq mymode "css-mode"))
     ((string= styclass "povray") (setq mymode "pov-mode"))
     ((string= styclass "lsl") (setq mymode "xlsl-mode"))
     ((string= styclass "r") (setq mymode "r-mode")) ;; st addition
     ((string= styclass "sh") (setq mymode "sh-mode"))
     ((string= styclass "latex") (setq mymode "latex-mode"))
     )

    (save-excursion
      (delete-region codeBegin codeEnd)
      (goto-char codeBegin)
      (insert (htmlize-string mycode mymode))
      )
    )
  )
