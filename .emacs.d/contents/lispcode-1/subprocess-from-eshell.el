(defun subprocess-call (command-string)
  (interactive)
  (let* ( (command-as-list (split-string command-string " ")) 
	  (proc (car command-as-list))
	  (args (cdr command-as-list)) 
	  (invocation nil) )
    (apply 'start-process 
	   proc
	   (get-buffer-create (format "*%s-buffer*" proc))
	   command-as-list)))




