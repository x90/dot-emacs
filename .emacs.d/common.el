;;;_* ===== Fortran =====

(add-hook 'fortran-mode-hook
	  '(lambda() (linum-mode 1)))
(add-hook 'f90-mode-hook
	  '(lambda() 
	     (linum-mode 1)
	     (toggle-truncate-lines 1)))

;;;_* ===== GDB =====

'(gdb-many-windows t)
'(gdb-show-main t)

;;;_* ===== Tramp-mode =====

(setq tramp-default-method "ssh")

