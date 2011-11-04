;; (defun eshell/git (&rest args)
;;   (apply 'eshell-exec-visual (cons "git" args)))

(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

(provide 'clgc-eshell)
