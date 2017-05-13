(autoload 'js2-mode "js2-mode" "Steve Yegge's Javascript Major Mode" t)

(defun my-js2-mode-hook ()
  (setq indent-tabs-mode nil
        js-indent-level 2
        js2-basic-offset 2
        js2-auto-indent-p t
        js2-cleanup-whitespace t
        js2-indent-on-enter-key nil
        js2-use-ast-for-indentation-p t))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)
(add-hook 'js-mode-hook 'smartparens-strict-mode)
(add-hook 'js2-mode-hook 'smartparens-strict-mode)

;; (require 'flymake-jslint)
;; (add-hook 'javascript-mode-hook
;; 	  (lambda () (flymake-mode t)))

(provide 'clgc-javascript)
