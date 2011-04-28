(autoload 'js2-mode "js2-mode" "Steve Yegge's Javascript Major Mode" t)

(defun my-js2-mode-hook ()
  (setq js-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;; (require 'flymake-jslint)
;; (add-hook 'javascript-mode-hook
;; 	  (lambda () (flymake-mode t)))

(provide 'clgc-javascript)
