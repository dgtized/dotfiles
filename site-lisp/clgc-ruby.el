(defun ruby-eval-buffer () (interactive)
   "Evaluate the buffer with ruby."
   (shell-command-on-region (point-min) (point-max) "ruby -w "))

(defun my-ruby-mode-hook ()
  ;; Keep ac-mode from trying to complete on an end
  (make-local-variable 'ac-ignores)
  (add-to-list 'ac-ignores "end")
  (require 'rinari)
  (setq rinari-tags-file-name "TAGS")
  (global-rinari-mode t))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'subword-mode)
(add-hook 'ruby-mode-hook 'superword-mode)

;; inf-ruby
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; Robe
(add-hook 'ruby-mode-hook 'robe-mode)
;; (add-hook 'robe-mode-hook 'robe-ac-setup)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; RVM
(add-hook 'after-init-hook 'rvm-use-default)

;; broken per https://github.com/dgutov/robe/issues/20
;; (add-hook 'robe-mode 'robe-ac-setup)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(provide 'clgc-ruby)
