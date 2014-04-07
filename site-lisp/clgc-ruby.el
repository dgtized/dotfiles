(defun ruby-eval-buffer () (interactive)
   "Evaluate the buffer with ruby."
   (shell-command-on-region (point-min) (point-max) "ruby -w "))

(defun my-ruby-mode-hook ()
  (require 'rinari)
  (setq rinari-tags-file-name "TAGS")
  (eval-after-load 'company-mode '(add-to-list 'company-backends 'company-robe))
  (global-rinari-mode t))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'subword-mode)
(add-hook 'ruby-mode-hook 'superword-mode)
(add-hook 'ruby-mode-hook 'projectile-rails-on)

;; inf-ruby
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-inf-ruby))

;; Robe
(add-hook 'ruby-mode-hook 'robe-mode)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; RVM
(add-hook 'after-init-hook 'rvm-use-default)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(defun rinari-rake-migrate-up ()
  (interactive)
  (rinari-rake "db:migrate"))

(provide 'clgc-ruby)
