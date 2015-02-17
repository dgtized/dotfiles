(defun ruby-eval-buffer () (interactive)
   "Evaluate the buffer with ruby."
   (shell-command-on-region (point-min) (point-max) "ruby -w "))

(defun clgc-ruby-mode-hook ()
  (require 'rinari)
  (setq rinari-tags-file-name "TAGS")
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-align-to-stmt-keywords '(def if case))
  (global-rinari-mode t))

(add-hook 'ruby-mode-hook 'clgc-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'subword-mode)
(add-hook 'ruby-mode-hook 'projectile-rails-on)

;; inf-ruby
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; Robe
(add-hook 'ruby-mode-hook 'robe-mode)

;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-robe))

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rbenv-use-corresponding))

;; rbenv
(defalias 'rvm-activate-corresponding-ruby 'rbenv-use-corresponding)
(add-hook 'after-init-hook 'rbenv-use-global)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(eval-after-load 'feature-mode
  '(setq feature-cucumber-command "zeus cucumber {feature}"))

(defun rinari-rake-migrate-up ()
  (interactive)
  (rinari-rake "db:migrate"))

(provide 'clgc-ruby)
