(defun ruby-eval-buffer () (interactive)
   "Evaluate the buffer with ruby."
   (shell-command-on-region (point-min) (point-max) "ruby -w "))

(defun clgc-ruby-mode-hook ()
  (require 'smartparens-ruby)
  (require 'rinari)
  (setq rinari-tags-file-name "TAGS")
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-align-to-stmt-keywords '(def if case))
  ;;(global-rinari-mode t)
  )

(add-hook 'ruby-mode-hook 'clgc-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'subword-mode)
(add-hook 'ruby-mode-hook 'projectile-rails-on)

;; inf-ruby
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; Robe
(add-hook 'ruby-mode-hook 'robe-mode)

;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-robe))

(require 'chruby)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (chruby-use-corresponding))

(defun clgc-chruby-default () (chruby "ruby-2.2.3"))

;; chruby
(defalias 'rvm-activate-corresponding-ruby 'chruby-use-corresponding)
(add-hook 'after-init-hook 'clgc-chruby-default)

(eval-after-load 'rspec-mode
  '(progn (rspec-install-snippets)
          (setq rspec-command-options "--format documentation --profile 30"
                rspec-use-opts-file-when-available nil
                rspec-use-rvm t)))

(eval-after-load 'feature-mode
  '(setq feature-cucumber-command "zeus cucumber {feature}"))

(defun inf-ruby-console-cap (dir)
  "Run Rails console in DIR."
  (interactive "D")
  (let* ((default-directory (file-name-as-directory dir))
         (envs (inf-ruby-console-rails-envs))
         (env (completing-read "Capistrano environment: " envs nil t
                               nil nil (car (member "staging" envs))))
         (with-bundler (file-exists-p "Gemfile")))
    (run-ruby (concat (when with-bundler "bundle exec ")
                      "cap "
                      env
                      " rails:console")
              "rails")))

(provide 'clgc-ruby)
