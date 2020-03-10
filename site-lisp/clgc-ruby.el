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
(add-hook 'ruby-mode-hook 'aggressive-indent-mode)
(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'dired-mode-hook 'rspec-dired-mode)

;; inf-ruby
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; From: https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/ruby/funcs.el#L43
;; and: https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/ruby/packages.el#L172
(defun spacemacs//inf-ruby-auto-enter ()
  "Automatically enters inf-ruby-mode in ruby modes' debugger breakpoints."
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter nil t))
(add-hook 'rspec-compilation-mode-hook 'spacemacs//inf-ruby-auto-enter)

;; Robe
(add-hook 'ruby-mode-hook 'robe-mode)

(eval-when-compile (require 'company))
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-robe))

(require 'chruby)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (chruby-use-corresponding))

(defun clgc-chruby-default () (chruby "ruby-2.5.7"))

;; chruby
(defalias 'rvm-activate-corresponding-ruby 'chruby-use-corresponding)
(add-hook 'after-init-hook 'clgc-chruby-default)

(with-eval-after-load 'rspec-mode
  (progn (rspec-install-snippets)
         (setq rspec-command-options "--format documentation --profile 30"
               rspec-use-bundler-when-possible nil
               rspec-use-opts-file-when-available nil
               rspec-use-chruby t
               rspec-primary-source-dirs '("app"))))

(eval-when-compile (require 'feature-mode))
(with-eval-after-load 'feature-mode
  (setq feature-cucumber-command "zeus cucumber {feature}"))

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

(defun clgc-rails-db-test ()
  (interactive)
  (projectile-rails-rake "db:migrate db:test:prepare"))

;; overriding rubocop build-command to specify relative config if available
(defvar rubocop-config-paths '("monolith/.rubocop.yml" ".rubocop.yml"))

(defun rubocop-detect-config ()
  (when rubocop-config-paths
    (let ((root (rubocop-project-root 'no-error)))
      (car (delq nil
                 (mapcar (lambda (f)
                           (let ((pconfig (expand-file-name f root)))
                             (when (file-exists-p pconfig) pconfig)))
                         rubocop-config-paths))))))

(defun rubocop-build-command (command path)
  "Build the full command to be run based on COMMAND and PATH.
The command will be prefixed with `bundle exec` if RuboCop is bundled."
  (concat
   (if (and (not rubocop-prefer-system-executable) (rubocop-bundled-p)) "bundle exec " "")
   command
   (if-let ((config-file (rubocop-detect-config)))
       (concat " --config " config-file " ")
     "")
   (rubocop-build-requires)
   " "
   path))

(provide 'clgc-ruby)
