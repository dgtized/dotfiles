(defun ruby-eval-buffer () (interactive)
   "Evaluate the buffer with ruby."
   (shell-command-on-region (point-min) (point-max) "ruby -w "))

(eval-when-compile (require 'ruby-tools))
(defun ruby-tools-jump-to-inside-of-thing ()
  ;; jump out of back or into beginning of string/symbol
  (when (looking-at "[:'\"]") (forward-char))
  ;; jump into beginning of string/symbol if after
  (when (not (or (ruby-tools-symbol-at-point-p)
                 (ruby-tools-string-at-point-p)))
    (ruby-backward-sexp))
  ;; but we still need to be inside to trigger the toggle
  (when (looking-at "[:'\"]") (forward-char)))

(defun ruby-toggle-symbol-string ()
  (interactive)
  (save-excursion
    (ruby-tools-jump-to-inside-of-thing)
    (cond ((ruby-tools-symbol-at-point-p)
           (ruby-tools-to-double-quote-string))
          ((ruby-tools-string-at-point-p)
           (ruby-tools-to-symbol)))))

(defun ruby-toggle-string-type ()
  (interactive)
  (save-excursion
    (ruby-tools-jump-to-inside-of-thing)
    (if (looking-back "[':]" (1- (point)))
        (ruby-tools-to-double-quote-string)
      (ruby-tools-to-single-quote-string))))

(defun my-ruby-mode-hook ()
  ;; Keep ac-mode from trying to complete on an end
  (make-local-variable 'ac-ignores)
  (add-to-list 'ac-ignores "end")
  (require 'rinari)
  (setq rinari-tags-file-name "TAGS")
  (global-rinari-mode t)

  (define-key ruby-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map "\C-j" 'newline)
  (require 'ruby-mode-expansions)
  (define-key ruby-mode-map (kbd "C-M-u") 'er/ruby-forward-up)
  (define-key ruby-mode-map (kbd "C-M-d") 'er/ruby-backward-up)
  (require 'ruby-tools)
  (define-key ruby-tools-mode-map (kbd "C-:") 'ruby-toggle-symbol-string)
  (define-key ruby-tools-mode-map (kbd "C-\"") 'ruby-toggle-string-type)
  (define-key ruby-tools-mode-map (kbd "C-'") nil))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; Robe
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode 'robe-ac-setup)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(provide 'clgc-ruby)
