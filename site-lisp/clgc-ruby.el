(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

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
  ;; (make-variable-buffer-local 'compilation-error-regexp-alist)
  ;; (add-to-list 'compilation-error-regexp-alist
  ;;       '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:" 1 2))
  ;; (add-to-list 'compilation-error-regexp-alist
  ;;       '("\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 2 3))
  ;;(make-variable-buffer-local 'compile-command)
  ;;(setq compile-command (concat "ruby -w " (buffer-file-name) " "))
  ;; (local-set-key "\C-cr" 'ruby-eval-buffer)
  (require 'rinari)
  (global-rinari-mode t)

  (c-add-style
   "ruby"
   '("bsd"
     (c-basic-offset . 4)
     (c-offsets-alist
      (case-label . 2)
      (label . 2)
      (statement-case-intro . 2)
      )))
  ;; from: http://shylock.uw.hu/Emacs/ruby-electric.el
  ;(require 'ruby-electric)
  ;(ruby-electric-mode)
  ;(abbrev-mode 1)
  (define-key ruby-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map "\C-j" 'newline)
  (require 'ruby-mode-expansions)
  (define-key ruby-mode-map (kbd "C-M-u") 'er/ruby-forward-up)
  (define-key ruby-mode-map (kbd "C-M-d") 'er/ruby-backward-up)
  (require 'ruby-tools)
  (define-key ruby-tools-mode-map (kbd "C-:") 'ruby-toggle-symbol-string)
  (define-key ruby-tools-mode-map (kbd "C-\"") 'ruby-toggle-string-type)
  (define-key ruby-tools-mode-map (kbd "C-'") nil)
  (require 'nxml-mode)
  ;(require 'rhtml-mode)
  ;(require 'rails)

  ;; Keep ac-mode from trying to complete on an end
  (make-local-variable 'ac-ignores)
  (add-to-list 'ac-ignores "end")
)

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode) t)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(setq rinari-tags-file-name "TAGS")

(provide 'clgc-ruby)
