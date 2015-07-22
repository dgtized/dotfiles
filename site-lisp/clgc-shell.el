(require 'eshell)
(require 'em-term)
(require 'em-smart)
(require 'em-hist)
(require 'company)

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook 'eshell-smart-initialize)
(add-to-list 'eshell-visual-commands "tmux")

(setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

(defun company-eshell-history (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell-history))
    (prefix (and (eq major-mode 'eshell-mode)
                 (let ((word (company-grab-word)))
                   (save-excursion
                     (eshell-bol)
                     (and (looking-at-p (s-concat word "$")) word)))))
    (candidates (remove-duplicates
                 (->> (ring-elements eshell-history-ring)
                      (remove-if-not (lambda (item) (s-prefix-p arg item)))
                      (mapcar 's-trim))
                 :test 'string=))
    (sorted t)))

(add-to-list 'company-backends 'company-eshell-history)

(provide 'clgc-shell)
