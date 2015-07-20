(require 'eshell)
(require 'em-term)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook 'eshell-smart-initialize)
(add-to-list 'eshell-visual-commands "tmux")

(setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

(provide 'clgc-shell)
