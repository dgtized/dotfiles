(require 'org)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

(setq org-directory (expand-file-name "~/Dropbox/org")
      org-default-notes-file (expand-file-name "incoming.org" org-directory)
      org-completion-use-ido t
      org-return-follows-link t
      org-log-done t)

(provide 'clgc-org)
