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
      org-use-fast-todo-selection t
      org-refile-use-outline-path t
      org-log-done t)

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE")
                          (sequence "WAITING(w)" "|" "CANCELED(c)")))

(setq org-agenda-files (mapcar (lambda (x) (expand-file-name x org-directory))
                               (list "incoming.org" "OpenSource.org"
                                     "NoRedInk.org" "Intoximeters.org")))

(setq org-refile-targets (quote ((nil :maxlevel . 2)
                                 (org-agenda-files :maxlevel . 2))))

;; Font highlight babel blocks
(setq org-src-fontify-natively t)

(provide 'clgc-org)
