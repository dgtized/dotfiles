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
                                     "NoRedInk.org")))

(setq org-refile-targets (quote ((nil :maxlevel . 2)
                                 (org-agenda-files :maxlevel . 2))))

;; Font highlight babel blocks
(setq org-src-fontify-natively t)

(setq org-babel-sh-command "bash")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (emacs-lisp . t)
   (elixir . t)
   (http . t)
   (python . t)
   (ruby . t)
   (sh . t)
   (sml . t)
   (sql . t)))

(defun org-md-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "```\n%s```\n"
   (org-remove-indentation
    (org-export-format-code-default example-block info))))


(provide 'clgc-org)
