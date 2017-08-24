(require 'org)

(autoload 'org-tree-slide-mode "org-tree-slide" t)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

(setq org-directory (expand-file-name "~/org")
      org-default-notes-file (expand-file-name "incoming.org" org-directory)
      org-completion-use-ido t
      org-return-follows-link t
      org-use-fast-todo-selection t
      org-refile-use-outline-path t
      org-confirm-babel-evaluate nil ;; DANGEROUS: allows babel evaluation by default
      org-log-done t)

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE")
                          (sequence "WAITING(w)" "|" "CANCELED(c)")))

(setq org-agenda-files (mapcar (lambda (x) (expand-file-name x org-directory))
                               (list "incoming.org" "OpenSource.org"
                                     "NoRedInk.org")))

(setq org-refile-targets
      (quote ((nil :maxlevel . 2)
              (org-agenda-files :maxlevel . 2)))
      org-capture-templates
      (quote (("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
               "* TODO %?\n %U\n  %i\n %a")
              ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
               "* %?\n %U\n  %i\n")
              ("n" "Note" entry (file+headline org-default-notes-file "Notes")
               "* %?\n %U\n  %i\n %a"))))

;; Font highlight babel blocks
(setq org-src-fontify-natively t)

(defun turn-on-org-show-all-inline-images ()
  (org-display-inline-images t t))

(add-hook 'org-mode-hook 'turn-on-org-show-all-inline-images)
(defun clgc-org-fix-inline-images ()
  (when org-inline-image-overlays
    (org-display-inline-images)))
(add-hook 'org-babel-after-execute-hook 'clgc-org-fix-inline-images)

(setq org-babel-sh-command "bash")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (dot . t)
   (emacs-lisp . t)
   (elixir . t)
   (gnuplot . t)
   (http . t)
   (python . t)
   (ruby . t)
   (shell . t)
   (sml . t)
   (sql . t)
   (plantuml . t)))

(defun org-md-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "```\n%s```\n"
   (org-remove-indentation
    (org-export-format-code-default example-block info))))

(setq org-babel-clojure-backend 'cider)
(require 'cider)

(require 'sql)
(require 'ob-sql)
(require 'ox-md)

(setq org-plantuml-jar-path (expand-file-name "~/usr/plantuml.jar"))

(provide 'clgc-org)
