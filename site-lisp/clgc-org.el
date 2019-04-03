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
(setq org-src-fontify-natively t
      org-startup-with-inline-images t
      org-startup-with-latex-preview t
      org-babel-sh-command "bash")

;; Force redisplay after each execute so images update inline
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

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

;; for graphviz-dot-mode
(setq-default tab-width 2)

(require 'sql)
(require 'ob-sql)
(require 'ox-md)

(let ((path (expand-file-name "~/usr/plantuml.jar")))
  (setq org-plantuml-jar-path path
        plantuml-jar-path path))

(setq-default org-download-image-dir "~/org/downloads")
(require 'org-download)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(require 'ox-reveal)

(provide 'clgc-org)
