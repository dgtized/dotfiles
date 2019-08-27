(require 'tempo)
;; (setq tempo-interactive t)

(tempo-define-template "org-digraph"
                       '(> "#+begin_src dot :file " (p "Filename: ") ".png\n"
                           > "digraph G {\n"
                           > "}\n"
                           > "#+end_src\n"))

(provide 'clgc-tempo)
