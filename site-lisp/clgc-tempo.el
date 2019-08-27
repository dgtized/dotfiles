(require 'tempo)
;; (setq tempo-interactive t)

(tempo-define-template "org-digraph"
                       '(> "#+begin_src dot :file " (p "Filename: ") ".png\n"
                           > "digraph G {\n"
                           > "}\n"
                           > "#+end_src\n"))

(define-abbrev org-mode-abbrev-table "dotgraph" "" 'tempo-template-org-digraph)

(provide 'clgc-tempo)
