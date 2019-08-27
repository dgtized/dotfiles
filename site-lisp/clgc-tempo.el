(require 'tempo)
;; (setq tempo-interactive t)

(tempo-define-template "org-digraph"
                       '(> "#+begin_src dot :file " (p "Filename:") ".png" n
                           > "digraph G {" n
                           > r> n>
                           "}" n
                           > "#+end_src" n)
                       "org-digraph"
                       "Insert a dot digraph src block with filename")

(define-abbrev org-mode-abbrev-table "dotgraph" "" 'tempo-template-org-digraph)

(provide 'clgc-tempo)
