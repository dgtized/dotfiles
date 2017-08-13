(require 'magit)

(setq magit-auto-revert-immediately t
      magit-auto-revert-mode t
      magit-completing-read-function (quote magit-ido-completing-read)
      magit-push-always-verify nil
      magit-push-arguments (quote ("--set-upstream"))
      magit-save-repository-buffers (quote dontask))

;; disable magit-insert-tags as it's a performance hog for magit-refs
(setq magit-refs-sections-hook
      '(magit-insert-error-header
        magit-insert-branch-description
        magit-insert-local-branches
        magit-insert-remote-branches
        ;; magit-insert-tags
        ))

;; stolen from https://github.com/kyleam/emacs.d/blob/master/lisp/km-hydra.el
(defhydra hydra-smerge (:hint nil)
  "
_b_ keep base    _d_ diff     _n_ next
_m_ keep mine    _e_ ediff    _p_ previous
_o_ keep other   _h_ refine
_a_ keep all
\n"
  ("b" smerge-keep-base)
  ("m" smerge-keep-mine)
  ("o" smerge-keep-other)
  ("a" smerge-keep-all)
  ("n" smerge-next)
  ("p" smerge-prev)
  ("h" smerge-refine)
  ("e" smerge-ediff :color blue)
  ("d" (call-interactively
        (pcase (read-char-choice
                "< base-mine, > base-other, = mine-other"
                (list ?< ?> ?=))
          (?< #'smerge-diff-base-mine)
          (?> #'smerge-diff-base-other)
          (?= #'smerge-diff-mine-other))))
  ("l" recenter-top-bottom "recenter")
  ("u" undo "undo")
  ("q" nil "quit"))

(provide 'clgc-magit)
