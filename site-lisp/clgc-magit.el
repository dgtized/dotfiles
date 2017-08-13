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

;; modified from https://github.com/kyleam/emacs.d/blob/master/lisp/km-hydra.el
(defhydra hydra-smerge (:hint nil)
  "
_b_ keep base  (middle)  _d_ diff     _n_ next
_u_ keep upper (mine)    _e_ ediff    _p_ previous
_l_ keep lower (other)   _h_ refine   _C_ combine with next
_a_ keep all             _r_ resolve  _c_ keep current
\n"
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("n" smerge-next)
  ("p" smerge-prev)
  ("C" smerge-combine-with-next)
  ("c" smerge-keep-current)
  ("h" smerge-refine)
  ("r" smerge-resolve)
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
