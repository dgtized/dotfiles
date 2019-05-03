(require 'magit)

(setq git-commit-setup-hook '(git-commit-save-message
                              git-commit-setup-changelog-support
                              git-commit-turn-on-auto-fill
                              git-commit-turn-on-flyspell
                              git-commit-propertize-diff
                              with-editor-usage-message)
      git-commit-summary-max-length 60)

(setq global-magit-file-mode t
      magit-refresh-verbose t
      magit-auto-revert-immediately t
      magit-auto-revert-mode t
      magit-completing-read-function 'ivy-completing-read
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
(defhydra hydra-smerge (:hint nil :pre (smerge-mode 1))
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
  ("L" recenter-top-bottom "recenter")
  ("u" undo "undo")
  ("q" nil "quit"))

(provide 'clgc-magit)
