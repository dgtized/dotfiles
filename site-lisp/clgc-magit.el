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

(provide 'clgc-magit)
