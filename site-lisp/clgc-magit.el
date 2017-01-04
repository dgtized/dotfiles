(require 'magit)

(setq magit-auto-revert-immediately t
      magit-auto-revert-mode t
      magit-completing-read-function (quote magit-ido-completing-read)
      magit-push-always-verify nil
      magit-push-arguments (quote ("--set-upstream"))
      magit-save-repository-buffers (quote dontask))

(provide 'clgc-magit)
