(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(erc-nick "dgtized")
 '(global-magit-wip-save-mode nil)
 '(js2-auto-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-indent-on-enter-key nil)
 '(js2-use-ast-for-indentation-p t)
 '(magit-completing-read-function (quote magit-builtin-completing-read))
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(magit-diff-options (quote ("--patience")))
 '(magit-set-upstream-on-push (quote dontask))
 '(magit-wip-mode nil)
 '(rspec-command-options "--format documentation --profile 10")
 '(rspec-use-opts-file-when-available nil)
 '(rspec-use-rvm t)
 '(web-mode-markup-indent-offset 2)
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-x-prompt yas-dropdown-prompt yas-completing-prompt yas-no-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t (:inherit highlight :underline nil)))))

