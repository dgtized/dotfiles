(global-set-key [f11] 'toggle-fullscreen)

(global-set-key "\C-xO" (lambda () (interactive) (other-window -1)))
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

(global-set-key "\C-xE" 'apply-macro-to-region-lines)

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(global-set-key "\C-c7" 'ucs-insert)

(global-set-key "\C-xg" 'magit-status)
(global-set-key [f6] 'magit-status)
(global-set-key [f8] 'eshell)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-cw" 'whitespace-cleanup)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)

(global-set-key "\C-cr" 'rename-file-and-buffer)

(global-set-key [remap find-tag] 'ido-find-tag)
(global-set-key (kbd "C-.") 'find-file-in-project)
(global-set-key (kbd "C-,") 'find-grep-in-project)

(global-set-key "\M-T" 'transpose-sexps)

(global-set-key (kbd "<f9>") 'org-tree-slide-mode)
(global-set-key (kbd "M-<f9>") 'org-tree-slide-mode)

;; (global-set-key "\C-cc" 'mode-compile)
(global-set-key [f5] 'mode-compile)
;; (global-set-key "\C-ck" 'mode-compile-kill)

(global-set-key (kbd "C-=") 'er/expand-region)

(windmove-default-keybindings)

(provide 'clgc-key-bindings)
