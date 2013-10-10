(global-set-key [f11] 'toggle-fullscreen)

(global-set-key "\C-xO" (lambda () (interactive) (other-window -1)))
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

(global-set-key "\C-xE" 'apply-macro-to-region-lines)

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(global-set-key "\C-c7" 'insert-char)

(global-set-key "\C-xg" 'magit-status)
(global-set-key [f6] 'magit-status)
(global-set-key [f8] 'eshell)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "C-c j") 'just-one-space)
(global-set-key "\C-cw" 'whitespace-cleanup)
(global-set-key "\C-cq" 'comment-dwim)

(global-set-key "\C-cR" 'rename-file-and-buffer)
(global-set-key "\C-ct" 'visit-term-buffer)
(global-set-key "\C-cd" 'ediff-windows-linewise)
(global-set-key "\C-cD" 'ediff-windows-wordwise)
(global-set-key "\C-cs" 'isearch-other-window)

(global-set-key (kbd "C-.") 'find-file-in-project)
(global-set-key (kbd "C-,") 'find-grep-in-project)

(global-set-key "\M-T" 'transpose-sexps)

(global-set-key (kbd "<f9>") 'org-tree-slide-mode)
(global-set-key (kbd "M-<f9>") 'org-tree-slide-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

(windmove-default-keybindings)

;; isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
           (regexp-quote isearch-string))))))

(provide 'clgc-keybindings)
