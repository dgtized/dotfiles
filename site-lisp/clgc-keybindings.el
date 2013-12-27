(global-set-key [f11] 'toggle-fullscreen)

(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-x E") 'apply-macro-to-region-lines)

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(global-set-key (kbd "C-c 7") 'insert-char)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key [f6] 'magit-status)
(global-set-key [f8] 'eshell)

(global-unset-key (kbd "C-z")) ;; use C-x C-z instead

(global-unset-key (kbd "M-x")) ;; use one below
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-c j") 'just-one-space)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c q") 'comment-dwim)

(global-set-key (kbd "C-c R") 'rename-file-and-buffer)
(global-set-key (kbd "C-x t") 'visit-term-buffer)
(global-set-key (kbd "C-c d") 'ediff-windows-linewise)
(global-set-key (kbd "C-c D") 'ediff-windows-wordwise)
(global-set-key (kbd "C-c s") 'isearch-other-window)

(global-set-key (kbd "C-c G") 'github-browse)

;; (global-set-key (kbd "C-.") 'find-file-in-project)
;; (global-set-key (kbd "C-,") 'find-grep-in-project)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "<f9>") 'org-tree-slide-mode)
(global-set-key (kbd "M-<f9>") 'org-tree-slide-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key ctl-x-4-map (kbd "t") 'transpose-windows)
(windmove-default-keybindings)

;; isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
           (regexp-quote isearch-string))))))

(provide 'clgc-keybindings)
