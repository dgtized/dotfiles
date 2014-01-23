;;; clgc-keybindings -- keybindings for clgc
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-x E") 'apply-macro-to-region-lines)

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(global-set-key (kbd "C-c 7") 'insert-char)

(global-unset-key (kbd "C-z")) ;; use C-x C-z instead

(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-c \\") 'align)
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)
(global-set-key (kbd "C-M-y") 'duplicate-line-or-region)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c W") 'whitespace-mode)
(global-set-key (kbd "C-c V") 'visual-line-mode)
(global-set-key (kbd "C-M-;") 'comment-dwim-line)
(global-set-key (kbd "C-c n") 'sp-select-next-thing-exchange)
(global-set-key (kbd "C-c r") 'sp-rewrap-sexp)
(global-set-key (kbd "C-c s") 'sp-splice-sexp)

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-j") 'join-line)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-c j") 'join-line)

;; Git related (really wish I could move this into VC prefix)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-blame-mode)
(global-set-key (kbd "C-c B") 'github-browse-file-blame)
(global-set-key (kbd "C-c l") 'magit-file-log)
(global-set-key (kbd "C-c G") 'github-browse-file)

(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c g") 'find-grep-in-project)

(global-set-key (kbd "C-c i") 'ido-goto-symbol)
(global-set-key (kbd "C-c t") 'visit-term-buffer)
(global-set-key (kbd "C-c e") 'eshell)

(global-set-key (kbd "C-'") 'ace-jump-mode)
(global-set-key (kbd "C-M-'") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "C-c R") 'rename-file-and-buffer)

(let ((map occur-mode-map))
  (define-key map (kbd "v") 'occur-mode-display-occurrence)
  (define-key map (kbd "p") 'occur-prev)
  (define-key map (kbd "n") 'occur-next))

(let ((map smartparens-mode-map))
  (define-key map (kbd "C-]") nil) ;; don't override abort recursive edit
  (define-key map (kbd "C-(") 'sp-backward-slurp-sexp)
  (define-key map (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key map (kbd "C-{") 'sp-backward-barf-sexp)
  (define-key map (kbd "C-}") 'sp-forward-barf-sexp)
  (define-key map (kbd "M-<delete>") 'sp-kill-symbol)
  (define-key map (kbd "M-<backspace>") 'sp-backward-kill-symbol)
  (define-key map (kbd "<delete>") 'sp-delete-char)
  (define-key map (kbd "M-<up>") 'sp-splice-sexp-killing-backward)
  (define-key map (kbd "M-<down>") 'sp-splice-sexp-killing-forward)
  (define-key map (kbd "M-r") 'sp-raise-sexp)
  (define-key map (kbd "C-M-t") 'sp-transpose-sexp)
  (define-key map (kbd "C-M-j") 'sp-split-sexp)
  (define-key map (kbd "M-J") 'sp-join-sexp)
  (define-key map (kbd "M-?") 'sp-convolute-sexp))

(define-key emacs-lisp-mode-map (kbd "C-c C-v") 'eval-buffer)

(global-set-key (kbd "<f8>") 'calc-dispatch)
(global-set-key (kbd "<f9>") 'org-tree-slide-mode)
(global-set-key (kbd "M-<f9>") 'org-tree-slide-mode)
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key ctl-x-4-map (kbd "t") 'transpose-windows)
(define-key ctl-x-4-map (kbd "e") 'ediff-other-window)
(define-key ctl-x-4-map (kbd "s") 'isearch-other-window)

(windmove-default-keybindings)

;; isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
           (regexp-quote isearch-string))))))

(provide 'clgc-keybindings)
