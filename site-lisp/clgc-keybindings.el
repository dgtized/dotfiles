;;; clgc-keybindings -- keybindings for clgc
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-x E") 'apply-macro-to-region-lines)
(global-set-key (kbd "C-x p") 'proced)

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(global-set-key (kbd "C-c 7") 'insert-char)

(global-unset-key (kbd "C-z")) ;; use C-x C-z instead

(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-\\") 'align-regexp)
(global-set-key (kbd "C-x \\") 'align)
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)
(global-set-key (kbd "C-M-y") 'duplicate-line-or-region)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c W") 'whitespace-mode)
(global-set-key (kbd "C-c V") 'visual-line-mode)
(global-set-key (kbd "C-c I") 'color-identifiers-mode)
(global-set-key (kbd "C-M-;") 'comment-dwim-line)

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-j") 'join-line)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-c j") 'join-line)

;; Org Related
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c ;") 'org-capture)

(eval-after-load 'org-mode
  '(progn
     (define-key org-mode-map (kbd "C-c ;") nil)
     (define-key org-mode-map (kbd "M-;") 'org-toggle-comment)))

;; Git related (really wish I could move this into VC prefix)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-blame-mode)
(global-set-key (kbd "C-c B") 'github-browse-file-blame)
(global-set-key (kbd "C-c L") 'magit-file-log)
(global-set-key (kbd "C-c G") 'github-browse-file)
(global-set-key (kbd "C-c Q") 'clgc-gist-region)

(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c g") 'find-grep-in-project)

(global-set-key (kbd "C-c i") 'ido-goto-symbol)
(global-set-key (kbd "C-c t") 'visit-term-buffer)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c o") 'browse-url)

(global-set-key (kbd "C-'") 'ace-jump-mode)
(global-set-key (kbd "C-M-'") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "C-c R") 'rename-file-and-buffer)

(let ((map occur-mode-map))
  (define-key map (kbd "v") 'occur-mode-display-occurrence)
  (define-key map (kbd "p") 'occur-prev)
  (define-key map (kbd "n") 'occur-next))

(global-set-key (kbd "C-x C-]") 'abort-recursive-edit) ; C-] overriden below
(let ((map smartparens-mode-map))
  (define-key map (kbd "C-(") 'sp-backward-slurp-sexp)
  (define-key map (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key map (kbd "C-{") 'sp-backward-barf-sexp)
  (define-key map (kbd "C-}") 'sp-forward-barf-sexp)
  (define-key map (kbd "M-<delete>") 'sp-kill-symbol)
  (define-key map (kbd "M-<backspace>") 'sp-backward-kill-symbol)
  (define-key map (kbd "<delete>") 'sp-delete-char)
  (define-key map (kbd "ESC <backspace>") 'sp-splice-sexp-killing-backward)
  (define-key map (kbd "ESC <delete>") 'sp-splice-sexp-killing-forward)
  (define-key map (kbd "M-s <backspace>") 'sp-backward-unwrap-sexp)
  (define-key map (kbd "M-s <delete>") 'sp-unwrap-sexp)
  (define-key map (kbd "C-M-t") 'sp-transpose-sexp)
  (define-key map (kbd "M-s r") 'sp-rewrap-sexp)
  (define-key map (kbd "M-s a") 'sp-absorb-sexp)
  (define-key map (kbd "M-s e") 'sp-emit-sexp)
  (define-key map (kbd "M-s s") 'sp-split-sexp)
  (define-key map (kbd "M-s j") 'sp-join-sexp)
  (define-key map (kbd "M-s c") 'sp-convolute-sexp))

(define-key help-map (kbd "P") 'find-library)
(define-key emacs-lisp-mode-map (kbd "C-c C-v") 'eval-buffer)

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

(eval-after-load 'gist
  '(progn (define-key gist-list-menu-mode-map (kbd "b") 'clgc-gist-browse)))

(eval-after-load 'smartscan
  '(let ((map smartscan-map))
     (define-key map (kbd "M-n") nil)
     (define-key map (kbd "M-p") nil)
     (define-key map (kbd "M-'") nil)
     (define-key map (kbd "M-s n") 'smartscan-symbol-go-forward)
     (define-key map (kbd "M-s p") 'smartscan-symbol-go-backward)
     (define-key map (kbd "M-s %") 'smartscan-symbol-replace)))

(windmove-default-keybindings)

;; isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
           (regexp-quote isearch-string))))))

(provide 'clgc-keybindings)
