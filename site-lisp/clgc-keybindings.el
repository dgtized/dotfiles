;;; clgc-keybindings -- keybindings for clgc
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-x E") 'apply-macro-to-region-lines)
(global-set-key (kbd "C-x p") 'proced)
(global-set-key (kbd "C-x P") 'prodigy)

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(global-set-key (kbd "C-c 7") 'insert-char)

(global-set-key (kbd "C-z") 'repeat) ;; use C-x C-z for suspend

(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x B") 'bury-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-\\") 'ace-window)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "S-<f5>") 'revert-this-buffer)
(global-set-key (kbd "C-<f5>") 'ansi-color-apply-buffer)
(global-set-key (kbd "C-c 6") 'evil-mode)

(global-set-key (kbd "C-x C-\\") 'align-regexp)
(global-set-key (kbd "C-x \\") 'align)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c W") 'whitespace-mode)
(global-set-key (kbd "C-c n") 'crux-cleanup-buffer-or-region)
(global-set-key (kbd "C-c $") 'crux-ispell-word-then-abbrev)
(global-set-key (kbd "C-c V") 'visual-line-mode)
(global-set-key (kbd "C-c I") 'color-identifiers-mode)
(global-set-key (kbd "C-c L") 'linum-mode)
(crux-with-region-or-line comment-or-uncomment-region)
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-M-j") 'join-line)

;; Projectile
(global-set-key (kbd "C-.") 'helm-projectile)
(global-set-key (kbd "C-,") 'helm-projectile-ag)

(global-set-key (kbd "ESC ESC g") 'projectile-rails-mode-goto-map)
(global-set-key (kbd "ESC ESC r") 'projectile-rails-mode-run-map)

(define-prefix-command 'menu-map)
(eval-after-load 'projectile
  '(set-keymap-parent 'menu-map 'projectile-command-map))

(global-set-key (kbd "<menu>") 'menu-map)

(define-key 'menu-map (kbd "y") 'helm-show-kill-ring)
(define-key 'menu-map (kbd "x") 'helm-M-x)
(define-key 'menu-map (kbd "TAB") 'ace-window)
(define-key 'menu-map (kbd "<menu>") 'helm-M-x)

;; Org Related
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

(eval-after-load 'graphviz-dot-mode
  '(define-key graphviz-dot-mode-map (kbd "<f5>") 'graphviz-dot-preview))

(eval-after-load 'org
  '(let ((map org-mode-map))
    (define-key map (kbd "C-'") nil) ;; overlaps ace-jump-mode
    (define-key map (kbd "<f9>") 'epresent-run)
    (define-key map (kbd "C-<f9>") 'org-tree-slide-mode)
    (define-key map (kbd "S-<f9>") 'org-tree-slide-skip-done-toggle)
    (define-key map (kbd "M-n") 'outline-next-visible-heading)
    (define-key map (kbd "M-p") 'outline-previous-visible-heading)))

;; Git related
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(define-key git-commit-mode-map (kbd "C-c k") 'markdown-insert-gfm-code-block)

(let ((map vc-prefix-map))
  (define-key map (kbd "S") 'vc-switch-backend) ; rebind from b
  (define-key map (kbd "b") 'magit-blame)
  (define-key map (kbd "B") 'github-browse-file-blame)
  (define-key map (kbd "f") 'magit-log-buffer-file)
  (define-key map (kbd "F") 'github-browse-file)
  (define-key map (kbd "C") 'github-browse-commit)
  (define-key map (kbd "j") 'jenkins-visit-branch))

(global-set-key (kbd "C-c Q") 'clgc-gist-region)
(eval-after-load 'magit
  '(define-key magit-mode-map "#"
     #'endless/visit-pull-request-url))

(eval-after-load 'gist
  '(progn (define-key gist-list-menu-mode-map (kbd "b") 'clgc-gist-browse)))

(global-set-key (kbd "C-c t") 'visit-term-buffer)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c B") 'browse-url)
(global-set-key (kbd "C-c J") 'webjump)
(global-set-key (kbd "C-c R") 'crux-rename-buffer-and-file)

(eval-after-load 'compile
  '(let ((map compilation-mode-map))
     (define-key map (kbd "n") 'compilation-next-error)
     (define-key map (kbd "p") 'compilation-previous-error)))

(let ((map occur-mode-map))
  (define-key map (kbd "v") 'occur-mode-display-occurrence)
  (define-key map (kbd "p") 'occur-prev)
  (define-key map (kbd "n") 'occur-next))

(global-set-key (kbd "C-M-g") 'abort-recursive-edit) ; C-] overriden below
(let ((map smartparens-mode-map))
  (define-key map (kbd "C-(") 'sp-backward-slurp-sexp)
  (define-key map (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key map (kbd "C-{") 'sp-backward-barf-sexp)
  (define-key map (kbd "C-}") 'sp-forward-barf-sexp)
  (define-key map (kbd "C-M-<up>") 'sp-backward-up-sexp)
  (define-key map (kbd "C-M-<down>") 'sp-down-sexp)
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
  (define-key map (kbd "M-s c") 'sp-convolute-sexp)
  (define-key map (kbd "M-s [") 'sp-add-to-previous-sexp)
  (define-key map (kbd "M-s ]") 'sp-add-to-next-sexp))

(define-key help-map (kbd "C-l") 'find-library)
(define-key emacs-lisp-mode-map (kbd "C-c C-v") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'visit-ielm)

(define-key emacs-lisp-mode-map (kbd "<f5>") 'ert-silently)
(define-key lisp-interaction-mode-map (kbd "<f5>") 'ert-silently)

(setq alchemist-key-command-prefix (kbd "C-c ."))
(eval-after-load 'alchemist-mode
  '(progn (define-key alchemist-mode-map (kbd "C-c C-c") 'alchemist-compile-this-buffer)))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c M-h") 'clojure-cheatsheet)
     (define-key clojure-mode-map (kbd "<f5>") 'cider-test-run-tests)))

(global-set-key (kbd "C-<f10>") 'menu-bar-mode)
(global-set-key (kbd "C-<f11>") 'clgc-toggle-monitor)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-+") 'er/expand-region)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Multiple Cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(let ((map search-map))
  (define-key map (kbd "ml") 'mc/edit-lines)
  (define-key map (kbd "mr") 'mc/mark-all-in-region)
  (define-key map (kbd "mA") 'mc/mark-all-like-this)
  (define-key map (kbd "m SPC") 'mc/mark-all-like-this-dwim)
  (define-key map (kbd "mx") 'mc/mark-more-like-this-extended)
  (define-key map (kbd "ms") 'mc/mark-all-symbols-like-this)
  (define-key map (kbd "md") 'mc/mark-all-symbols-like-this-in-defun)
  (define-key map (kbd "mp") 'mc/mark-sgml-tag-pair)
  (define-key map (kbd "M-s") 'mc/sort-regions)
  (define-key map (kbd "M-r") 'mc/reverse-regions)
  (define-key map (kbd "#") 'mc/insert-numbers))

(define-key ctl-x-4-map (kbd "t") 'crux-transpose-windows)
(define-key ctl-x-4-map (kbd "e") 'ediff-other-window)
(define-key ctl-x-4-map (kbd "s") 'isearch-other-window)
(define-key ctl-x-4-map (kbd "i") 'projectile-find-implementation-or-test-other-window)

(eval-after-load 'ruby-mode
  '(progn (define-key ruby-mode-map (kbd "<f5>") 'rspec-verify)
          (define-key ruby-mode-map (kbd "<f6>") 'clgc-ruby-compile-this-buffer)
          (define-key ruby-mode-map (kbd "C-c C-c") 'clgc-ruby-compile-this-buffer)
          (define-key ruby-mode-map (kbd "C-c v") 'chruby-use-corresponding)
          (define-key ruby-mode-map (kbd "S-<f6>") 'coverage-mode)))

(eval-after-load 'feature-mode
  '(define-key feature-mode-map (kbd "<f5>") 'feature-verify-all-scenarios-in-buffer))

(global-set-key (kbd "M-N") 'smartscan-symbol-go-forward)
(global-set-key (kbd "M-P") 'smartscan-symbol-go-backward)
(global-set-key (kbd "M-s %") 'smartscan-symbol-replace)

(windmove-default-keybindings)

(global-set-key (kbd "<f12>") 'ace-window)
(global-set-key (kbd "C-c j") 'avy-goto-char)
(global-set-key (kbd "C-c h") 'avy-pop-mark)
(global-set-key (kbd "C-'") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-M-'") 'avy-pop-mark)

(require 'multiple-cursors)
(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
(global-set-key (kbd "M-m") 'iy-go-up-to-char)
(global-set-key (kbd "M-M") 'iy-go-up-to-char-backward)

(global-set-key (kbd "M-z") 'zop-to-char)
(global-set-key (kbd "C-M-z") 'zap-to-char)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'crux-move-beginning-of-line)

(global-set-key (kbd "M-SPC") 'cycle-spacing)

(eval-after-load 'company
  '(global-set-key (kbd "C-c y") 'company-yasnippet))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(provide 'clgc-keybindings)
