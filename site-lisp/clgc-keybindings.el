;;; clgc-keybindings -- keybindings for clgc

;; TODO: make sue of context-menu-functions for mouse actions as described in:
;; https://ruzkuku.com/texts/emacs-mouse.html

(keymap-global-set "C-x O" (lambda () (interactive) (other-window -1)))

(keymap-global-set "C-x E" 'apply-macro-to-region-lines)
(keymap-global-set "C-x p" 'proced)
(keymap-global-set "C-x P" 'prodigy)

(keymap-global-set "C-z" 'repeat) ;; use C-x C-z for suspend

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(keymap-global-set "C-c 7" 'insert-char)

(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "C-x B" 'bury-buffer)
(keymap-global-set "C-x k" 'kill-current-buffer)
(keymap-global-set "C-\\" 'ace-window)
(keymap-global-set "M-/" 'hippie-expand)

(keymap-global-set "S-<f5>" 'revert-this-buffer)
(keymap-global-set "C-<f5>" 'ansi-color-apply-buffer)
(keymap-global-set "C-<f7>" 'redraw-display) ;; fix for wayland sticky update

(keymap-global-set "C-c 6" 'evil-mode)

(keymap-global-set "C-x C-\\" 'align-regexp)
(keymap-global-set "C-x \\" 'align)
(keymap-global-set "C-c g" 'aggressive-indent-mode)
;; TODO: switch to duplicate-dwim from 29.1?
(keymap-global-set "C-c d" 'crux-duplicate-current-line-or-region)
(keymap-global-set "C-c w" 'whitespace-cleanup)
(keymap-global-set "C-c W" 'whitespace-mode)
(keymap-global-set "C-c n" 'crux-cleanup-buffer-or-region)
(keymap-global-set "C-c $" 'crux-ispell-word-then-abbrev)
(keymap-global-set "C-c V" 'visual-line-mode)
(keymap-global-set "C-c I" 'color-identifiers-mode)
(keymap-global-set "C-c C-u" 'string-inflection-all-cycle)
(keymap-global-set "C-c L" 'global-display-line-numbers-mode)
(crux-with-region-or-line comment-or-uncomment-region)
(keymap-global-set "C-M-;" 'comment-or-uncomment-region)
(keymap-global-set "C-S-k" 'kill-whole-line)
(keymap-global-set "C-M-j" 'join-line)

;; Projectile
(keymap-set projectile-mode-map "C-c p" 'projectile-command-map)
(keymap-global-set "C-." 'counsel-projectile)
(keymap-global-set "C-," 'counsel-projectile-ag)

(keymap-global-set "ESC ESC g" 'projectile-rails-mode-goto-map)
(keymap-global-set "ESC ESC r" 'projectile-rails-mode-run-map)

(define-prefix-command 'menu-map)
(with-eval-after-load 'projectile
  (progn
    (set-keymap-parent 'menu-map 'projectile-command-map)
    (keymap-set 'projectile-command-map "s f" 'projectile-ag-files)))

(keymap-global-set "<menu>" 'menu-map)

(let ((map menu-map))
  (keymap-set map "y" 'helm-show-kill-ring)
  (keymap-set map "x" 'helm-M-x)
  (keymap-set map "TAB" 'ace-window)
  (keymap-set map "<menu>" 'helm-M-x))

(keymap-global-set "C-x C-m" 'counsel-M-x)
(keymap-global-set "M-X" 'smex-major-mode-commands)
(keymap-global-set "C-c m" 'ivy-resume)

;; Org Related
(keymap-global-set "C-c l" 'org-store-link)
(keymap-global-set "C-c A" 'org-agenda)
(keymap-global-set "C-c C" 'org-capture)
(keymap-global-set "C-c b" 'org-switchb)

(with-eval-after-load 'graphviz-dot-mode
  (keymap-set graphviz-dot-mode-map "<f5>" 'graphviz-dot-preview))

(eval-when-compile (require 'org))
(with-eval-after-load 'org
  (let ((map org-mode-map))
    (keymap-set map "C-'" nil) ;; overlaps ace-jump-mode
    (keymap-set map "<f9>" 'epresent-run)
    (keymap-set map "C-<f9>" 'org-tree-slide-mode)
    (keymap-set map "S-<f9>" 'org-tree-slide-skip-done-toggle)
    (keymap-set map "C-<f9>" 'org-reveal-export-to-html-and-browse)
    (keymap-set map "M-n" 'outline-next-visible-heading)
    (keymap-set map "M-p" 'outline-previous-visible-heading)))

;; Git related
(keymap-global-set "C-x g" 'magit-status)
(keymap-global-set "C-x M-g" 'magit-dispatch-popup)

(with-eval-after-load 'git-commit-mode
  (keymap-set git-commit-mode-map "C-c k" 'markdown-insert-gfm-code-block))

(let ((map vc-prefix-map))
  (keymap-set map "S" 'vc-switch-backend) ; rebind from b
  (keymap-set map "b" 'magit-blame)
  (keymap-set map "B" 'github-browse-file-blame)
  (keymap-set map "f" 'magit-log-buffer-file)
  (keymap-set map "F" 'github-browse-file)
  (keymap-set map "C" 'github-browse-commit)
  (keymap-set map "j" 'jenkins-visit-branch))

(keymap-global-set "C-c Q" 'clgc-gist-region)
(eval-when-compile (require 'magit))
(with-eval-after-load 'magit
  (keymap-set magit-mode-map "#" #'endless/visit-pull-request-url))

(with-eval-after-load 'gist
  (keymap-set gist-list-menu-mode-map "b" 'clgc-gist-browse))

(keymap-global-set "C-c B" 'browse-url)
(keymap-global-set "C-c J" 'webjump)
(keymap-global-set "C-c R" 'crux-rename-buffer-and-file)
(keymap-global-set "C-c D" 'crux-delete-buffer-and-file)
(keymap-global-set "C-c V" 'crux-view-url)
(keymap-global-set "C-c F" 'crux-sudo-edit)

(keymap-set prog-mode-map "<f5>" 'compile)
(with-eval-after-load 'c-mode
  (keymap-set c-mode-map "C-c c" 'compile))
(with-eval-after-load 'compile
  (let ((map compilation-mode-map))
    (keymap-set map "v" 'compilation-display-error)
    (keymap-set map "n" 'compilation-next-error)
    (keymap-set map "p" 'compilation-previous-error)))

(let ((map occur-mode-map))
  (keymap-set map "v" 'occur-mode-display-occurrence))

(keymap-global-set "C-M-g" 'abort-recursive-edit) ; C-] overriden below
(let ((map smartparens-mode-map))
  (keymap-set map "C-(" 'sp-backward-slurp-sexp)
  (keymap-set map "C-)" 'sp-forward-slurp-sexp)
  (keymap-set map "C-{" 'sp-backward-barf-sexp)
  (keymap-set map "C-}" 'sp-forward-barf-sexp)
  (keymap-set map "C-M-<up>" 'sp-backward-up-sexp)
  (keymap-set map "C-M-<down>" 'sp-down-sexp)
  (keymap-set map "M-<delete>" 'sp-kill-symbol)
  (keymap-set map "M-<backspace>" 'sp-backward-kill-symbol)
  (keymap-set map "<delete>" 'sp-delete-char)
  (keymap-set map "ESC <backspace>" 'sp-splice-sexp-killing-backward)
  (keymap-set map "ESC <delete>" 'sp-splice-sexp-killing-forward)
  (keymap-set map "M-s <backspace>" 'sp-backward-unwrap-sexp)
  (keymap-set map "M-s <delete>" 'sp-unwrap-sexp)
  (keymap-set map "C-M-t" 'sp-transpose-sexp)
  (keymap-set map "M-s r" 'sp-rewrap-sexp)
  (keymap-set map "M-s a" 'sp-absorb-sexp)
  (keymap-set map "M-s e" 'sp-emit-sexp)
  (keymap-set map "M-s s" 'sp-split-sexp)
  (keymap-set map "M-s j" 'sp-join-sexp)
  (keymap-set map "M-s c" 'sp-convolute-sexp)
  (keymap-set map "M-s [" 'sp-add-to-previous-sexp)
  (keymap-set map "M-s ]" 'sp-add-to-next-sexp))

(let ((map help-map))
  (keymap-set map "C-l" 'find-library)
  (keymap-set map "f" 'helpful-callable) ;; counsel-describe-function
  (keymap-set map "v" 'helpful-variable) ;; counsel-describe-variable
  (keymap-set map "k" 'helpful-key) ;; describe-key
  (keymap-set map "y" 'helpful-at-point))

;; https://www.emacswiki.org/emacs/EvaluatingExpressions
(let ((map emacs-lisp-mode-map))
  (keymap-set map "C-c C-k" 'eval-buffer)
  (keymap-set map "C-c C-p" 'pp-eval-last-sexp)
  (keymap-set map "C-c :" 'pp-eval-expression)
  (keymap-set map "C-c C-z" 'visit-ielm)
  (keymap-set map "<f5>" 'ert-silently))

(keymap-set lisp-interaction-mode-map "<f5>" 'ert-silently)

(setq alchemist-key-command-prefix (kbd "C-c ."))
(with-eval-after-load 'alchemist-mode
  (keymap-set alchemist-mode-map "C-c C-c" 'alchemist-compile-this-buffer)
  (keymap-set alchemist-mode-map "C-x C-e" 'alchemist-iex-send-last-sexp))

(with-eval-after-load 'clojure-mode
  (let ((map clojure-mode-map))
    (keymap-set map "C-c M-h" 'clojure-cheatsheet)
    (keymap-set map "<f5>" 'cider-test-run-ns-tests)
    (keymap-set map "C-c k t" 'kaocha-runner-run-test-at-point)
    (keymap-set map "C-c k r" 'kaocha-runner-run-tests)
    (keymap-set map "C-c k a" 'kaocha-runner-run-all-tests)
    (keymap-set map "C-c k w" 'kaocha-runner-show-warnings)
    (keymap-set map "C-c k h" 'kaocha-runner-hide-windows)))

(with-eval-after-load 'cider-mode
  (let ((map cider-mode-map))
    (keymap-set map "C-c T" 'cider-auto-test-mode)
    (keymap-set map "M-." 'cider-find-var)
    (keymap-set map "M-," 'cider-pop-back)
    (keymap-set map "C-c ." 'cider-pprint-eval-last-sexp)
    (keymap-set map "C-x 4 M-." 'cider-find-dwim-other-window)
    (keymap-set map "C-c , g" 'cider-eval-to-test-example)))

(keymap-global-set "C-<f10>" 'menu-bar-mode)
(keymap-global-set "C-<f11>" 'clgc-toggle-monitor)

(keymap-global-set "C-=" 'er/expand-region)
(keymap-global-set "M-+" 'er/expand-region)

(keymap-global-set "C-s" 'isearch-forward-regexp)
(keymap-global-set "C-r" 'isearch-backward-regexp)
(keymap-global-set "C-M-s" 'isearch-forward)
(keymap-global-set "C-M-r" 'isearch-backward)
(keymap-set isearch-mode-map "M-s t" 'swiper-isearch-toggle)
(keymap-set swiper-map "M-s t" 'swiper-isearch-toggle)

;; Multiple Cursors
(keymap-global-set "C->" 'mc/mark-next-like-this)
(keymap-global-set "C-<" 'mc/mark-previous-like-this)
(let ((map search-map))
  (keymap-set map "m l" 'mc/edit-lines)
  (keymap-set map "m r" 'mc/mark-all-in-region)
  (keymap-set map "m A" 'mc/mark-all-like-this)
  (keymap-set map "m SPC" 'mc/mark-all-like-this-dwim)
  (keymap-set map "m x" 'mc/mark-more-like-this-extended)
  (keymap-set map "m s" 'mc/mark-all-symbols-like-this)
  (keymap-set map "m d" 'mc/mark-all-symbols-like-this-in-defun)
  (keymap-set map "m p" 'mc/mark-sgml-tag-pair)
  (keymap-set map "M-s" 'mc/sort-regions)
  (keymap-set map "M-r" 'mc/reverse-regions)
  (keymap-set map "#" 'mc/insert-numbers))

(let ((map ctl-x-4-map))
  (keymap-set map "t" 'crux-transpose-windows)
  (keymap-set map "e" 'ediff-other-window)
  (keymap-set map "s" 'isearch-other-window)
  (keymap-set map "i" 'projectile-find-implementation-or-test-other-window)
  (keymap-set map "M-." 'xref-find-definitions-other-window))

(with-eval-after-load 'ruby-mode
  (let ((map ruby-mode-map))
    (keymap-set map "<f5>" 'rspec-verify)
    (keymap-set map "<f6>" 'clgc-ruby-compile-this-buffer)
    (keymap-set map "<f7>" 'rubocop-check-current-file)
    (keymap-set map "C-c C-c" 'clgc-ruby-compile-this-buffer)
    (keymap-set map "C-c v" 'chruby-use-corresponding)
    (keymap-set map "S-<f6>" 'coverage-mode)
    (keymap-set map "C-c :" 'clgc-ruby-string->symbol)
    (keymap-set map "C-c #" 'ruby-toggle-hash-syntax)
    (keymap-set map "C-c C-u" 'string-inflection-ruby-style-cycle)))

(with-eval-after-load 'rspec-dired-mode
  (keymap-set rspec-dired-mode-map "<f5>" 'rspec-dired-verify))

(with-eval-after-load 'feature-mode
  (keymap-set feature-mode-map "<f5>" 'feature-verify-all-scenarios-in-buffer))

(keymap-global-set "M-N" 'smartscan-symbol-go-forward)
(keymap-global-set "M-P" 'smartscan-symbol-go-backward)
(keymap-global-set "M-s %" 'smartscan-symbol-replace)

(require 'symbol-overlay)
(let ((map symbol-overlay-map))
  (keymap-set map "o" 'symbol-overlay-remove-all)
  (keymap-set map "f" 'symbol-overlay-switch-forward)
  (keymap-set map "b" 'symbol-overlay-switch-backward)
  (fset 'symbol-overlay-map map))

(keymap-set symbol-overlay-mode-map "C-c o" 'symbol-overlay-map)
(keymap-global-set "C-O" 'symbol-overlay-mode)

(windmove-default-keybindings)

(keymap-global-set "<f12>" 'ace-window)
(keymap-global-set "C-c j" 'avy-goto-char)
(keymap-global-set "C-c h" 'avy-pop-mark)
(keymap-global-set "C-'" 'avy-goto-word-or-subword-1)
(keymap-global-set "C-M-'" 'avy-pop-mark)

(require 'multiple-cursors)
;; (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
;; (keymap-global-set "M-m" 'iy-go-up-to-char)
;; (keymap-global-set "M-M" 'iy-go-up-to-char-backward)

(keymap-global-set "M-z" 'zap-up-to-char)

;; FIXME: what is keymap equivalent for remap?
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'crux-move-beginning-of-line)

(with-eval-after-load 'company
  (keymap-global-set "C-c y" 'company-yasnippet))

(keymap-set isearch-mode-map "C-o" 'isearch-occur)

(with-eval-after-load 'haskell-mode
  (let ((map haskell-mode-map))
    (keymap-set map "C-c C-l" 'hask-process-load-or-reload)
    (keymap-set map "C-`" 'haskell-interactive-bring)
    (keymap-set map "C-c C-t" 'haskell-process-do-type)
    (keymap-set map "C-c C-i" 'haskell-process-do-info)
    (keymap-set map "C-c C-c" 'haskell-process-cabal-build)
    (keymap-set map "C-c C-k" 'haskell-interactive-mode-clear)
    (keymap-set map "C-c c" 'haskell-process-cabal)
    (keymap-set map "C-c C-z" 'haskell-interactive-switch)))

(provide 'clgc-keybindings)
