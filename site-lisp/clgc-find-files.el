(eval-when-compile (require 'cl))

;; Ido
(require 'ido)
(require 'flx-ido)
(require 'ffap)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
(setq ido-enable-flex-matching t ;; enable fuzzy matching
      ido-case-fold t
      ido-create-new-buffer 'always
      ido-use-virtual-buffers t
      ido-use-faces t
      flx-ido-threshhold 8192
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      smex-history-length 32 ;; might need to be set before smex starts to work
      )

(defadvice ido-file-internal (around ffap activate)
  "When called with a prefix, use `ffap' instead."
  (if current-prefix-arg
      (ffap)
    ad-do-it))

(setq recentf-max-saved-items 50)
(recentf-mode t)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

(projectile-mode t)
(setq projectile-enable-caching t
      projectile-switch-project-action 'projectile-dired
      projectile-use-git-grep t
      projectile-tags-command
      "ctags-exuberant --exclude='*.min.js' --exclude='vendor/assets' -Re -f \"%s\" %s")

(defun clgc-ibuffer-projectile ()
  (ibuffer-projectile-set-filter-groups)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))
(add-hook 'ibuffer-hook 'clgc-ibuffer-projectile)

(setq ag-highlight-search t
      ag-group-matches nil
      ag-ignore-list '("*.min.js" "vendor/assets/*"))
(add-hook 'ag-mode-hook 'next-error-follow-minor-mode)

(add-hook 'compilation-mode-hook 'winnow-mode)

(require 'helm-config)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(add-to-list 'ido-ignore-directories "target")

(require 'etags)
(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
	    (unless (integerp x)
	      (push (prin1-to-string x t) tag-names)))
	  tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(setq completion-ignored-extensions
      '("~" ".aux" ".a" ".bbl" ".blg" ".dvi" ".elc" ".class"
        ".hc" ".hi" ".log" ".mlc" ".o" ".so" ".toc" ".rbc"))

;; github-browse-file settings
(setq github-browse-file-show-line-at-point t
      github-browse-file-visit-url nil)

(eval-after-load 'webjump
  '(progn
     (add-to-list
      'webjump-sites
      '("Github" .
        [simple-query "https://github.com"
                      "https://github.com/search?q="
                      "&type=Everything&repo=&langOverride=&start_value=1"]))))

(provide 'clgc-find-files)
