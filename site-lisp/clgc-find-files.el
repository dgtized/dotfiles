(require 'ivy)
(setq ivy-use-virtual-buffers t
      ivy-height 16
      ivy-count-format "(%d/%d) "
      projectile-completion-system 'ivy)

(require 'flx)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy))
      ivy-initial-inputs-alist nil
      )

(ivy-mode t)
(require 'ivy-hydra)
(counsel-mode t)

(require 'projectile)

;; I don't like rebindings for consel-projectile-switch-project or
;; counsel-projectile-ag. If switch project can actually just jump to project
;; then i would use it, but it wants to do two actions, similarly the ag search
;; ditches occur buffers for a ivy-occur buffer which may be useful but is not
;; what I want. This overrides the upstream bindings.
(setq counsel-projectile-key-bindings
      '((projectile-find-file        . counsel-projectile-find-file)
        (projectile-find-file-dwim   . counsel-projectile-find-file-dwim)
        (projectile-find-dir         . counsel-projectile-find-dir)
        (projectile-switch-to-buffer . counsel-projectile-switch-to-buffer)
        ("sG" . counsel-projectile-grep)
        ("sS" . counsel-projectile-ag)
        ("sR" . counsel-projectile-rg)
        ("P"  . counsel-projectile-switch-project)
        (" "  . counsel-projectile)
        ("si" . counsel-projectile-git-grep)
        ;; ("Oc"                        . counsel-projectile-org-capture)
        ;; ("Oa"                        . counsel-projectile-org-agenda)
        ))

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

(counsel-projectile-mode t)

(defun clgc-ibuffer-projectile ()
  (ibuffer-projectile-set-filter-groups)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))
(add-hook 'ibuffer-hook 'clgc-ibuffer-projectile)

(setq ag-highlight-search t
      ag-group-matches nil
      ag-ignore-list '("*.min.js" "vendor/assets/*"))
;; (add-hook 'ag-mode-hook 'next-error-follow-minor-mode)

(require 'rg)
(rg-enable-default-bindings (kbd "C-c s"))
(add-hook 'rg-mode-hook 'wgrep-ag-setup)

(add-hook 'compilation-mode-hook 'winnow-mode)

(require 'helm-config)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; (add-to-list 'ido-ignore-directories "target")

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

(with-eval-after-load 'webjump
  (add-to-list
   'webjump-sites
   '("Github" .
     [simple-query "https://github.com"
                   "https://github.com/search?q="
                   "&type=Everything&repo=&langOverride=&start_value=1"])))

(provide 'clgc-find-files)
