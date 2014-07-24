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
      ido-use-faces nil
      flx-ido-threshhold 8192
      ido-use-filename-at-point nil
      ido-use-url-at-point nil)

(defadvice ido-file-internal (around ffap activate)
  "When called with a prefix, use `ffap' instead."
  (if current-prefix-arg
      (ffap)
    ad-do-it))

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

(projectile-global-mode)
(setq projectile-enable-caching t
      projectile-switch-project-action 'projectile-dired
      projectile-use-git-grep t)

(add-hook 'ag-mode-hook 'next-error-follow-minor-mode)
(add-hook 'ack-and-a-half 'next-error-follow-minor-mode)

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
