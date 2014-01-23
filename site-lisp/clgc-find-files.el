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
(when (file-exists-p "/usr/bin/ack-grep")
  (setq ack-and-a-half-executable "/usr/bin/ack-grep"))

(require 'helm-config)

(add-to-list 'ido-ignore-directories "target")

(require 'find-file-in-project)
(dolist (pattern '("*.css" "*.less" "*.gsp" "*.erb" "*.rabl"
                   "*.groovy" "*.java" "*.json" "*rc"
                   "*.sql" "*.xml" "*.properties"
                   "*.md" "*README*" "*.org"))
  (add-to-list 'ffip-patterns pattern t))

(setq ffip-limit 1024)

;; override ffip-join-patterns to wrap output in a single group
;; so that -and -not actually works for filtering
(defun ffip-join-patterns ()
  "Turn `ffip-paterns' into a string that `find' can use."
  (format "\\( %s \\)"
          (mapconcat (lambda (pat) (format "-name \"%s\"" pat))
                     ffip-patterns " -or ")))

(setq ffip-find-options
      (format "-not \\( %s \\)"
	      (mapconcat (lambda (path)
			   (format "-regex \"%s\"" path))
			 '(".*/old/.*"
			   ".*/target/test-reports/.*"
			   ".*/web-app/js/vendor/.*") " -or ")))

;; https://gist.github.com/1198329
;; original command: '("git ls-files -z | xargs -0 egrep -nH -e " . 41)
(defun find-grep-in-project (command-args)
  (interactive
   (progn
     (list (read-shell-command "Run find (like this): "
                               '("git grep -nH -e " . 17)
                               'grep-find-history))))
  (when command-args
    (let ((null-device nil)) ; see grep
      (grep command-args))))

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

;; make the backup gods obey ME! no more ~ sprinkles all over the place
(setq version-control nil)
(let ((backup-dir (expand-file-name "backups" user-emacs-directory)))
  (setq backup-directory-alist
        `(("." . ,backup-dir)))
  (setq auto-save-file-name-transforms
        `((".*" ,backup-dir t))))

(provide 'clgc-find-files)
