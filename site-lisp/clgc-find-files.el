(eval-when-compile (require 'cl)
                   (require 'clgc-elpa))

;; Ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(require 'find-file-in-project)
(dolist (pattern '("*.css" "*.groovy" "*.java" "*.sql"
                   "*rc" "*.gsp" "*.xml" "*.properties"))
  (add-to-list 'ffip-patterns pattern t))

(setq ffip-limit 768)

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

(defvar crs-hated-buffers
  '("KILL" "*Compile-Log*"))

(setq completion-ignored-extensions
      '("~" ".aux" ".a" ".bbl" ".blg" ".dvi" ".elc" ".class"
        ".hc" ".hi" ".log" ".mlc" ".o" ".so" ".toc" ".rbc"))

;; make the backup gods obey ME! no more ~ sprinkles all over the place
(setq version-control nil)
(setq backup-directory-alist
      (list (cons "." (expand-file-name "backups" user-emacs-directory))))

(provide 'clgc-find-files)