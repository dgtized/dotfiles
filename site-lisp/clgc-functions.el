(require 'crux)

(defmacro when-emacs-version (vers &rest body)
  `(when (equal emacs-major-version ,vers)
    ,@body))

(defun call-if-fbound (function &rest args)
  (when (fboundp function)
    (apply function args)))

;; show ascii table, but probably worth investigating
;; `describe-current-display-table'.
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (cl-loop for i from 0 to 254 do
           (progn
             (insert (format "%4d %x %-4c" i i i))
             (if (= (mod (1+ i) 4) 0) (insert "\n"))))
  (goto-char (point-min))
  (view-mode t))

(defun unfill-paragraph ()
  "Folds a multi-line paragraph into a single line of text.

In other words, the opposite of fill paragraph.
Borrowed from Stefan Monnier <foo at acm.org>"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun visit-ielm ()
  "Switch to default `ielm' buffer. Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(defun isearch-other-window ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward)))

(defun ediff-other-window (wordwise)
  "Ediff linewise or wordwise depending on prefix argument"
  (interactive "P")
  (if wordwise
      (ediff-windows-wordwise t)
    (ediff-windows-linewise t)))

(defun load-environment-variables ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "export \\([^=]+\\)=\\(.+\\)" nil t)
      (let ((var (match-string 1))
            (value (match-string 2)))
        (message "Setting %s to %s" var value)
        (setenv var value)))))

(defun clgc-gist-browse ()
  "Browse url the currently selected gist"
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (gist (gist-list-db-get-gist id)))
    (browse-url (oref gist :html-url))))

(defun clgc-gist-region (&optional public)
  "Post either the current region, or if mark is not set, the
  current buffer as a new paste at gist.github.com

Copies the URL into the kill ring and calls browse-url

With a prefix argument, makes a public paste."
  (interactive "P")
  (if (use-region-p)
      (gist-region (point) (mark) (not public))
    (gist-buffer (not public))))

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote" (magit-get-remote) "url"))
           (or (magit-get-push-branch)
               (user-error "No remote branch")))))

(defun revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

(defun rdired (directory)
  (interactive "D")
  (find-dired directory
              "-not -path '*/.svn*' -not -path '*/.git*' -and -not -path '*.o' -and -type f"))

(defun eshell/rdired (&optional directory)
  (funcall 'rdired (or directory default-directory)))

(defun ansi-color-apply-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; see builtin json-pretty-print-?{buffer,ordered,buffer-ordered}
(defun clgc-prettify-json-region ()
  "Prettify json in region or buffer"
  (interactive)
  (save-restriction
    (when (use-region-p) (narrow-to-region (region-beginning) (region-end)))
    (shell-command-on-region (point-min) (point-max) "python -mjson.tool" t t)))

(defun ert-silently ()
  (interactive)
  (ert t))

(defun clgc-ruby-compile-this-buffer ()
  (interactive)
  (save-current-buffer (ruby-compilation-this-buffer)))

(defun load-secrets ()
  (interactive)
  (if (file-exists-p "~/.emacs.d/secrets.el.gpg")
      (load-file "~/.emacs.d/secrets.el.gpg")
    (if (file-exists-p "~/.emacs.d/secrets.el")
        (load-file "~/.emacs.d/secrets.el"))))

(require 'request)
(defun slack-webhook (json)
  (request (getenv "SLACK_WEBHOOK")
           :type "POST"
           :headers '(("Content-Type" . "application/json"))
           :data json))

(defun slack-attachment (username channel attachments)
  (json-encode `(:username ,username :channel ,channel :attachments ,attachments)))

(defvar jenkins-url "jenkins/%s"
  "Url to jenkins with a format argument to replace with the branch name")

(defun jenkins-visit-branch ()
  "Visit the current branch on Jenkins"
  (interactive)
  (browse-url
   (format jenkins-url
           (or (magit-get-current-branch)
               (user-error "No remote branch")))))

(defun clgc-toggle-monitor ()
  "Toggle display resolution from 3840x2160 to 1920x1080"
  (interactive)
  (async-shell-command "toggle-monitor.sh"))

;; ag/read-file-type in interactive is not autoloaded
(require 'ag)

(defun projectile-ag-files (search-term file-type)
  "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression."
  (interactive
   (list (projectile--read-search-string-with-default (format "Ag search files for" ))
         (ag/read-file-type)))
  (let ((ag-ignore-list (delq nil
                              (delete-dups
                               (append
                                ag-ignore-list
                                (projectile--globally-ignored-file-suffixes-glob)
                                ;; ag supports git ignore files directly
                                (unless (eq (projectile-project-vcs) 'git)
                                  (append (projectile-ignored-files-rel)
                                          (projectile-ignored-directories-rel)
                                          grep-find-ignored-files
                                          grep-find-ignored-directories)))))))
    (funcall 'ag-files search-term file-type (projectile-project-root))))


(defun clgc-ruby-string->symbol ()
  "Change string at point into a symbol"
  (interactive)
  (when (ruby-string-at-point-p)
    (save-excursion
      (sp-beginning-of-sexp)
      (sp-splice-sexp)
      (insert ":"))))

;; https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun clgc-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun nri-insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%H:%M:%S %Z %Y-%m-%d" (current-time) "UTC")))

(defun rubocop-auto-gen-config ()
  "Regenerate .rubocop_todo.yml with settings from .rubocop.yml"
  (interactive)
  (let ((cmd "rubocop -c .rubocop.yml --format emacs --auto-gen-config"))
    (rubocop--dir-command cmd (rubocop-project-root))))

;; (require 'eww)

;; (defun eww-render-current-buffer ()
;;   "Render HTML in the current buffer with EWW"
;;   (interactive)
;;   (beginning-of-buffer)
;;   (eww-display-html 'utf8 (buffer-name)))

;; (defun html2text ()
;;   "Replacement for standard html2text using shr."
;;   (interactive)
;;   (let ((dom (libxml-parse-html-region (point-min) (point-max))))
;;     (switch-to-buffer "*html*")
;;     (erase-buffer)
;;     (shr-insert-document dom)
;;     (goto-char (point-min))))

;; Ideas:
;; Parse ag matches for files / contents for graphviz tricks

(defun clgc-font-examples ()
  (interactive)
  (switch-to-buffer "*Font Examples*")
  (erase-buffer)
  (let ((str "The quick brown fox jumps over the lazy dog ´`''\"\"1lI|¦!Ø0Oo{[()]}.,:; ")
        (font-families (cl-remove-duplicates
                        (sort (font-family-list)
                              (lambda(x y) (string< (upcase x) (upcase y))))
                        :test 'string=)))
    (dolist (ff font-families)
      (insert (propertize str 'font-lock-face `(:family ,ff)) ff "\n")))
  (goto-char (point-min))
  (view-mode t))

(provide 'clgc-functions)
