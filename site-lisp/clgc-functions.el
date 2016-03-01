(require 'crux)

(defmacro when-emacs-version (vers &rest body)
  `(when (equal emacs-major-version ,vers)
    ,@body))

(defun call-if-fbound (function &rest args)
  (when (fboundp function)
    (apply function args)))

;show ascii table
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (loop for i from 0 to 254 do
        (insert (format "%4d %c\n" i i)))
  (goto-line 0))

;; from http://stackoverflow.com/a/4717026/34450
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
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

(provide 'clgc-functions)
