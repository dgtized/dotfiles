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
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (goto-line 0))

;insert date into buffer
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

;convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))

;vice versa
(defun unix2dos ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n")))

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

(defun comment-dwim-line ()
  "Comment or uncomment the current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

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

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(defun load-environment-variables ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "export \\([^=]+\\)=\\(.+\\)" nil t)
      (let ((var (match-string 1))
            (value (match-string 2)))
        (message "Setting %s to %s" var value)
        (setenv var value)))))

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun github-browse ()
  (interactive)
  (let ((url (magit-get "remote" (magit-get-current-remote) "url")))
    (save-match-data
      (if (string-match "github.com:\\([^/]+\\)/\\([^\.]+\\).git" url)
          (let ((owner (match-string 1 url))
                (repo (match-string 2 url)))
            (browse-url (format "https://github.com/%s/%s/blob/%s/%s#L%d"
                                owner repo
                                (magit-get-current-branch)
                                (magit-file-relative-name (buffer-file-name))
                                (line-number-at-pos))))))))

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

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

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

(provide 'clgc-functions)
