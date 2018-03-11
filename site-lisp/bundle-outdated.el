(require 'thingatpt)
(require 'bundler)

(defun bundle-outdated-update ()
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (if (string-match "^  \\* \\([^\s]+\\).*$" line)
        (let* ((package (match-string 1 line))
               (cmd (concat "bundle update " package " | grep -i install")))
          (async-shell-command cmd "*Bundler update*")
          (bundle-outdated-toggle-strike)))))

(defun bundle-outdated-toggle-strike ()
  (interactive)
  (let* ((s (+ 2 (line-beginning-position)))
         (e (line-end-position))
         (properties (get-text-property (1+ s) 'font-lock-face)))
    (if properties
        (remove-text-properties s e '(font-lock-face nil))
      (put-text-property s e 'font-lock-face '(:strike-through t)))))

;;;###autoload
(define-minor-mode bundle-outdated-mode
  "Run updates directly from bundle outdated output

\\{bundle-outdated-mode-map\}"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c u") 'bundle-outdated-update)
            (define-key map (kbd "C-c i") 'bundle-outdated-toggle-strike)
            map))

(provide 'bundle-outdated)
