(require 'thingatpt)
(require 'bundler)

(defun bundle-outdated-update ()
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (package (nth 2 (s-split-up-to "\s+" line 3))))
    (compilation-start (concat "bundle update " package " | grep -i install"))
    (bundle-outdated-toggle-strike)))

(defun bundle-outdated-toggle-strike ()
  (interactive)
  (let* ((s (+ 2 (point-at-bol)))
         (e (point-at-eol))
         (properties (get-text-property (1+ s) 'face)))
    (if properties
        (remove-text-properties s e '(face nil))
      (add-face-text-property s e '(:strike-through t)))))

;;;###autoload
(define-minor-mode bundle-outdated-mode
  "Run updates directly from bundle outdated output

\\{bundle-outdated-mode-map\}"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c u") 'bundle-outdated-update)
            (define-key map (kbd "C-c i") 'bundle-outdated-toggle-strike)
            map))

(provide 'bundle-outdated)
