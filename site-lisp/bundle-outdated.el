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
        (progn
          (message "remove")
          (remove-text-properties s e '(face nil)))
        (add-face-text-property s e '(:strike-through t)))))

(define-key text-mode-map (kbd "C-c u") 'bundle-outdated-update)
(define-key text-mode-map (kbd "C-c i") 'bundle-outdated-toggle-strike)

(provide 'bundle-outdated)
