(defun clgc-sql-save-history ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat "~/.emacs.d/sql/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'clgc-sql-save-history)

;; SQLi Mode
(defun clgc-sql-interactive-hook () (toggle-truncate-lines t))
(add-hook 'sql-interactive-mode-hook 'clgc-sql-interactive-hook)


;; (sql-set-product-feature 'mysql :explain "explain format=json")

;;;###autoload
(defun clgc-mysql-explain-statement ()
  "Send the current command at point to SQL, prefixed with explain"
  (interactive)
  (let* ((outbuf "*SQL EXPLAIN EXTENDED*")
         (start (save-excursion
                  (backward-paragraph)
                  (point)))
         (end (save-excursion
                (forward-paragraph)
                (point)))
         (command (concat "explain format=json "
                          (buffer-substring-no-properties start end))))
    (sql-execute sql-buffer outbuf command nil nil)
    (with-current-buffer outbuf
      (read-only-mode -1)
      (goto-char (point-min))
      (when (search-forward "{")
        (delete-region 1 (1- (point))))
      (goto-char (point-max))
      (when (search-backward "}")
        (delete-region (1+ (point)) (point-max)))
      (goto-char (point-min))
      (js-mode)
      )))

(eval-when-compile (require 'sql))
(with-eval-after-load 'sql
  (progn
    (define-key sql-interactive-mode-map (kbd "C-c C-e") 'clgc-mysql-explain-statement)
    (define-key sql-mode-map (kbd "C-c C-e") 'clgc-mysql-explain-statement)))

(provide 'clgc-sql)
