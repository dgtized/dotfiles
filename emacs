;; Please be -*- emacs-lisp -*-

(if (file-exists-p "~/.site-config")
    (save-excursion
      (let ((site-config-buf (find-file-noselect "~/.site-config")))
        (switch-to-buffer site-config-buf)
        (goto-line 0)
        (while (re-search-forward "export \\(.*\\)=\\(.*\\)" nil t)
          (setenv (match-string 1) (match-string 2)))
        (kill-buffer site-config-buf))))

(load (concat (getenv "DOTC_DIR") "/site-lisp/init.el"))
