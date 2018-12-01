;;; .emacs --- initialization
;; Please be -*- emacs-lisp -*-
;;; Code:

(package-initialize)

(if (file-exists-p "~/.site-config")
    (save-excursion
      (let ((site-config-buf (find-file-noselect "~/.site-config")))
        (switch-to-buffer site-config-buf)
        (goto-char (point-min))
        (while (re-search-forward "export \\(.*\\)=\\(.*\\)" nil t)
          (setenv (match-string 1) (match-string 2)))
        (kill-buffer site-config-buf))))

(setenv "INSIDE_EMACS" "1")
(load (concat (getenv "DOTC_DIR") "/site-lisp/clgc-init.el"))

;;; .emacs ends here
