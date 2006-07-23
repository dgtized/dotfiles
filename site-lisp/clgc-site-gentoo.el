;;; ispell-aspell site-lisp configuration

(setq ispell-program-name "aspell")

;;; auctex site-lisp configuration

(require 'tex-site)

;;; elib site-lisp configuration

(setq load-path (cons "/usr/share/emacs/site-lisp/elib" load-path))

;;; htmlize site-lisp configuration

(if (file-directory-p "/usr/share/emacs/site-lisp/htmlize") 
    (progn
      (add-to-list 'load-path "/usr/share/emacs/site-lisp/htmlize")
      (require 'htmlize)))

;;; mmm-mode site-lisp configuration

(setq load-path (cons "/usr/share/emacs/site-lisp/mmm-mode" load-path))
(require 'mmm-auto)

;;; cedet site-lisp configuration
; (load "/usr/share/emacs/site-lisp/cedet/common/cedet")

;; preview-latex gentoo site configuration

(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
(autoload 'LaTeX-preview-setup "preview")

;;; JDE site-lisp configuration
; (setq load-path (cons "/usr/share/emacs/site-lisp/jde/lisp" load-path))
;(autoload 'jde-mode "jde" "Java Development Environment Emacs" t)
;(setq auto-mode-alist (cons '("\\.java$" . jde-mode) auto-mode-alist))

;;; svn site-lisp configuration

(setq load-path (cons "/usr/share/emacs/site-lisp/subversion" load-path))
(add-to-list 'vc-handled-backends 'SVN)
(require 'psvn)

;; (when (file-exists-p "/usr/share/emacs/site-lisp/site-gentoo.el")
;;  (load "/usr/share/emacs/site-lisp/site-gentoo"))

(message "Loaded gentoo specific libs -- site-gentoo")

(provide 'clgc-site-gentoo)
