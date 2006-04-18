
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(defun my-matlab-mode-hook ()
  (setq matlab-indent-function t) ; if you want function bodies indented
  (setq fill-column 76)                 ; where auto-fill should wrap
  (matlab-mode-hilit)
  (turn-on-auto-fill))
(setq matlab-mode-hook 'my-matlab-mode-hook)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
;(defun my-matlab-shell-mode-hook () '())
;(setq matlab-mode-hook 'my-matlab-mode-hook)
