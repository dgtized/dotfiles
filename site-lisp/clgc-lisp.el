(require 'clj-refactor)

(add-hook 'emacs-lisp-mode-hook 'auto-recompile-el-buffer)

(defun auto-recompile-el-buffer ()
  "Recompile elisp buffers on save if it has been compiled before"
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (when (file-exists-p (byte-compile-dest-file buffer-file-name))
                (byte-compile-file buffer-file-name)))))

;; Enhance Lisp Modes
(dolist (mode '(scheme emacs-lisp inferior-emacs-lisp lisp clojure
                       inferior-lisp cider-repl))
  (let ((mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    (add-hook mode-hook 'smartparens-strict-mode)))

(with-eval-after-load 'slime
  (add-hook slime 'smartparens-strict-mode)
  (add-hook slime-repl 'smartparens-strict-mode))

(with-eval-after-load 'sly
  (add-hook sly 'smartparens-strict-mode))

;; Aggressive indent in buffer, but not repl
(dolist (mode '(scheme emacs-lisp lisp clojure))
  (let ((mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    (add-hook mode-hook 'aggressive-indent-mode)))

;; (defadvice cider-eval-last-sexp (after cider-flash-last activate)
;;   (flash-region (save-excursion (backward-sexp) (point)) (point)))
;; (defadvice cider-eval-defun-at-point (after cider-flash-at activate)
;;   (apply #'flash-region (cider--region-for-defun-at-point)))

;; Clojure Specific
(defun clgc-clojure-mode-hook ()
  (require 'smartparens-clojure)
  (subword-mode +1)
  (clj-refactor-mode t)
  (cljr-add-keybindings-with-prefix "C-c r"))

(defun clgc-cider-repl-mode-hook ()
  (subword-mode +1))

(defun clgc-cider-mode-hook ()
  (cider-auto-test-mode t))

(eval-when-compile (require 'cider))
(with-eval-after-load 'cider
  (setq cider-show-error-buffer t
        cider-prompt-for-symbol nil
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-history-file (expand-file-name "clojure_repl_history" user-emacs-directory)
        cider-repl-history-size 1000
        cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))"))

(add-hook 'clojure-mode-hook 'clgc-clojure-mode-hook)
(add-hook 'cider-repl-mode-hook 'clgc-cider-repl-mode-hook)
(add-hook 'cider-mode-hook 'clgc-cider-mode-hook)
;;(with-eval-after-load 'clojure-mode (flycheck-clojure-setup))

;; (eval-after-load 'clojure-mode
;;   '(sayid-setup-package))

(setq inferior-lisp-program "sbcl")

(defun clj-import-profiling ()
  (interactive)
  (cider-interactive-eval
   "(require '[clj-async-profiler.core :as prof]
           '[clj-java-decompiler.core :refer [decompile disassemble]]
           '[criterium.core :as crit])"))

(defun clj-decompile-popup-eval-handler (&optional buffer)
  "Make a handler for printing evaluation results in popup BUFFER.
This is used by pretty-printing commands."
  (nrepl-make-response-handler
   (or buffer (current-buffer))
   (lambda (buffer value)
     (cider-emit-into-popup-buffer buffer value nil t))
   (lambda (buffer value)
     (cider-emit-into-popup-buffer buffer value nil t))
   (lambda (buffer value)
     (cider-emit-into-popup-buffer buffer value nil t))
   nil
   nil
   nil
   (lambda (buffer warning)
     (cider-emit-into-popup-buffer buffer warning 'font-lock-warning-face t))))

(defun clj-decompile-render (operation)
  (let ((sexp (cider-sexp-at-point))
        (buf (cider-popup-buffer (format "*%s result*" operation)
                                 nil 'java-mode 'ancillary)))
    (cider-interactive-eval
     (format "(%s %s)" operation sexp)
     (clj-decompile-popup-eval-handler buf))))

(defun clj-decompile ()
  "Use clj-java-decompiler to decompile sexp-at-point"
  (interactive)
  (clj-import-profiling)
  (clj-decompile-render "decompile"))

(defun clj-disassemble ()
  "Use clj-java-decompiler to disassemble sexp-at-point"
  (interactive)
  (clj-import-profiling)
  (clj-decompile-render "disassemble"))

(provide 'clgc-lisp)
