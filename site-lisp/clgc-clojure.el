;;; clgc-clojure --- Clojure/Cider helpers           -*- lexical-binding: t; -*-

;;; Code:

(defun clj-import-profiling ()
  (interactive)
  (cider-interactive-eval
   "(require '[clj-async-profiler.core :as prof]
           '[clj-java-decompiler.core :refer [decompile disassemble]]
           '[criterium.core :as crit])"))

;; TODO: add helper for popup view image from clojure?
(defun clj-combined-popup-eval-handler (buffer complete-handler)
  "Make a handler for combining evaluation, stdout, and stderr results in popup BUFFER.
This is used to generate mode specific popups."
  (nrepl-make-response-handler
   buffer
   (lambda (buffer value)
     (cider-emit-into-popup-buffer buffer value nil t))
   (lambda (buffer value)
     (cider-emit-into-popup-buffer buffer value nil t))
   (lambda (buffer value)
     (cider-emit-into-popup-buffer buffer value nil t))
   (lexical-let ((complete-handler complete-handler))
     (lambda (buffer)
       (with-current-buffer buffer
         (setq buffer-read-only nil)
         (funcall complete-handler)
         (setq buffer-read-only t))))
   nil
   nil
   (lambda (buffer warning)
     (cider-emit-into-popup-buffer buffer warning 'font-lock-warning-face t))))

(defun clj-decompile-render (operation mode complete-handler)
  (let ((sexp (cider-sexp-at-point))
        (buf (cider-popup-buffer (format "*%s result*" operation)
                                 nil mode 'ancillary)))
    (cider-interactive-eval
     (format "(do (require '[clj-java-decompiler.core :refer [decompile disassemble]]) (%s %s) :ok)"
             operation sexp)
     (clj-combined-popup-eval-handler buf complete-handler))))

(defun clj-decompile ()
  "Use clj-java-decompiler to decompile sexp-at-point"
  (interactive)
  (clj-decompile-render
   "decompile"
   'java-mode
   (lambda ()
     (indent-region (point-min) (point-max))
     (whitespace-cleanup))))

(defun clj-disassemble ()
  "Use clj-java-decompiler to disassemble sexp-at-point"
  (interactive)
  (clj-decompile-render
   "disassemble"
   'javap-mode
   (lambda ()
     (whitespace-cleanup))))

;; adapted from http://adereth.github.io/blog/2014/05/29/custom-clojure-evaluation-keybindings-in-emacs/
(defun clj-quick-benchmark ()
  (interactive)
  (let ((sexp (cider-sexp-at-point))
        (buf (cider-popup-buffer "*cider-quick-benchmark*"
                                 nil 'text-mode 'ancillary)))
    (cider-interactive-eval
     (format "(do (require 'criterium.core) (criterium.core/quick-bench %s) :ok)"
             sexp)
     (clj-combined-popup-eval-handler
      buf
      (lambda () nil)))))

;; requires spec-provider {:mvn/version "0.4.14"}
(defun clj-infer-specs ()
  (interactive)
  (let ((sexp (cider-sexp-at-point))
        (buf (cider-popup-buffer "*cider-infer-specs*"
                                 nil 'clojure-mode 'ancillary)))
    (cider-interactive-eval
     (format (concat "(do (require '[spec-provider.provider :as ispec]) "
                     "(ispec/pprint-specs (ispec/infer-specs %s :emacs/cider-value) 'emacs 's))")
             sexp)
     (clj-combined-popup-eval-handler
      buf
      (lambda () nil)))))

(defun depot-outdated ()
  "List out of date deps.edn dependencies"
  (interactive)
  (compile "clojure -Aoutdated"))

(defun depot-upgrade ()
  "Upgrade deps.edn dependencies"
  (interactive)
  (compile "clojure -Aoutdated -update"))


(provide 'clgc-clojure)
