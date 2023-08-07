;;; clgc-clojure --- Clojure/Cider helpers           -*- lexical-binding: t; -*-

;;; Code:

(eval-when-compile (require 'cider))
(eval-when-compile (require 'nrepl-client))

(require 'cider-eval)
(require 'cider-format)

(require 'clj-refactor)

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

(defun clj-deps-root ()
  "Locate the root directory of a deps.edn based Clojure project."
  (or (locate-dominating-file default-directory "deps.edn")
      (user-error "Not in a deps.edn Clojure project")))

(defmacro in-clj-root (&rest body)
  "Run commands in root of Clojure project."
  `(let ((default-directory (clj-deps-root)))
     ,@body))

(defun depot-outdated ()
  "List out of date deps.edn dependencies"
  (interactive)
  (in-clj-root
   (compile "clojure -Moutdated --every")))

(defun depot-upgrade ()
  "Upgrade deps.edn dependencies"
  (interactive)
  (in-clj-root
   (compile "clojure -Moutdated --every --write")))

(defun antq-outdated ()
  (interactive)
  (in-clj-root
   (compile "clojure -Mantq --error-format=\"{{name}} - {{version}} -> {{latest-version}}\n  {{changes-url}}\"")))

(defun clj-deps-tree ()
  "List dependency tree from deps.edn"
  (interactive)
  (in-clj-root
   (compile "clojure -Stree")))

(defun clj-kondo ()
  "Invoke default clj-kondo linting in directory."
  (interactive)
  (in-clj-root
   (compile "clj-kondo --lint src test")))

;; WIP, need to generalize better
(defun cider-eval-to-test-handler (sexp copy-to-kill buffer)
  "Make a handler for evaluating and printing result in BUFFER along with evaluated SEXP in test form."
  (nrepl-make-response-handler
   buffer
   (lambda (buffer value)
     (cider-emit-into-popup-buffer buffer value nil t))
   (lambda (_buffer out)
     (cider-emit-interactive-eval-output out))
   (lambda (_buffer err)
     (cider-emit-interactive-eval-err-output err))
   (lambda (buffer)
     (cider-emit-into-popup-buffer buffer sexp)
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (cider-format-buffer))
       (when copy-to-kill
         (kill-ring-save (point-min) (point-max)))))
   nil
   nil
   (lambda (buffer warning)
     (cider-emit-into-popup-buffer buffer warning 'font-lock-warning-face t))))

;; I would prefer to use cider--nrepl-print-request-map as it should handle
;; pretty printing, however that results in evaluation errors. Using pr request
;; map works so long as the output example does not have reader symbols like
;; #vec3[0 0 0]. Need a way to both pretty print and ensure output can be evaluated.
;;
;; TODO: experiment with cider-eval-sexp-up-to-point to evaluate inside let statements
(defun cider-eval-to-test-example (&optional copy-to-kill)
  "Evaluate the preceding sexp and generate a clj-test form ala (is (= evaluation (sexp))).

  With a prefix argument, copies the result to kill ring after
  evaluation completes."
  (interactive "P")
  (let* ((sexp (cider-last-sexp))
         (result-buffer (cider-popup-buffer "*cider-test-example*" nil 'clojure-mode 'ancillary))
         ;; the evaluated result is quoted to ensure paren pairs are interpreted as lists
         (leader "(is (= '")
         (suffix (concat "\n       " sexp "))")))
    (cider-emit-into-popup-buffer result-buffer leader)
    (cider-interactive-eval sexp
                            (cider-eval-to-test-handler suffix copy-to-kill result-buffer)
                            nil
                            (cider--nrepl-pr-request-map))))

(provide 'clgc-clojure)
