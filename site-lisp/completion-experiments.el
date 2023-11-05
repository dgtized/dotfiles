(completing-read ";) "
                 (list (propertize "vim" 'display "there is evil-mode...")
                       (propertize "emacs" 'display "the best")))

 (completing-read ";) "
         (list (propertize "vim; there iw evil-mode" 'display "vim")
               (propertize "emacs; the best" 'display "emacs")))



(defvar my-completions '(("a" "description of a") ("b" "b's description")))
(assoc "a" my-completions)

(defun my-annotation-function (s)
  (let ((item (assoc s minibuffer-completion-table)))
    (when item
      (concat "  -- " (cadr item)))))

(my-annotation-function "a")

(let ((completion-extra-properties '(:annotation-function my-annotation-function)))
  (completing-read "Prompt: " my-completions))

(let* ((alist '(("GNU" . "GNU is not Unix")
                ("Emacs" . "Eight Megabytes And Constantly Swapping")))
       (annotf (lambda (str)
                 (format " (%s)" (cdr (assoc str alist))))))
  (completing-read "Candidates: "
                   (lambda (str pred action)
                     (if (eq action 'metadata)
                         `(metadata
                           (annotation-function . ,annotf)
                           (cycle-sort-function . identity)
                           (display-sort-function . identity))
                       (complete-with-action action alist str pred)))))

(defun my-completion-function (prefix)
  ;; You can just ignore the prefix
  '(("[clojure.string :as s] [clj]" . "clj")
    ("[clojure.string :as s] [cljs]" . "cljs")))

(let ((completion-ignore-case t))
  (completing-read "Add require: "
                   (completion-table-dynamic #'my-completion-function)
		   nil t nil nil))

lazy-completion-table

