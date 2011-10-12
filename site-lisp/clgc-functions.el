
(defmacro when-emacs-version (vers &rest body)
  `(when (equal emacs-major-version ,vers)
    ,@body))

(defun call-if-fbound (function &rest args)
  (when (fboundp function)
	(apply function args)))

(defun indent-or-complete ()
  "Complete if point is at end of line, and indent line."
  (interactive)
  (if (looking-at "$")
      (hippie-expand nil))
  (indent-for-tab-command))

;show ascii table
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (goto-line 0))

;insert date into buffer
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

;convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))

;vice versa
(defun unix2dos ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n"))) 

; outline stuff

;; Outline Mode
; Outline-minor-mode key map
(require 'outline)
(define-prefix-command 'cm-map nil "Outline-")
; HIDE
(define-key cm-map "q" 'hide-sublevels)     ; Hide everything but the top-level headings
;(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "t" 'outline-toggle-all) ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)         ; Hide other branches
(define-key cm-map "c" 'hide-entry)         ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)        ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)       ; Hide everything in this entry and sub-entries
; SHOW
(define-key cm-map "a" 'show-all)           ; Show (expand) everything
;(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "e" 'outline-toggle-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key "\M-o" cm-map)

(defun outline-toggle-entry () (interactive)
  "Toggle outline hiding for the entry under the cursor"
  (defvar cpos_save)
  (if (progn
	(setq cpos_save (point))
	(end-of-line)
	(get-char-property (point) 'invisible))
      (progn 
	(show-subtree)
	(goto-char cpos_save))
    (progn 
      (hide-leaves)
      (goto-char cpos_save))))

(defvar outline-toggle-all-flag t)
(defun outline-toggle-all () (interactive)
  "Toggle outline hiding for the entire file"
  (if outline-toggle-all-flag
      (progn
	(setq outline-toggle-all-flag nil)
	(show-all))
    (progn 
      (setq outline-toggle-all-flag t)
      (hide-body))))

(provide 'clgc-functions)
