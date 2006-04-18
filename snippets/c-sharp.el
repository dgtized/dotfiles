
;(setq-default compilation-error-regexp-alist
; (append '(
; ; Microsoft JVC:
; ;sample.java(6,1) : error J0020: Expected 'class' or 'interface'
; ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) : \\(error\\|warning\\) J[0-9]+:" 1 3 4)
;
; ; Microsoft C/C++:
; ;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
; ;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
; ;VC EEi
; ;e:\projects\myce40\tok.h(85) : error C2236: unexpected 'class' '$S1'
; ;myc.cpp(14) : error C3149: 'class System::String' : illegal use of managed type 'String'; did you forget a '*'?
;    ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \
;: \\(error\\|warning\\) C[0-9]+:" 1 3)
; ) compilation-error-regex-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              C# Mode support
;;;

(autoload 'csharp-mode "cc-mode")

(c-add-style "myC#Style"
   '("C#"
   (c-basic-offset . 2)
   (c-comment-only-line-offset . (0 . 0))
   (c-offsets-alist . (
     (c                     . c-lineup-C-comments)
     (inclass		    . 0)
     (namespace-open	    . +)
     (namespace-close	    . +)
     (innamespace	    . 0)
     (class-open	    . +)
     (class-close	    . +)
     (inclass		    . 0)
     (defun-open	    . +)
     (defun-block-intro     . 0)
     (inline-open	    . +)
     (inline-close	    . 0)
     (statement-block-intro . 0)
     (statement-cont	    . +)
     (brace-list-intro      . +)
     (topmost-intro-cont    . 0)
     (block-open	    . +)
     (block-close	    . 0)
     (arglist-intro	    . +)
;     (arglist-cont	    . 0)
     (arglist-close	    . 0)
     ))
   ))

(add-hook 'csharp-mode-hook 
          (lambda ()
            (cond (window-system
                   (turn-on-font-lock)
                   (c-set-style "myC#Style")))))



(append '(          
          ;;C# Compiler
          ;;t.cs(6,18): error SC1006: Name of constructor must match name of class
          ;;(setq compilation-error-regexp-alist
          ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) CS[0-9]+:" 1 3 4)
          )
        compilation-error-regexp-alist)
