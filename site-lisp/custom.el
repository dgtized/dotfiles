(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:application eshell) eshell-connection-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile
      tramp-flatpak-connection-local-default-profile)
     ((:application tramp) tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-flatpak-connection-local-default-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin"
                         "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
                         "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
                         "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
                               (tramp-kubernetes--container
                                (car tramp-current-connection))
                               104
                               (tramp-kubernetes--pod
                                (car tramp-current-connection))
                               120
                               (tramp-kubernetes--context-namespace
                                (car tramp-current-connection))))
     (eshell-connection-default-profile (eshell-path-env-list))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin"
                         "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
                         "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
                         "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
                                          (user . string) (egid . number)
                                          (comm . 52) (state . 5)
                                          (ppid . number) (pgrp . number)
                                          (sess . number) (ttname . string)
                                          (tpgid . number) (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time) (pri . number)
                                          (nice . number) (vsize . number)
                                          (rss . number) (etime . tramp-ps-time)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number) (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number) (ttname . string)
                                          (time . tramp-ps-time) (nice . number)
                                          (etime . tramp-ps-time) (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
                                          (user . string) (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string) (ppid . number)
                                          (pgrp . number) (sess . number)
                                          (ttname . string) (tpgid . number)
                                          (minflt . number) (majflt . number)
                                          (time . tramp-ps-time) (pri . number)
                                          (nice . number) (vsize . number)
                                          (rss . number) (etime . number)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh")
                                                   (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile (path-separator . ":")
                                                    (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f"
     "1a1cdd9b407ceb299b73e4afd1b63d01bbf2e056ec47a9d95901f4198a0d2428"
     "85d1dbf2fc0e5d30f236712b831fb24faf6052f3114964fdeadede8e1b329832"
     "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c"
     "392395ee6e6844aec5a76ca4f5c820b97119ddc5290f4e0f58b38c9748181e8d"
     "73c69e346ec1cb3d1508c2447f6518a6e582851792a8c0e57a22d6b9948071b4"
     "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347"
     "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3"
     "834cbeacb6837f3ddca4a1a7b19b1af3834f36a701e8b15b628cad3d85c970ff"
     "170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3"
     "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a"
     "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(erc-nick "dgtized")
 '(package-selected-packages
   '(4clojure ace-window ag aggressive-indent anti-zenburn-theme atomic-chrome
              auctex browse-kill-ring bundler buttercup camcorder chruby cider
              clj-refactor clojure-mode clojure-mode-extra-font-locking
              clojure-snippets coffee-mode color-identifiers-mode
              command-log-mode company-inf-ruby counsel counsel-projectile cov
              coverage crux csharp-mode diminish dockerfile-mode elm-mode
              elmacro epresent espresso-theme evil expand-region f feature-mode
              flash-region flatui-theme flx-ido flycheck-clj-kondo
              flycheck-clojure flycheck-credo flycheck-dialyxir
              flycheck-dialyzer flycheck-elixir flycheck-elm free-keys fuzzy
              geiser geiser-guile geiser-racket gist gitconfig-mode
              github-browse-file github-clone gitignore-mode glsl-mode gnuplot
              gnuplot-mode go-mode graphviz-dot-mode hc-zenburn-theme helm-ag
              helm-core helm-projectile helm-unicode helpful ibuffer-projectile
              ido-completing-read+ ivy ivy-hydra iy-go-to-char javap-mode
              jetpack js2-mode json-mode json-navigator jsonian kaocha-runner
              kibit-helper less-css-mode leuven-theme list-environment lua-mode
              magit magit-section markdown-mode material-theme multiple-cursors
              ob-elixir ob-http ob-sml occur-context-resize org-contrib
              org-download org-tree-slide ox-reveal package-lint pcre2el
              plan9-theme plantuml-mode processing-mode prodigy projectile-rails
              racket-mode rainbow-delimiters redis request rg rinari ripgrep
              rspec-mode rubocop ruby-hash-syntax ruby-tools rust-mode sass-mode
              sbt-mode scala-mode simple-httpd slamhound slime sly sly-asdf
              sly-macrostep sly-quicklisp smartparens smartscan smex
              solarized-theme starter-kit-eshell string-inflection swiper
              symbol-overlay terraform-mode tuareg undo-tree visual-fill-column
              vterm web-mode web-server wgrep-ag wgsl-mode winnow yaml-mode
              yard-mode yari zenburn-theme))
 '(safe-local-variable-values
   '((eval
      (lambda nil
        (when (not (featurep 'recursive-reveries))
          (let
              ((recursive-reveries-file
                (expand-file-name "recursive-reveries.el" default-directory)))
            (when (file-exists-p recursive-reveries-file)
              (load recursive-reveries-file) (require 'recursive-reveries))))))
     (cider-repl-display-help-banner)
     (elisp-lint-indent-specs (if-let* . 2) (when-let* . 1) (let* . defun)
                              (nrepl-dbind-response . 2) (cider-save-marker . 1)
                              (cider-propertize-region . 1)
                              (cider-map-repls . 1) (cider--jack-in . 1)
                              (cider--make-result-overlay . 1)
                              (insert-label . defun)
                              (insert-align-label . defun) (insert-rect . defun)
                              (cl-defun . 2) (with-parsed-tramp-file-name . 2)
                              (thread-first . 0) (thread-last . 0)
                              (transient-define-prefix . defmacro)
                              (transient-define-suffix . defmacro))
     (elisp-lint-indent-specs (if-let* . 2) (when-let* . 1) (let* . defun)
                              (nrepl-dbind-response . 2) (insert-label . defun)
                              (insert-align-label . defun) (insert-rect . defun)
                              (cl-defun . 2) (cljr--update-file . 1)
                              (cljr--with-string-content . 1)
                              (with-parsed-tramp-file-name . 2)
                              (thread-first . 0) (thread-last . 0)
                              (transient-define-prefix . defmacro)
                              (transient-define-suffix . defmacro))
     (checkdoc-force-docstrings-flag nil)
     (byte-compile-docstring-max-column 240) (checkdoc-package-keywords-flag)
     (eval
      (lambda nil
        (when (not (featurep 'clerk))
          (let ((clerk-file (expand-file-name "clerk.el" default-directory)))
            (when (file-exists-p clerk-file) (load clerk-file) (require 'clerk))))))
     (eval
      (lambda nil
        (when (not (featurep 'lemniscate))
          (let
              ((lemniscate-file
                (expand-file-name "lemniscate.el" default-directory)))
            (when (file-exists-p lemniscate-file)
              (load lemniscate-file) (require 'lemniscate))))))
     (eval
      (lambda nil
        (when (not (featurep 'shimmers))
          (let
              ((shimmers-file (expand-file-name "shimmers.el" default-directory)))
            (when (file-exists-p shimmers-file)
              (load shimmers-file) (require 'shimmers))))))
     (kaocha-runner-extra-configuration . "{:config-file \"test/tests.edn\"}")
     (cider-clojure-cli-global-options . -A:perf)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby")))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

