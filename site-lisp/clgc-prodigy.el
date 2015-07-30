(require 'prodigy)

(prodigy-define-tag :name 'rails
  :ready-message "Listening on 0\\.0\\.0\\.0:[0-9]+, CTRL\\+C to stop")

(prodigy-define-tag
  :name 'zeus
  :on-output
  (lambda (&rest args)
    (let ((service (plist-get args :service))
          (poll-zeus "ps aux | grep 'zeus slave' | grep -c environment"))
      (when (>= (string-to-number (shell-command-to-string poll-zeus)) 2)
        (prodigy-set-status service 'ready)))))

(prodigy-define-tag :name 'resque-pool
  :ready-message "Starting worker")

(provide 'clgc-prodigy)
