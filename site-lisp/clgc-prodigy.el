(require 'prodigy)

(prodigy-define-tag :name 'rails
  :ready-message "Listening on 0\\.0\\.0\\.0:[0-9]+, CTRL\\+C to stop")

(prodigy-define-tag
  :name 'zeus
  :on-output
  (prodigy-callback (service)
    (let ((poll-zeus "ps aux | grep -c 'zeus slave:'"))
      (when (>= (string-to-number (shell-command-to-string poll-zeus)) 6)
        (prodigy-set-status service 'ready)))))

(prodigy-define-tag :name 'resque-pool
  :ready-message "Starting worker")

(provide 'clgc-prodigy)
