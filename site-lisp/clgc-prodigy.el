(require 'prodigy)

(prodigy-define-tag :name 'rails
  :ready-message "Listening on 0\\.0\\.0\\.0:[0-9]+, CTRL\\+C to stop")

(prodigy-define-tag
  :name 'zeus
  :command "/bin/bash"
  ;; Strip zeus start of color codes to prevent ansi out-of-bound errors
  :args '("-c" "zeus start | sed \"s,\x1B\[[0-9;]*[a-zA-Z],,g\"")
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t
  :on-output
  (prodigy-callback (service)
    (let ((poll-zeus "ps aux | grep -c 'zeus slave:'"))
      (when (>= (string-to-number (shell-command-to-string poll-zeus)) 6)
        (prodigy-set-status service 'ready)))))

(prodigy-define-tag :name 'resque-pool
  :ready-message "Starting worker")

(prodigy-define-tag :name 'resque-scheduler
  :ready-message "Schedules Loaded")

(prodigy-define-tag :name 'dynamo-db
  :ready-message "Initializing DynamoDB Local with the following configuration:")

(provide 'clgc-prodigy)
