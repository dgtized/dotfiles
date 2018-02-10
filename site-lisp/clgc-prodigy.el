(require 'prodigy)

(prodigy-define-tag :name 'rails
  :ready-message "Listening on 0\\.0\\.0\\.0:[0-9]+, CTRL\\+C to stop")

(prodigy-define-tag
  :name 'zeus
  :command "zeus"
  :args '("--simple-status" "start")
  :ready-message "test_helper status: ready")

(prodigy-define-tag :name 'resque-pool
  :ready-message "Starting worker")

(prodigy-define-tag :name 'resque-scheduler
  :ready-message "Schedules Loaded")

(prodigy-define-tag :name 'dynamo-db
  :ready-message "Initializing DynamoDB Local with the following configuration:")

(provide 'clgc-prodigy)
