(in-package #:cl-user)


(defpackage #:cl-async-twitter
  (:nicknames #:twitter)
  (:use #:cl
        #:cl-async
        #:cl-async-oauth
        #:blackbird
        #:babel
        #:cl-json)
  (:export ;; url
           #:*resource-base-url*
           #:*request-token-url*
           #:*authenticate-url*
           #:*access-token-url*
           #:*user-stream-url*
           #:*friendships-create-url*
           #:*direct-messages-new-url*
           #:*statuses-user-timeline-url*
           #:*statuses-update-url*
           #:*users-lookup-url*

           ;; twitter-api
           #:make-twitter-session
           #:login
           #:cli-oob-verifier-cb
           #:resp-error
           #:start-streaming
           #:call-with-retries
           #:friendships-create
           #:direct-messages-new
           #:user-blocking-p
           #:statuses-update
           #:users-lookup
           
           ;; message
           #:access-json
           #:user-status-p
           #:status-rt-text-p
           #:status-rt-p
           #:user-mentioned-p
           #:dm-to-user-p
           #:filter-user-mention
           #:build-status-text
           #:trim-twitter-text
           #:trim-spaced-text
           ))
