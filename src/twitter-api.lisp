(in-package #:cl-async-twitter)


(define-condition api-error (error)
  ((name :initarg :name :reader api-error-name)
   (url :initarg :url :reader api-error-url)
   (status :initarg :status :reader api-error-status)
   (msg :initarg :url :reader api-error-msg))
  (:report (lambda (c s)
             (print-unreadable-object (c s :type t :identity t)
               (format s "~a: ~a: ~a"
                       (api-error-name c)
                       (api-error-status c)
                       (api-error-msg c)))))
  (:documentation "Indicates an error returned by the Twitter APIs."))


(defun api-error-from-resp (name url resp)
  (make-condition
    'api-error
    :name name
    :url url
    :status (nth 0 resp)
    :msg (nth 1 resp)))


(defun catched-call (func &rest args)
  (catcher (apply func args)
           (t (e)
              (vom:error "~s" e)
              nil)))


(defun make-twitter-session (consumer-key consumer-secret
                             &optional access-token access-token-secret)
  (make-session :consumer-key consumer-key
                :consumer-secret consumer-secret
                :token access-token
                :token-secret access-token-secret
                :resource-base-url *resource-base-url*))


(defun cli-oob-verifier-cb (session)
  #'(lambda ()
      (format *query-io* "Authorization URL: ~a~%"
              (build-authorization-url session *authenticate-url*))
      (format *query-io* "Enter verifier: ")
      (finish-output *query-io*)
      (read-line)))


(defun resp-error (resp)
  (/= (nth 0 resp) 200))


(defun login (session request-token-cb verifier-cb)
  (alet ((resp (request-token
                 session
                 *request-token-url*
                 request-token-cb
                 :method :post)))
    (when (resp-error resp)
      (error (api-error-from-resp "login" *request-token-url* resp)))

    (alet* ((verifier (funcall verifier-cb))
            (resp (access-token
                    session
                    *access-token-url*
                    verifier
                    :method :post)))
      (when (resp-error resp)
        (error (api-error-from-resp "login" *access-token-url* resp)))
      (let ((resp-body (nth 1 resp)))
        (list (parse-integer (cdr (assoc "user_id" resp-body :test #'equal)))
              (cdr (assoc "screen_name" resp-body :test #'equal)))))))


(defun keep-alive-nl-p (line)
  (and (= 1 (length line))
       (eql #\return (aref line 0))))


(defun full-line-p (line)
  (and (> (length line) 0)
       (eql #\return (aref line (1- (length line))))))


(defun stated-streaming-body-parser (chunk start end state)
  (let* ((new-data (subseq chunk start end))
         (new-str (octets-to-string new-data))
         (buffered (gethash :buffer state))
         (full-str (concatenate 'string buffered new-str))
         (last-line "")
         results)
    (vom:debug "(length buffered) = ~s" (length buffered))
    (vom:debug "(length new-str) = ~s" (length new-str))
    (vom:debug "(length full-str) = ~s" (length full-str))
    (with-input-from-string (stream full-str)
      (loop for line = (read-line stream nil)
            when (null line) return nil
            do (cond
                 ((keep-alive-nl-p line)
                  (push :keep-alive results))
                 ((full-line-p line)
                  (push (decode-json-from-string line) results))
                 (t
                  (setf last-line line)))))
    (vom:debug "(length last-line) = ~s" (length last-line))
    (when (> (length last-line) 0)
      (push :partial-line results))
    (setf (gethash :buffer state) last-line)
    (nreverse results)))


(defun make-streaming-body-parser (message-cb)
  (let ((state (make-hash-table)))
    (setf (gethash :buffer state) "")
    #'(lambda (chunk start end)
        (let ((results (stated-streaming-body-parser chunk start end state)))
          (loop for m in results do (funcall message-cb m))))))


(defun start-streaming (session message-cb)
  (alet ((resp (streaming-request
                 session *user-stream-url*
                 :method :get
                 :body-cb (make-streaming-body-parser message-cb))))
    (when (resp-error resp)
      (error (api-error-from-resp "start-streaming" *user-stream-url* resp)))
    t))


(defun call-with-retries (fun &optional clean-up-fun retries (retry-interval 2))
  (labels ((do-call-fun (remaining-retries)
             (if (and (integerp remaining-retries)
                      (<= retries 0))
               (progn
                 (vom:debug "No more retries, stop.")
                 (if clean-up-fun
                   (catched-call clean-up-fun)))
               (alet ((result (catched-call fun)))
                 (vom:debug "call-with-retries: ~s: result = ~s" fun result)
                 (vom:debug "Retrying in ~a second(s)..." retry-interval)
                 (with-delay (retry-interval)
                   (do-call-fun (if (integerp retries)
                                   (1- retries)
                                   retries)))))))
    (do-call-fun retries)))


(defun friendships-create (session target-id)
  (alet ((resp (request
                 session *friendships-create-url*
                 :method :post
                 :params `(("user_id" . ,target-id)
                           ("follow" . "true")))))
    (cond
      ((not (resp-error resp))
       t)
      ((= (nth 0 resp) 403)
       t)
      (t
       (error (api-error-from-resp
                "friendships-create"
                *friendships-create-url*
                resp))))))


(defun direct-messages-new (session target-id text)
  (alet ((resp (request
                 session *direct-messages-new-url*
                 :method :post
                 :params `(("user_id" . ,target-id)
                           ("text" . ,text)))))
    (when (resp-error resp)
      (error (api-error-from-resp
               "direct-messages-new"
               *direct-messages-new-url*
               resp)))
    t))


(defun user-blocking-p (session user-id)
  (alet ((resp (request
                 session *statuses-user-timeline-url*
                 :method :get
                 :params `(("user_id" . ,user-id)
                           ("count" . 1)
                           ("trim_user" . 1)
                           ("include_rts" . 1)))))
    (cond
      ((not (resp-error resp))
       nil)
      ((= (nth 0 resp) 401)
       t)
      (t
       (error (api-error-from-resp
                "user-blocking-p"
                *statuses-user-timeline-url*
                resp))))))


(defun statuses-update (session status-text &optional in-reply-to)
  (alet* ((params (cons `("status" . ,status-text)
                       (if in-reply-to
                         `(("in_reply_to_status_id" . ,in-reply-to))
                         nil)))
          (resp (request
                  session *statuses-update-url*
                  :method :post
                  :params params)))
    (when (resp-error resp)
      (error (api-error-from-resp
               "statuses-update"
               *statuses-update-url*
               resp)))
    t))


(defun users-lookup (session user-id-list)
  (alet* ((user-ids-str (format nil "~{~a~^,~}" user-id-list))
          (resp (request
                  session *users-lookup-url*
                  :method :post
                  :params `(("user_id" . ,user-ids-str)))))
    (when (resp-error resp)
      (error (api-error-from-resp "users-lookup" *users-lookup-url* resp)))
    (nth 1 resp)))
