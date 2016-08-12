(in-package #:cl-async-twitter)


(defun access-json (parsed-json &rest keys)
  (loop for k in keys
        for p = (assoc k parsed-json) then (assoc k v)
        for v = (cdr p)
        for v-p = (not (not p))
        finally (return (values v v-p))))


(defun user-status-p (user-id message)
  (let ((msg-user-id (access-json message :user :id)))
    (= msg-user-id user-id)))


(defun status-rt-text-p (mentions text)
  (labels ((look-back-for-rt (text idx)
             (if (and (>= idx 0) (eql (aref text idx) #\space))
               (look-back-for-rt text (1- idx))
               (if (> idx 0)
                 (or (and (eql #\R (aref text (1- idx))) (eql #\T (aref text idx)))
                     (and (eql #\r (aref text (1- idx))) (eql #\t (aref text idx))))
                 nil)))
           (mention-rt-p (mentions)
             (if mentions
               (let* ((m (car mentions))
                      (m-range (access-json m :indices))
                      (m-start (nth 0 m-range)))
                 (or (look-back-for-rt text (1- m-start))
                     (mention-rt-p (cdr mentions))))
               nil)))
    (mention-rt-p mentions)))


(defun status-rt-p (message)
  (or (access-json message :retweeted--status)
      (status-rt-text-p
        (access-json message :entities :user--mentions)
        (access-json message :text))))


(defun user-mentioned-p (user-id mentions)
  (dolist (m mentions nil)
    (when (= user-id (access-json m :id))
      (return-from user-mentioned-p m))))


(defun dm-to-user-p (user-id message)
  (let ((target-id (access-json
                     message
                     :direct--message :recipient :id)))
    (= target-id user-id)))


(defun filter-user-mention (user-id mentions)
  (loop for m in mentions
        when (/= user-id (access-json m :id))
        collect m))


(defun build-status-text (to-user mentions text)
  (with-output-to-string (output)
    (when to-user
      (format output "@~a " (access-json to-user :screen--name)))
    (loop for m in mentions
          with user-list = (if to-user
                             `(,(access-json to-user :screen--name))
                             nil)
          do (let ((cur-screen-name (access-json m :screen--name)))
               (when (not (find cur-screen-name user-list :test #'equal))
                 (push cur-screen-name user-list)
                 (format output "@~a " cur-screen-name))))
    (format output "~a" text)))


(defun trim-twitter-text (text)
  (if (> (length text) 140)
    (concatenate 'string (subseq text 0 136) "....")
    text))


(defun trim-spaced-text (text)
  (labels ((find-last-space (text)
             (loop for c across text
                   for i from 0
                   with last-space = -1
                   do (cond
                        ((>= i 136)
                         (return-from find-last-space last-space))
                        ((eql #\space (aref text i))
                         (setf last-space i)))
                   finally (return last-space))))
    (if (> (length text) 140)
      (let ((last-space (find-last-space text)))
        (if (>= last-space 0)
          (concatenate 'string (subseq text 0 last-space) " ....")
          (trim-twitter-text text)))
      text)))
