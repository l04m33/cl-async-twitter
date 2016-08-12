(in-package #:cl-user)

(defpackage #:message-test
  (:use #:cl
        #:prove
        #:cl-async-twitter))

(in-package #:message-test)


(plan 25)

(ok
  (not
    (status-rt-text-p '() "dummy text"))
  "status-rt-text-p - empty mentions")

(ok
  (not
    (status-rt-text-p '(((:screen--name . "dummy")
                     (:name . "Dummy Name")
                     (:id . 1234)
                     (:id--str . "1234")
                     (:indices 0 6)))
                  "@dummy blah blah blah"))
  "status-rt-text-p - negative")

(ok
  (not
    (status-rt-text-p '(((:screen--name . "dummy")
                     (:name . "Dummy Name")
                     (:id . 1234)
                     (:id--str . "1234")
                     (:indices 10 16)))
                  "blah blah @dummy blah blah blah"))
  "status-rt-text-p - negative 2")

(ok
  (status-rt-text-p '(((:screen--name . "dummy")
                   (:name . "Dummy Name")
                   (:id . 1234)
                   (:id--str . "1234")
                   (:indices 3 9)))
                "RT @dummy blah blah blah")
  "status-rt-text-p - positive uppercase")

(ok
  (status-rt-text-p '(((:screen--name . "dummy")
                   (:name . "Dummy Name")
                   (:id . 1234)
                   (:id--str . "1234")
                   (:indices 2 9)))
                "rt @dummy blah blah blah")
  "status-rt-text-p - positive lowercase")

(ok
  (status-rt-text-p '(((:screen--name . "dummy")
                   (:name . "Dummy Name")
                   (:id . 1234)
                   (:id--str . "1234")
                   (:indices 2 9)))
                "RT@dummy blah blah blah")
  "status-rt-text-p - positive no-space")

(ok
  (status-rt-text-p '(((:screen--name . "dummy")
                   (:name . "Dummy Name")
                   (:id . 1234)
                   (:id--str . "1234")
                   (:indices 0 6))
                  ((:screen--name . "dummy2")
                   (:name . "Dummy Name 2")
                   (:id . 2345)
                   (:id--str . "2345")
                   (:indices 15 22)))
                "@dummy blah RT @dummy2 blah blah")
  "status-rt-text-p - positive multiple mentions")

(ok
  (status-rt-text-p '(((:screen--name . "dummy")
                   (:name . "Dummy Name")
                   (:id . 1234)
                   (:id--str . "1234")
                   (:indices 3 9))
                  ((:screen--name . "dummy2")
                   (:name . "Dummy Name 2")
                   (:id . 2345)
                   (:id--str . "2345")
                   (:indices 18 25)))
                "RT @dummy blah RT @dummy2 blah blah")
  "status-rt-text-p - positive multiple RT")


(ok
  (user-mentioned-p 1234 '(((:screen--name . "dummy")
                               (:name . "Dummy Name")
                               (:id . 1234)
                               (:id--str . "1234")
                               (:indices 0 6))
                              ((:screen--name . "dummy2")
                               (:name . "Dummy Name 2")
                               (:id . 2345)
                               (:id--str . "2345")
                               (:indices 15 22))))
  "user-mentioned-p - positive")

(ok
  (not
    (user-mentioned-p 1234 '(((:screen--name . "dummy2")
                                 (:name . "Dummy Name 2")
                                 (:id . 2345)
                                 (:id--str . "2345")
                                 (:indices 15 22)))))
  "user-mentioned-p - negative")

(ok
  (not
    (user-mentioned-p 1234 '()))
  "user-mentioned-p - empty mentions")

(is
  (filter-user-mention 1234 '(((:screen--name . "dummy")
                                       (:name . "Dummy Name")
                                       (:id . 1234)
                                       (:id--str . "1234")
                                       (:indices 0 6))
                                      ((:screen--name . "dummy2")
                                       (:name . "Dummy Name 2")
                                       (:id . 2345)
                                       (:id--str . "2345")
                                       (:indices 15 22))))
  '(((:screen--name . "dummy2")
     (:name . "Dummy Name 2")
     (:id . 2345)
     (:id--str . "2345")
     (:indices 15 22)))
  :test #'equal
  "filter-user-mention")

(is
  (build-status-text nil nil "@twitter : #dummy #tags #str")
  "@twitter : #dummy #tags #str"
  :test #'equal
  "build-status-text - null user and null mentions")

(is
  (build-status-text
    '((:id . 1234) (:screen--name . "dummy"))
    nil "@twitter : #dummy #tags #str")
  "@dummy @twitter : #dummy #tags #str"
  :test #'equal
  "build-status-text - null mentions")

(is
  (build-status-text
    nil '(((:id . 1234) (:screen--name . "dummy"))
          ((:id . 2345) (:screen--name . "dummy2")))
    "dummy str")
  "@dummy @dummy2 dummy str"
  :test #'equal
  "build-status-text - null user")

(is
  (build-status-text
    '((:id . 1234) (:screen--name . "dummy"))
    '(((:id . 2345) (:screen--name . "dummy2"))
      ((:id . 3456) (:screen--name . "dummy3")))
    "dummy str")
  "@dummy @dummy2 @dummy3 dummy str"
  :test #'equal
  "build-status-text")

(is
  (build-status-text
    '((:id . 1234) (:screen--name . "dummy"))
    '(((:id . 1234) (:screen--name . "dummy"))
      ((:id . 2345) (:screen--name . "dummy2")))
    "dummy str")
  "@dummy @dummy2 dummy str"
  :test #'equal
  "build-status-text - duplicate user")

(is
  (build-status-text
    '((:id . 1234) (:screen--name . "dummy"))
    '(((:id . 2345) (:screen--name . "dummy2"))
      ((:id . 2345) (:screen--name . "dummy2")))
    "dummy str")
  "@dummy @dummy2 dummy str"
  :test #'equal
  "build-status-text - duplicate mentions")

(is
  (trim-spaced-text "@dummy #tag1 #tag2")
  "@dummy #tag1 #tag2"
  :test #'equal
  "trim-spaced-text - short string")

(let* ((orig-str (with-output-to-string (output)
                   (loop for n = (random 10000)
                         for tag = "@dummy :" then (format nil " #測試~a" n)
                         for total-len = (length "@dummy :") then (+ total-len (length tag))
                         do (write-string tag output)
                         when (> total-len 140) return nil)))
       (trimmed-str (trim-spaced-text orig-str)))

  (ok
    (and (> (length orig-str) 140)
         (<= (length trimmed-str) 140))
    "trim-spaced-text - long string length")

  (is
    (subseq trimmed-str (- (length trimmed-str) (length " ....")))
    " ...."
    :test #'equal
    "trim-spaced-text - ends with \" ....\""
    )

  (is
    (aref orig-str (- (length trimmed-str) 5))
    #\space
    "trim-spaced-text - trimmed at space character"))

(let* ((orig-str (with-output-to-string (output)
                   (loop for n = (random 10000)
                         for tag = "@dummy : #" then (format nil "測試~a" n)
                         for total-len = (length "@dummy :") then (+ total-len (length tag))
                         do (write-string tag output)
                         when (> total-len 140) return nil)))
       (trimmed-str (trim-spaced-text orig-str)))
  (is trimmed-str "@dummy : ...."
      :test #'equal
      "trim-spaced-text - long tag without spaces"))

(let* ((orig-str (with-output-to-string (output)
                   (loop for n = (random 10000)
                         for tag = "@dummy:#" then (format nil "測試~a" n)
                         for total-len = (length "@dummy :") then (+ total-len (length tag))
                         do (write-string tag output)
                         when (> total-len 140) return nil)))
       (trimmed-str (trim-spaced-text orig-str)))
  (is (length trimmed-str) 140
      "trim-spaced-text - length of long string without spaces")
  (is
    (subseq trimmed-str (- (length trimmed-str) (length "....")))
    "...."
    :test #'equal
    "trim-spaced-text - long string without spaces ends with \"....\""))

(finalize)
