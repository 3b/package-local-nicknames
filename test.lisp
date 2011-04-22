(defpackage #:foo
  (:use :cl)
  (:export #:foo)
  (:local-nicknames (:hoge :cl)
                    (:bar :cl-user)))

(in-package #:foo)

(defun foo () (format t "foo:foo!~%"))

(defparameter bar::*piyo* (hoge:+ bar::*foo* bar::*bar*))


(in-package #:cl)
(defpackage #:bar
  (:use :common-lisp)
  (:local-nicknames (:cl :foo)))

(in-package #:bar)
(cl:foo)