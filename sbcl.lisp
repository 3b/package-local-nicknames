;;(in-package :package-local-nicknames)

#-sbcl (error "sbcl only for now...")

(in-package :sb-impl)


;;; since we can't modify the PACKAGE struct, we store the aliases externally
;;; in a weak key hash table from package objects ->
;;;  hash of nickname -> real-name
(defparameter *packages-with-local-nicknames*
  (make-hash-table :weakness :key))

(defun package-local-nicknames (package-designator)
  (let* ((package (find-package package-designator))
         (local-nicknames (gethash package *packages-with-local-nicknames*))
         (list nil))
    (when local-nicknames
      (maphash (lambda (k v) (push (list k v) list)) local-nicknames))
    list))

(defun set-package-local-nicknames (package-designator nicknames)
  (let* ((package (find-package package-designator))
         (ht (make-hash-table :test 'equal)))
    (assert package nil "no package for designator ~s in set-package-local-nicknames" package-designator)
    (loop for (n rn) in nicknames
       do (setf (gethash n ht) rn))
    (setf (gethash package *packages-with-local-nicknames*) ht)))

;;; can't redefine %defpackage, since existing defpackage forms would
;;; have already expanded to calls with the original signature
(defun %defpackage-pln (name nicknames size shadows shadowing-imports
                        use imports interns exports implement lock
                        local-nicknames doc-string
                        source-location)
  (declare (type simple-string name)
           (type list nicknames shadows shadowing-imports
                 imports interns exports local-nicknames)
           (type (or list (member :default)) use)
           (type (or simple-string null) doc-string)
           ;;#-package-local-nicknames
           ;;(ignore local-nicknames)
           #-sb-package-locks
           (ignore implement lock))
  (with-package-graph ()
    (let* ((existing-package (find-package name))
           (use (use-list-packages existing-package use))
           (shadowing-imports (import-list-symbols shadowing-imports))
           (imports (import-list-symbols imports)))
      (if existing-package
          (prog1
            (update-package-with-variance existing-package name
                                          nicknames source-location
                                          shadows shadowing-imports
                                          use imports interns exports
                                          implement lock doc-string)
            (let ((old-local-nicknames (package-local-nicknames existing-package)))
              (setf old-local-nicknames (set-difference old-local-nicknames local-nicknames :test #'equal))
              (when old-local-nicknames
                (warn 'package-at-variance
                      :format-control "~A also has the following local nicknames: ~% ~{ ~S -> ~S ~}"
                      :format-arguments (cons name old-local-nicknames)))
              (set-package-local-nicknames existing-package local-nicknames)))
          (let ((package (make-package name
                                       :use nil
                                       :internal-symbols (or size 10)
                                       :external-symbols (length exports))))
            (prog1
                (update-package package
                                nicknames source-location
                                shadows shadowing-imports
                                use imports interns exports
                                implement lock doc-string)
              (set-package-local-nicknames package local-nicknames)))))))

(defun find-global-package (package-designator)
  (flet ((find-package-from-string (string)
           (declare (type string string))
           (let ((packageoid (gethash string *package-names*)))
             (when (and (null packageoid)
                        (not *in-package-init*) ; KLUDGE
                        (let ((mismatch (mismatch "SB!" string)))
                          (and mismatch (= mismatch 3))))
               (restart-case
                   (signal 'bootstrap-package-not-found :name string)
                 (debootstrap-package ()
                   (return-from find-global-package
                     (if (string= string "SB!XC")
                         (find-global-package "COMMON-LISP")
                         (find-global-package
                          (substitute #\- #\! string :count 1)))))))
             packageoid)))
    (typecase package-designator
      (package package-designator)
      (symbol (find-package-from-string (symbol-name package-designator)))
      (string (find-package-from-string package-designator))
      (character (find-package-from-string (string package-designator)))
      (t (error 'type-error
                :datum package-designator
                :expected-type '(or character package string symbol))))))

;; todo: real API for this...
(defun find-package-using-package (name package-designator &key (errorp t))
  (when (packagep name)
    (return-from find-package-using-package name))
  (check-type name (or symbol string character) "package-designator")
  (let* ((package (if (packagep package-designator)
                      package-designator
                      (find-package package-designator)))
         (local-nicknames (gethash package *packages-with-local-nicknames*))
         (real-name (when local-nicknames (gethash (string name)
                                                   local-nicknames)))
         (real-package (when real-name (find-global-package real-name))))
    ;; should not finding a package be an error?
    (when (and real-name (not real-package) errorp)
      ;; todo: real error
      (error "package nickname ~s in package ~s points to non-existant package ~s" name (package-name package) real-name))
    ;; return t/nil in 2nd value to indicate whether a nickname was defined
    ;; in case we didn't signal an error
    (values real-package (and real-name t))))

;;; trying to redefine find-package tends to break things, so define it
;;; with another name and (setf fdefinition) later
(defun find-package-pln (package-designator)
  (or (and (boundp '*package*)
           (find-package-using-package package-designator *package*))
      (find-global-package package-designator)))


(without-package-locks
  (defmacro defpackage (package &rest options)
    #+sb-doc
    #.(format nil
    "Defines a new package called PACKAGE. Each of OPTIONS should be one of the
    following: ~{~&~4T~A~}
    All options except ~{~A, ~}and :DOCUMENTATION can be used multiple
    times."
              '((:nicknames "{package-name}*")
                (:size "<integer>")
                (:shadow "{symbol-name}*")
                (:shadowing-import-from "<package-name> {symbol-name}*")
                (:use "{package-name}*")
                (:import-from "<package-name> {symbol-name}*")
                (:intern "{symbol-name}*")
                (:export "{symbol-name}*")
                #+sb-package-locks (:implement "{package-name}*")
                #+sb-package-locks (:lock "boolean")
                ;;#!+package-local-nicknames
                (:local-nicknames "(<nickname> <real-name>)*)")
                (:documentation "doc-string"))
              '(:size #+sb-package-locks :lock))
    (let ((nicknames nil)
          (size nil)
          (shadows nil)
          (shadowing-imports nil)
          (use nil)
          (use-p nil)
          (imports nil)
          (interns nil)
          (exports nil)
          (implement (stringify-package-designators (list package)))
          (implement-p nil)
          (lock nil)
          (local-nicknames nil)
          (doc nil))
      #-sb-package-locks
      (declare (ignore implement-p))
      (dolist (option options)
        (unless (consp option)
          (error 'simple-program-error
                 :format-control "bogus DEFPACKAGE option: ~S"
                 :format-arguments (list option)))
        (case (car option)
          (:nicknames
           (setf nicknames (stringify-package-designators (cdr option))))
          (:size
           (cond (size
                  (error 'simple-program-error
                         :format-control "can't specify :SIZE twice."))
                 ((and (consp (cdr option))
                       (typep (second option) 'unsigned-byte))
                  (setf size (second option)))
                 (t
                  (error
                   'simple-program-error
                   :format-control ":SIZE is not a positive integer: ~S"
                   :format-arguments (list (second option))))))
          (:shadow
           (let ((new (stringify-string-designators (cdr option))))
             (setf shadows (append shadows new))))
          (:shadowing-import-from
           (let ((package-name (stringify-package-designator (second option)))
                 (names (stringify-string-designators (cddr option))))
             (let ((assoc (assoc package-name shadowing-imports
                                 :test #'string=)))
               (if assoc
                   (setf (cdr assoc) (append (cdr assoc) names))
                   (setf shadowing-imports
                         (acons package-name names shadowing-imports))))))
          (:use
           (setf use (append use (stringify-package-designators (cdr option)) )
                 use-p t))
          (:import-from
           (let ((package-name (stringify-package-designator (second option)))
                 (names (stringify-string-designators (cddr option))))
             (let ((assoc (assoc package-name imports
                                 :test #'string=)))
               (if assoc
                   (setf (cdr assoc) (append (cdr assoc) names))
                   (setf imports (acons package-name names imports))))))
          (:intern
           (let ((new (stringify-string-designators (cdr option))))
             (setf interns (append interns new))))
          (:export
           (let ((new (stringify-string-designators (cdr option))))
             (setf exports (append exports new))))
          #+sb-package-locks
          (:implement
           (unless implement-p
             (setf implement nil))
           (let ((new (stringify-package-designators (cdr option))))
             (setf implement (append implement new)
                   implement-p t)))
          #+sb-package-locks
          (:lock
           (when lock
             (error 'simple-program-error
                    :format-control "multiple :LOCK options"))
           (setf lock (coerce (second option) 'boolean)))
          ;;#!+package-local-nicknames
          (:local-nicknames
           (let ((new (mapcar #'stringify-package-designators
                              (cdr option))))
             ;; fixme: expand list if we allow multiple nicknames
             ;; in one clause? (ex: (:real-name :foo :bar) ->
             ;; (:real-name :foo) (:real-name :bar)
             (setf local-nicknames (append local-nicknames new))))
          (:documentation
           (when doc
             (error 'simple-program-error
                    :format-control "multiple :DOCUMENTATION options"))
           (setf doc (coerce (second option) 'simple-string)))
          (t
           (error 'simple-program-error
                  :format-control "bogus DEFPACKAGE option: ~S"
                  :format-arguments (list option)))))
      (check-disjoint `(:intern ,@interns) `(:export  ,@exports))
      (check-disjoint `(:intern ,@interns)
                      `(:import-from
                        ,@(apply #'append (mapcar #'rest imports)))
                      `(:shadow ,@shadows)
                      `(:shadowing-import-from
                        ,@(apply #'append (mapcar #'rest shadowing-imports))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (%defpackage-pln ,(stringify-string-designator package) ',nicknames ',size
                          ',shadows ',shadowing-imports ',(if use-p use :default)
                          ',imports ',interns ',exports ',implement ',lock
                          ',local-nicknames ',doc
                          (sb-c:source-location))))))

(without-package-locks
 (setf (fdefinition 'find-package)
       #'find-package-pln))
