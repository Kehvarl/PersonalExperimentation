(in-package :cl-user)

(defpackage :com.kehvarl.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))

(defpackage :com.kehvarl.spam
  (:use :common-lisp :com.kehvarl.pathnames))
