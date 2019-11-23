;;;; Radiance Launcher
;;; Please load this file in script mode.
;;; Example:
;;;   ros start.lisp

;;; Sanity checks.
(unless *load-pathname*
  (error "Please LOAD this file."))

(defvar *root* (make-pathname :name NIL :type NIL :defaults *load-pathname*))
(defun path (pathname)
  (merge-pathnames pathname *root*))

;; Load Radiance and configure it.
(ql:quickload '(#-sbcl prepl radiance))

(defmethod radiance:environment-directory (environment (kind (eql :configuration)))
  (path (make-pathname :directory `(:relative "config" ,environment))))

(defmethod radiance:environment-directory (environment (kind (eql :cache)))
  (path (make-pathname :directory `(:relative "cache" ,environment))))

(defmethod radiance:environment-directory (environment (kind (eql :data)))
  (path (make-pathname :directory `(:relative "data" ,environment))))

(defmethod radiance:environment-directory (environment (kind (eql :template)))
  (path (make-pathname :directory `(:relative "override" ,environment "template"))))

(defmethod radiance:environment-directory (environment (kind (eql :static)))
  (path (make-pathname :directory `(:relative "override" ,environment "static"))))

(radiance:startup)

;;; Load all user modules and things.
(mapcar #'ql:quickload
        (radiance:find-all-modules (path "modules/")))
(load (path "setup.lisp"))

;;; Boot to REPL.
(sleep 1)
(in-package #:rad-user)
(unwind-protect
     (progn
       #-sbcl (prepl:repl)
       #+sbcl (sb-ext:enable-debugger)
       #+sbcl (sb-impl::toplevel-init))
  (when (radiance:started-p)
    (radiance:shutdown)))
