(in-package :cffi-gdbm)

(defparameter *gdbm* nil)

(defun db-open (file &key (block-size 0) (flags :wrcreat) (mode '(:irusr :iwusr)) (fatal-callback nil))
  (%gdbm-open (namestring file)
              block-size
              flags
              mode
              (if fatal-callback
                  (get-callback fatal-callback)
                  (null-pointer))))

(defun db-close (&optional (gdbm *gdbm*))
  (%gdbm-close gdbm))

(defmacro with-gdbm ((file &rest options) &body body)
  `(let ((*gdbm* (apply #'db-open ,file ,options)))
     (unwind-protect
          (progn ,@body)
       (db-close *gdbm*))))
