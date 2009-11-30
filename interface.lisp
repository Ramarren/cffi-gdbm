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

(defun store (key content &optional (flag :insert) (gdbm *gdbm*))
  (let ((retval (datum-store key content flag gdbm)))
    (ecase retval
      (-1 (error "Not writer or invalid data, error: ~a" gdbm-errno))
      (+1 (error "Key ~a already exists, error: ~a" key gdbm-errno))
      (0 t))))

(defun exists (key &optional (gdbm *gdbm*))
  (not (zerop (datum-exists key gdbm))))

(defun fetch (key &optional (as :string) (gdbm *gdbm*))
  (multiple-value-call #'decode-datum (datum-fetch key gdbm) as))

(defun db-delete (key &optional (gdbm *gdbm*))
  (let ((retval (datum-delete key gdbm)))
    (ecase retval
      (-1 (error "Key ~a does not exist or not a writer, error ~a" key gdbm-errno))
      (0 t))))

(defun make-key-iterator (&optional (as :string) (gdbm *gdbm*))
  (let ((key nil)
        (done nil))
    #'(lambda ()
        (if (and key (not done))
            (setf key (multiple-value-call #'decode-datum
                        (datum-nextkey key gdbm) as))
            (setf key (multiple-value-call #'decode-datum
                        (datum-firstkey gdbm) as)))
        (unless key
          (setf done t))
        key)))

(defun sync (&optional (gdbm *gdbm*))
  (%gdbm-sync gdbm)
  t)

(defun reorganize (&optional (gdbm *gdbm*))
  (let ((retval (%gdbm-reorganize gdbm)))
    (cond ((zerop retval) t)
          ((minusp retval) (error "Reorganization error: ~a" gdbm-errno)))))

(defun get-error-string ()
  (%gdbm-strerror gdbm-errno-raw))
