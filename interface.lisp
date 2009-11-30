(in-package :cffi-gdbm)

(defparameter *gdbm* nil)

(defun get-error-string ()
  (%gdbm-strerror gdbm-errno-raw))

(defun db-open (file &key (block-size 0) (flags :wrcreat) (mode '(:irusr :iwusr)) (fatal-callback nil) (on-fail :error))
  (let ((retval
         (%gdbm-open (namestring file)
                     block-size
                     flags
                     mode
                     (if fatal-callback
                         (get-callback fatal-callback)
                         (null-pointer)))))
    (if (null-pointer-p retval)
        (if (eql on-fail :error)
            (error "Cannot open database ~a, error: ~a" file (get-error-string))
            nil)
        retval)))

(defun db-close (&optional (gdbm *gdbm*))
  (when (and gdbm (not (null-pointer-p gdbm)))
    (%gdbm-close gdbm)))

(defun call-with-gdbm (gdbm function)
  (unwind-protect
       (funcall function gdbm)
    (db-close gdbm)))

(defmacro with-gdbm ((file &rest options) &body body)
  "Execute BODY with database FILE open with options (evaluated) and bound to *GDBM*."
  (with-unique-names (gdbm gdbm-body)
    `(when-let (,gdbm (funcall #'db-open ,file ,@options))
       (call-with-gdbm ,gdbm
                       #'(lambda (,gdbm-body)
                           (let ((*gdbm* ,gdbm-body))
                             ,@body))))))

(defun store (key content &optional (flag :insert) (gdbm *gdbm*))
  (let ((retval (datum-store key content flag gdbm)))
    (ecase retval
      (-1 (error "Not writer or invalid data, error: ~a" (get-error-string)))
      (+1 (error "Key ~a already exists, error: ~a" key (get-error-string)))
      (0 t))))

(defun exists (key &optional (gdbm *gdbm*))
  (not (zerop (datum-exists key gdbm))))

(defun fetch (key &optional (as :string) (gdbm *gdbm*))
  (multiple-value-call #'decode-datum (datum-fetch key gdbm) as))

(defun db-delete (key &optional (gdbm *gdbm*))
  (let ((retval (datum-delete key gdbm)))
    (ecase retval
      (-1 (error "Key ~a does not exist or not a writer, error ~a" key (get-error-string)))
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
          ((minusp retval) (error "Reorganization error: ~a" (get-error-string))))))

