(in-package :cffi-gdbm)

(defgeneric encode-datum (data)
  (:method ((data vector))
    (etypecase data
      ((vector (unsigned-byte 8))
         (let ((dptr (foreign-alloc :uint8 :initial-contents data :count (length data))))
           (values dptr (length data))))
      ((vector (unsigned-byte 32))
         (let ((dptr (foreign-alloc :uint32 :initial-contents data :count (length data))))
           (values dptr (* 4 (length data)))))))
  (:method ((data string))
    (foreign-string-alloc data :null-terminated-p nil))
  (:documentation "Encode DATA and return a pointer to content and content length in bytes."))

(defclass vector-decoder ()
  ((foreign-element-type :accessor foreign-element-type-of :initarg :foreign-element-type)
   (vector-element-type  :accessor vector-element-type-of  :initarg :vector-element-type)))

(defparameter *u8-decoder* (make-instance 'vector-decoder
                                          :foreign-element-type :uint8
                                          :vector-element-type '(unsigned-byte 8)))

(defgeneric decode-datum (dptr dsize as)
  (:method :around (dptr dsize as)
    (unless (null-pointer-p dptr)
      (prog1
          (call-next-method)
        (foreign-free dptr))))
  (:method (dptr dsize (as (eql :string)))
    (nth-value 0 (foreign-string-to-lisp dptr :count dsize)))
  (:method (dptr dsize (as vector-decoder))
    (let* ((foreign-element-type (foreign-element-type-of as))
           (vector-element-type (vector-element-type-of as))
           (foreign-type-size (foreign-type-size foreign-element-type)))
      (assert (zerop (mod dsize foreign-type-size)))
      (let ((out-vector (make-array (/ dsize foreign-type-size)
                                    :element-type vector-element-type)))
        (dotimes (i (/ dsize foreign-type-size) out-vector)
          (setf (aref out-vector i) (mem-aref dptr foreign-element-type i))))))
  (:documentation "Decode datum given by DPTR and DSIZE using decoder designator AS."))

(defmacro with-datum ((data dptr dsize) &body body)
  `(multiple-value-bind (,dptr ,dsize) (encode-datum ,data)
     (unwind-protect
          (progn
            ,@body)
       (foreign-free ,dptr))))

(defun datum-store (key content flag database)
  (with-datum (key key-dptr key-dsize)
    (with-datum (content content-dptr content-dsize)
      (%gdbm-store database key-dptr key-dsize content-dptr content-dsize flag))))

(defun datum-fetch (key database)
  (with-foreign-object (fdatum 'foreign-datum)
    (with-datum (key dptr dsize)
      (%gdbm-fetch fdatum database dptr dsize))
    (values (foreign-slot-value fdatum 'foreign-datum 'dptr)
            (foreign-slot-value fdatum 'foreign-datum 'dsize))))

(defun datum-exists (key database)
  (not (zerop
        (with-datum (key dptr dsize)
          (%gdbm-exists database dptr dsize)))))

(defun datum-delete (key database)
  (not (zerop
        (with-datum (key dptr dsize)
          (%gdbm-delete database dptr dsize)))))

(defun datum-firstkey (database)
  (with-foreign-object (fdatum 'foreign-datum)
    (%gdbm-firstkey fdatum database)
    (values (foreign-slot-value fdatum 'foreign-datum 'dptr)
            (foreign-slot-value fdatum 'foreign-datum 'dsize))))

(defun datum-nextkey (key database)
  (with-foreign-object (fdatum 'foreign-datum)
    (with-datum (key dptr dsize)
      (%gdbm-nextkey fdatum database dptr dsize))
    (values (foreign-slot-value fdatum 'foreign-datum 'dptr)
            (foreign-slot-value fdatum 'foreign-datum 'dsize))))
