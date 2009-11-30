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
    (encode-datum (string-to-octets data))))

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
