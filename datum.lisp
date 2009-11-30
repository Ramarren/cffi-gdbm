(in-package :cffi-gdbm)

(defgeneric encode-datum (data)
  (:method ((data vector))
    (etypecase data
      ((vector (unsigned-byte 8))
         (let ((dptr (foreign-alloc :uint8 :initial-contents data :count (length data))))
           (make-datum :dptr dptr :dsize (length data))))
      ((vector (unsigned-byte 32))
         (let ((dptr (foreign-alloc :uint32 :initial-contents data :count (length data))))
           (make-datum :dptr dptr :dsize (* 4 (length data)))))))
  (:method ((data string))
    (encode-datum (string-to-octets data))))
