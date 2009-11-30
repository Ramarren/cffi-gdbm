(in-package :cffi-gdbm)

;;; library

(define-foreign-library gdbm
  (:unix "/home/ramarren/C/gdbm/gdbm-1.8.3/.libs/libgdbm.so"))

(load-foreign-library 'gdbm)

;;; types

(defstruct datum
  dptr
  (dsize 0 :type (unsigned-byte 32)))

(define-foreign-type gdbm-datum ()
  ()
  (:actual-type :uint64)
  (:simple-parser gdbm-datum))

(define-parse-method gdbm-datum ()
  (make-instance 'gdbm-datum))

;; (defmethod translate-to-foreign ((value datum) (type gdbm-datum))
;;   (let ((data (logior (ash (datum-dsize value)  32)
;;                       (pointer-address (datum-dptr value)))))
;;     (format t "~16,'0x" data)
;;     data))

(defcstruct foreign-datum
  (dptr :pointer)
  (dsize :int))

(defmethod translate-to-foreign ((value datum) (type gdbm-datum))
  (with-foreign-object (fdatum 'foreign-datum)
    (setf (foreign-slot-value fdatum 'foreign-datum 'dptr) (datum-dptr value)
          (foreign-slot-value fdatum 'foreign-datum 'dsize) (datum-dsize value))
    (mem-ref fdatum :uint64)))

;; (defmethod translate-from-foreign (value (type gdbm-datum))
;;   (let ((dptr (ldb (byte 32 32) value)))
;;     (unless (zerop dptr)
;;       (make-datum :dptr (make-pointer dptr)
;;                   :dsize (ldb (byte 32 0) value)))))

(defmethod translate-from-foreign (value (type gdbm-datum))
  (with-foreign-object (fdatum 'foreign-datum)
    (setf (mem-ref fdatum :uint64) value)
    (make-datum :dptr (foreign-slot-value fdatum 'foreign-datum 'dptr)
                :dsize (foreign-slot-value fdatum 'foreign-datum 'dsize))))

(defctype gdbm-file :pointer)

(defbitfield gdbm-flags
  (:reader 0)
  (:writer 1)
  (:wrcreat 2)
  (:newdb 3)
  (:fast #x10)
  (:sync #x20)
  (:nolock #x40))

(defbitfield mode
  (:isuid #o04000)
  (:isgid #o02000)
  (:isvtx #o01000)
  (:irusr #o00400)
  (:iwusr #o00200)
  (:ixusr #o00100)
  (:irgrp #o00040)
  (:iwgrp #o00020)
  (:ixgrp #o00010)
  (:iroth #o00004)
  (:iwoth #o00002)
  (:ixoth #o00001))

(defcenum gdbm-error-code
  :no-error
  :malloc-error
  :block-size-error
  :file-open-error
  :file-write-error
  :file-seek-error
  :file-read-error
  :bad-magic-number
  :empty-database
  :cant-be-reader
  :cant-be-writer
  :reader-cant-delete
  :reader-cant-store
  :reader-cant-reorganize
  :unknown-update
  :item-not-found
  :reorganize-failed
  :cannot-replace
  :illegal-data
  :opt-already-set
  :opt-illegal)

(defcenum gdbm-store-flag
  :insert :replace)

;;; variables

(defcvar (gdbm-version "gdbm_version") :string)
(defcvar (gdbm-errno "gdbm_errno") gdbm-error-code)

;;; functions

(defcfun (%gdbm-open "gdbm_open") gdbm-file
  (name :string)
  (block-size :int)
  (flags gdbm-flags)
  (mode mode)
  (fatal-func :pointer))

(defcfun (%gdbm-close "gdbm_close") :void
  (dbf gdbm-file))

(defcfun (%gdbm-store "gdbm_store") :int
  (dbf gdbm-file)
  (key gdbm-datum)
  (content gdbm-datum)
  (flag gdbm-store-flag))

(defcfun (%gdbm-fetch "gdbm_fetch") :uint64
  (dbf gdbm-file)
  (key gdbm-datum))

(defcfun (%gdbm-exists "gdbm_exists") :int
  (dbf gdbm-file)
  (key gdbm-datum))

(defcfun (%gdbm-delete "gdbm_delete") :int
  (dbf gdbm-file)
  (key gdbm-datum))

(defcfun (%gdbm-firstkey "gdbm_firstkey") gdbm-datum
  (dbf gdbm-file))

(defcfun (%gdbm-nextkey "gdbm_nextkey") gdbm-datum
  (dbf gdbm-file)
  (key gdbm-datum))

(defcfun (%gdbm-reorganize "gdbm_reorganize") :int
  (dbf gdbm-file))

(defcfun (%gdbm-sync "gdbm_sync") :void
  (dbf gdbm-file))

(defcfun (%gdbm-strerror "gdbm_strerror") :string
  (errno :int))

(defcfun (%gdbm-setopt "gdbm_setopt") :int
  (dbf gdbm-file)
  (option :int)
  (value :pointer)
  (size :int))

(defcfun (%gdbm-fdesc "gdbm_fdesc") :int
  (dbf gdbm-file))

;;; aux

(defcstruct foreign-datum
  (dptr :pointer)
  (dsize :int))

(defun foreign-datum-layout ()
  (with-foreign-object (fdatum 'foreign-datum)
    (setf (foreign-slot-value fdatum 'foreign-datum 'dptr) (make-pointer 1)
          (foreign-slot-value fdatum 'foreign-datum 'dsize) 2)
    (format t "~16,'0x" (mem-ref fdatum :uint64))))