(in-package :cffi-gdbm)

;;; library

(define-foreign-library gdbm
  (:unix "libgdbm.so"))

(load-foreign-library 'gdbm)

;;; types

(defcstruct foreign-datum
  (dptr :pointer)
  (dsize :int))

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
(defcvar (gdbm-errno-raw "gdbm_errno") :int)

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
  (key-dptr :pointer)
  (key-dsize :int)
  (content-dptr :pointer)
  (content-dsize :int)
  (flag gdbm-store-flag))

(defcfun (%gdbm-fetch "gdbm_fetch") :void
  (retval :pointer)
  (dbf gdbm-file)
  (key-dptr :pointer)
  (key-dsize :int))

(defcfun (%gdbm-exists "gdbm_exists") :int
  (dbf gdbm-file)
  (key-dptr :pointer)
  (key-dsize :int))

(defcfun (%gdbm-delete "gdbm_delete") :int
  (dbf gdbm-file)
  (key-dptr :pointer)
  (key-dsize :int))

(defcfun (%gdbm-firstkey "gdbm_firstkey") :void
  (retval :pointer)
  (dbf gdbm-file))

(defcfun (%gdbm-nextkey "gdbm_nextkey") :void
  (retval :pointer)
  (dbf gdbm-file)
  (key-dptr :pointer)
  (key-dsize :int))

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

(defun foreign-datum-layout ()
  (with-foreign-object (fdatum 'foreign-datum)
    (setf (foreign-slot-value fdatum 'foreign-datum 'dptr) (make-pointer 1)
          (foreign-slot-value fdatum 'foreign-datum 'dsize) 2)
    (format t "~16,'0x" (mem-ref fdatum :uint64))))