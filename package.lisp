(defpackage #:cffi-gdbm
    (:nicknames #:gdbm)
  (:use #:cl #:cffi #:alexandria #:babel)
  (:export #:*gdbm*
           #:db-open
           #:db-close
           #:with-gdbm
           #:store
           #:store
           #:exists
           #:fetch
           #:db-delete
           #:make-key-iterator
           #:sync
           #:reorganize
           #:get-error-string
           #:vector-decoder
           #:*u8-decoder*))
