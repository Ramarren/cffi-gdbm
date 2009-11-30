(defpackage #:cffi-gdbm
    (:nicknames #:gdbm)
  (:use #:cl #:cffi #:alexandria #:babel)
  (:export #:*gdbm*
           #:db-open
           #:db-close
           #:with-gdbm))
