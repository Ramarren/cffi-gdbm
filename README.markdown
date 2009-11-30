# cffi-gdbm

This are bindings for `GNU dbm`. It should be obvious what expored symbols do.

## Note

GDBM extensively uses structures as arguments and return values passed by value. This bindings will not work if those are implemented differently than for my system.