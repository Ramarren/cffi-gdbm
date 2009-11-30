# cffi-gdbm

This are bindings for `GNU dbm`. It should be obvious what exported symbols do. This is only very lightly tested.

## Note

GDBM extensively uses structures as arguments and return values passed by value. This bindings will not work if the C compiler decides to implement them differently than on my system.