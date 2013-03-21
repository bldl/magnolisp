#lang magnolisp

#|

Primitives are called like procedures, but they are
implemented "natively" on top of the execution platform. They can also
be implemented in "user space", but care must then be taken to import
all the required language, as implementation is within Magnolisp.

|#

(require/racket (prefix-in r. racket/base))

(primitive (hello)
           {
            (r.#%app r.displayln "Hello World!")
            }
           {
            "printf(\"Hello World!\");"
            })

(call hello)
