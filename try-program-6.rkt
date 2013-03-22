#lang magnolisp

#|

Compiled procedures can be flagged as 'static' or not, depending on an
'export' annotation. This may turn out to be temporary, as probably we
want to export entire 'library' definitions, and even if implemented
using macros, adding annotations is probably not the most convenient
way to go.

|#

(procedure (#^export p)
           (call p))

(procedure (q)
           (call q))
