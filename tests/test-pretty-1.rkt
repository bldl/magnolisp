#lang magnolisp

(typedef int (#:annos foreign))
(typedef string (#:annos foreign))
(typedef DataBaseSession (#:annos foreign))

(function (badCond) (#:annos (type (fn predicate)) foreign))

(function (openDataBase name)
  (#:annos (type (fn string DataBaseSession)))
  1)

(function (main anEncoding aMaxNumThreads 
                aDataBaseName aDoFlushToDisk)
  #an(^(fn string int string predicate DataBaseSession) export)
  (do
    (var enc anEncoding)
    (var th aMaxNumThreads)
    (var name aDataBaseName)
    (when (badCond)
      (return (main anEncoding aMaxNumThreads 
                    aDataBaseName aDoFlushToDisk)))
    (return (openDataBase name))))

