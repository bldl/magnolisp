#lang magnolisp

(typedef int #:: (foreign))
(typedef string #:: (foreign))
(typedef DataBaseSession #:: (foreign))

(function (badCond) #:: ((type (-> Bool)) foreign))

(function (openDataBase name)
  #:: ((type (-> string DataBaseSession)))
  1)

(function (main anEncoding aMaxNumThreads 
                aDataBaseName aDoFlushToDisk)
  #:: (^(-> string int string Bool DataBaseSession) export)
  (begin
    (var enc anEncoding)
    (var th aMaxNumThreads)
    (var name aDataBaseName)
    (when (badCond)
      (main anEncoding aMaxNumThreads 
            aDataBaseName aDoFlushToDisk))
    (openDataBase name)))
