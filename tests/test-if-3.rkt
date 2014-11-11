#lang magnolisp

(typedef int (#:annos foreign))

(function (holds? x)
  (#:annos (type (fn int bool)) foreign)
  #f)

(function (f x)
  (#:annos export (type (fn int int)))
  (do
    (when (holds? x)
      (return 1))
    (unless (holds? x)
      (return 2))
    (when (holds? x)
      (void)
      (return 3))
    (unless (holds? x)
      (void)
      (return 4))
    (return 5)))

(f 5)
