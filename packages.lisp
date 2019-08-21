(cl:defpackage :bodge-heap
  (:use :cl)
  (:export #:make-binary-heap
           #:binary-heap-push
           #:binary-heap-pop
           #:binary-heap-peek

           #:make-pairing-heap
           #:pairing-heap-push
           #:pairing-heap-pop
           #:pairing-heap-peek))
