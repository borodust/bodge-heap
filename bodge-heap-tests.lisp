(cl:defpackage :bodge-heap.tests
  (:use :cl :bodge-heap))
(5am:def-suite :bodge-heap.tests)

(cl:in-package :bodge-heap.tests)
(5am:in-suite :bodge-heap.tests)


(defun %make-test-array ()
  (let ((array (make-array 100)))
    (loop for i below (length array)
          do (setf (aref array i) i))
    array))


(5am:test pairing-heap
  (let* ((heap (make-pairing-heap))
         (sorted (%make-test-array))
         (shuffled (alexandria:copy-array sorted))
         control
         result)
    (sort sorted #'<)
    (alexandria:shuffle shuffled)

    (loop for item across shuffled
          do (pairing-heap-push heap item))

    (setf result (loop for item = (pairing-heap-pop heap)
                       while item
                       collect item)
          control (loop for item across sorted
                        collect item))

    (5am:is (equal result control))))


(5am:test binary-heap
  (let* ((heap (make-binary-heap))
         (sorted (%make-test-array))
         (shuffled (alexandria:copy-array sorted))
         control
         result)
    (sort sorted #'<)
    (alexandria:shuffle shuffled)

    (loop for item across shuffled
          do (binary-heap-push heap item))

    (setf result (loop for item = (binary-heap-pop heap)
                       while item
                       collect item)
          control (loop for item across sorted
                        collect item))

    (5am:is (equal result control))))
