(cl:in-package :bodge-heap)

(defconstant +default-expansion-factor+ 2)

;;;
;;; Binary Heap
;;;
(defstruct (binary-heap
            (:constructor %make-binary-heap (key test expansion-factor)))
  (queue (make-array 1 :adjustable t :fill-pointer 0) :type array :read-only t)
  (key #'identity :type function :read-only t)
  (test #'M :type function :read-only t)
  (expansion-factor +default-expansion-factor+ :type number :read-only t))


(defun make-binary-heap (&key (key #'identity)
                           (test #'<)
                           (expansion-factor +default-expansion-factor+))
  (%make-binary-heap key test expansion-factor))


(defun binary-heap-push (heap value)
  (let ((queue (binary-heap-queue heap))
        (key (binary-heap-key heap))
        (test (binary-heap-test heap)))
    (labels ((get-key (idx)
               (funcall key (aref queue idx)))
             (compare (this-idx that-idx)
               (funcall test (get-key this-idx) (get-key that-idx))))
      (prog1 (if (= (fill-pointer queue) (array-total-size queue))
                 (vector-push-extend value queue
                                     (ceiling
                                      (* (array-total-size queue)
                                         (binary-heap-expansion-factor heap))))
                 (vector-push value queue))
        (when (> (length queue) 1)
          (loop with current-idx = (- (length queue) 1)
                for parent-idx = (1- (ceiling (/ current-idx 2)))
                while (compare current-idx parent-idx)
                do (rotatef (aref queue current-idx)
                            (aref queue parent-idx))
                   (setf current-idx parent-idx)
                while (> parent-idx 0)))))))


(defun binary-heap-pop (heap)
  (let ((queue (binary-heap-queue heap))
        (key (binary-heap-key heap))
        (test (binary-heap-test heap)))
    (labels ((get-key (idx)
               (funcall key (aref queue idx)))
             (compare (this-idx that-idx)
               (funcall test (get-key this-idx) (get-key that-idx))))
      (when (> (length queue) 0)
        (prog1 (aref queue 0)
          (setf (aref queue 0) (aref queue (1- (fill-pointer queue))))
          (decf (fill-pointer queue))
          (loop with current-idx = 0
                for left-child-idx = (+ (* current-idx 2) 1)
                for right-child-idx = (1+ left-child-idx)
                for swap-target-idx = current-idx
                when (and (<= left-child-idx (fill-pointer queue))
                          (not (compare swap-target-idx left-child-idx)))
                  do (setf swap-target-idx left-child-idx)
                when (and (<= right-child-idx (fill-pointer queue))
                          (not (compare swap-target-idx right-child-idx)))
                  do (setf swap-target-idx right-child-idx)
                while (/= swap-target-idx current-idx)
                do (rotatef (aref queue current-idx) (aref queue swap-target-idx))
                   (setf current-idx swap-target-idx)))))))


(defun binary-heap-peek (heap)
  (when (> (length (binary-heap-queue heap)) 0)
    (aref (binary-heap-queue heap) 0)))
