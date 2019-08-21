(asdf:defsystem :bodge-heap
  :description "Heap implementation"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :serial t
  :components ((:file "packages")
               (:file "pairing-heap")
               (:file "binary-heap")))


(asdf:defsystem :bodge-heap/tests
  :description "Heap tests"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:bodge-heap :fiveam :alexandria)
  :serial t
  :components ((:file "bodge-heap-tests")))
