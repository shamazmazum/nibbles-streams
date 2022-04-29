(defsystem :nibbles-streams
  :name :nibbles-streams
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Proof of concept for lossless audio compressor"
  :licence "2-clause BSD"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "signed-octets")
               (:file "nibbles-streams"))
  :depends-on (:trivial-gray-streams
               :nibbles
               :serapeum)
  :in-order-to ((test-op (load-op "nibbles-streams/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (symbol-function
                      (intern (symbol-name '#:run-tests)
                              (find-package :nibbles-streams-tests))))))

(defsystem :nibbles-streams/tests
  :name :nibbles-streams/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:nibbles-streams
               :fiveam
               :flexi-streams))
