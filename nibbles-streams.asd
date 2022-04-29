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
               :serapeum))
