(defpackage nibbles-streams
  (:use    #:cl)
  (:local-nicknames (:tgs  :trivial-gray-streams)
                    (:sera :serapeum))
  (:export
   ;; Classes
   #:nibbles-stream
   #:nibbles-output-stream
   #:nibbles-input-stream

   ;; Accessors
   #:nibbles-stream-element-type
   #:nibbles-stream-endianness

   ;; Conditions
   #:nibbles-stream-error))
