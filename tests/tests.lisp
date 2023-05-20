(in-package :nibbles-streams-tests)

(def-suite streams :description "Test nibbles streams")

(defun run-tests ()
  (let ((status (run 'streams)))
    (explain! status)
    (results-status status)))

(defparameter *test-sequence*
  #(10 1 2 3 4 12 244 1 255 127 128                   ; octets
    #xda #x04 #xa2 #xf7 0 0 1 0 #x18 #xfc #xd2 #x04   ; (sb 16) little
    #x04 #xda #xf7 #xa2 0 0 0 1 #xfc #x18 #x04 #xd2   ; (sb 16) big
    #x40 #x9c #x63 #x04 0 0 1 0 #xe8 #x80 #x25 #xd8   ; (ub 16) little
    #x9c #x40 #x04 #x63 0 0 0 1 #x80 #xe8 #xd8 #x25)) ; (ub 16) big

(defun read-into-sequence (stream n)
  (let ((sequence (make-array n :initial-element 0)))
    (read-sequence sequence stream)
    sequence))

(in-suite streams)
(test output-streams
  (let ((sequence
         (flexi:with-output-to-sequence (output)
           (let ((stream (make-instance 'ns:nibbles-output-stream
                                        :stream output)))
             ;; Start with unsigned octets
             (write-byte 10 stream)
             (write-sequence #(1 2 3 4) stream)
             ;; Change to signed
             (setf (ns:nibbles-stream-element-type stream)
                   '(signed-byte 8))
             (write-byte  12 stream)
             (write-byte -12 stream)
             (write-sequence #(1 -1 127 -128) stream)
             ;; (SIGNED-BYTE 16)
             (setf (ns:nibbles-stream-element-type stream)
                   '(signed-byte 16)
                   (ns:nibbles-stream-endianness stream)
                   :little)
             (write-byte  1242 stream)
             (write-byte -2142 stream)
             (write-sequence #(0 1 -1000 1234) stream)
             (setf (ns:nibbles-stream-element-type stream)
                   '(signed-byte 16)
                   (ns:nibbles-stream-endianness stream)
                   :big)
             (write-byte  1242 stream)
             (write-byte -2142 stream)
             (write-sequence #(0 1 -1000 1234) stream)
             ;; (UNSIGNED-BYTE 16)
             (setf (ns:nibbles-stream-element-type stream)
                   '(unsigned-byte 16)
                   (ns:nibbles-stream-endianness stream)
                   :little)
             (write-byte 40000 stream)
             (write-byte 1123 stream)
             (write-sequence #(0 1 33000 55333) stream)
             (setf (ns:nibbles-stream-element-type stream)
                   '(unsigned-byte 16)
                   (ns:nibbles-stream-endianness stream)
                   :big)
             (write-byte 40000 stream)
             (write-byte 1123 stream)
             (write-sequence #(0 1 33000 55333) stream)
             ;; Others?
             ))))
    (is (equalp sequence *test-sequence*))))

(test input-streams
  (flexi:with-input-from-sequence (input *test-sequence*)
    (let ((stream (make-instance 'ns:nibbles-input-stream
                                 :stream input)))
      ;; Start with unsigned octets
      (is (= (read-byte stream) 10))
      (is (equalp (read-into-sequence stream 4)
                  #(1 2 3 4)))
      ;; Change to signed
      (setf (ns:nibbles-stream-element-type stream)
            '(signed-byte 8))
      (is (= (read-byte stream)  12))
      (is (= (read-byte stream) -12))
      (is (equalp (read-into-sequence stream 4)
                  #(1 -1 127 -128)))
      ;; (SIGNED-BYTE 16)
      (setf (ns:nibbles-stream-element-type stream)
            '(signed-byte 16)
            (ns:nibbles-stream-endianness stream)
            :little)
      (is (= (read-byte stream)  1242))
      (is (= (read-byte stream) -2142))
      (is (equalp (read-into-sequence stream 4)
                  #(0 1 -1000 1234)))
      (setf (ns:nibbles-stream-element-type stream)
            '(signed-byte 16)
            (ns:nibbles-stream-endianness stream)
            :big)
      (is (= (read-byte stream)  1242))
      (is (= (read-byte stream) -2142))
      (is (equalp (read-into-sequence stream 4)
                  #(0 1 -1000 1234)))
      ;; (UNSIGNED-BYTE 16)
      (setf (ns:nibbles-stream-element-type stream)
            '(unsigned-byte 16)
            (ns:nibbles-stream-endianness stream)
            :little)
      (is (= (read-byte stream) 40000))
      (is (= (read-byte stream) 1123))
      (is (equalp (read-into-sequence stream 4)
                  #(0 1 33000 55333)))
      (setf (ns:nibbles-stream-element-type stream)
            '(unsigned-byte 16)
            (ns:nibbles-stream-endianness stream)
            :big)
            (is (= (read-byte stream) 40000))
      (is (= (read-byte stream) 1123))
      (is (equalp (read-into-sequence stream 4)
                  #(0 1 33000 55333)))
      ;; Others?
      )))

#+sbcl
(test element-type-checks
  (flexi:with-output-to-sequence (output)
    (let ((stream (make-instance 'ns:nibbles-output-stream
                                 :stream output
                                 :element-type '(unsigned-byte 16))))
      (finishes (write-byte 60000 stream))
      (signals type-error (write-byte 100000 stream))
      (finishes (write-sequence #(1 2 3) stream))
      (signals type-error (write-sequence #(10000 100000 1000000) stream))))
  (flexi:with-input-from-sequence (input #(0 1 0 0 0 13))
    (let ((stream (make-instance 'ns:nibbles-input-stream
                                 :stream input
                                 :element-type '(unsigned-byte 16))))
      (signals type-error (read-sequence (make-array 3 :element-type 'bit)
                                         stream)))))

(test start-end-arguments
  (flexi:with-input-from-sequence (input #(1 2 3 4 5))
    (let ((stream (make-instance 'ns:nibbles-input-stream
                                 :stream input))
          (vector (vector 10 11 12)))
      (is (= (read-sequence vector stream :start 1 :end 2) 2))
      (is (equalp vector #(10 1 12)))
      (setf (ns:nibbles-stream-element-type stream)
            '(unsigned-byte 16))
      (is (= (read-sequence vector stream :start 2) 3))
      (is (equalp vector #(10 1 770)))
      (is (= (read-byte stream) 1284))))
  (equalp
   (flexi:with-output-to-sequence (output)
     (let ((stream (make-instance 'ns:nibbles-output-stream
                                  :stream output)))
       (write-sequence #(1 2 3 4) stream :start 2)
       (setf (ns:nibbles-stream-element-type stream)
             '(unsigned-byte 16))
       (write-sequence #(10 9 8 7) stream :start 2 :end 3)))
   #(3 4 8 0)))
