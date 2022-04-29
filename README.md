# nibbles-streams

Nibbles-streams is a wrapper around `nibbles` which allows you to create input
and output binary streams with changeable element type and endianness.

~~~~{.lisp}
(flexi-streams:with-output-to-sequence (stream)
  ;; Open little endian nibbles-stream with underlying stream's element-type
  (let ((output (make-instance 'nibbles-streams:nibbles-output-stream
                               :stream stream)))
    (write-byte 2 output)
    (write-sequence #(1 2 3) output)
    ;; Change element-type and endianness
    (setf (nibbles-streams:nibbles-stream-element-type output)
          '(signed-byte 16)
          (nibbles-streams:nibbles-stream-endianness output)
          :big)
    (write-byte 1000 output)
    (write-sequence #(-1 -2001 300) output)))
=> #(2 1 2 3 3 232 255 255 248 47 1 44)
~~~~
