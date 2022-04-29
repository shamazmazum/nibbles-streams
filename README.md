# nibbles-streams
[![CI](https://github.com/shamazmazum/nibbles-streams/actions/workflows/tests.yml/badge.svg)](https://github.com/shamazmazum/nibbles-streams/actions/workflows/tests.yml)

Nibbles-streams is a wrapper around `nibbles` which allows you to create input
and output binary streams with changeable element type and endianness.

An example:
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

The package `nibble-streams` export two types of streams: `nibbles-input-stream`
and `nibbles-output-stream`, accessors `nibbles-stream-element-type` and
`nibbles-stream-endianness` and a condition `nibbles-stream-error` which is
signalled when a wrong element type or endianness is specified.

## Supported element types
These element types are supported:

* `(unsigned-byte 8)`
* `(unsigned-byte 16)`
* `(unsigned-byte 32)`
* `(unsigned-byte 64)`
* `(signed-byte 8)`
* `(signed-byte 16)`
* `(signed-byte 32)`
* `(signed-byte 64)`
