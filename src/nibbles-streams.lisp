(in-package :nibbles-streams)

(defclass nibbles-stream (tgs:fundamental-binary-stream)
  ((element-type :initarg       :element-type
                 :accessor      nibbles-stream-element-type
                 :documentation "Element type of nibble-stream. Setfable.")
   (endianness   :initarg       :endianness
                 :initform      :little
                 :accessor      nibbles-stream-endianness
                 :type          (member :big :little)
                 :documentation "Endianness of the stream. Can be :LITTLE or :BIG")
   (stream       :initarg       :stream
                 :initform      (error "Specify underlying stream")
                 :reader        nibbles-stream-stream
                 :type          stream
                 :documentation "Underlying stream. Must be specified at creation time."))
  (:documentation "Generic nibbles stream. Not to be instantiated."))

(defclass nibbles-output-stream (nibbles-stream tgs:fundamental-binary-output-stream)
  ((byte-writer     :type          function
                    :accessor      nibbles-stream-byte-writer
                    :documentation "Function which is called to write a byte to the stream")
   (sequence-writer :type          function
                    :accessor      nibbles-stream-sequence-writer
                    :documentation
                    "Function which is called to write a sequence of bytes to the stream"))
  (:documentation "Output nibbles stream."))

(defclass nibbles-input-stream (nibbles-stream
                                tgs:fundamental-binary-input-stream)
  ((byte-reader     :type          function
                    :accessor      nibbles-stream-byte-reader
                    :documentation "Function which is called to read a byte from the stream")
   (sequence-reader :type          function
                    :accessor      nibbles-stream-sequence-reader
                    :documentation
                    "Function which is called to read a sequence of bytes from the stream"))
  (:documentation "Input nibbles stream."))

(define-condition nibbles-stream-error (simple-error)
  ()
  (:documentation "Condition class realted to nibbles-stream."))

(defmethod initialize-instance :after ((nibbles-stream nibbles-stream)
                                       &key element-type &allow-other-keys)
  (setf (nibbles-stream-element-type nibbles-stream)
        (or element-type (stream-element-type
                          (nibbles-stream-stream nibbles-stream))))
  (update-functions nibbles-stream))

(defgeneric update-functions (stream)
  (:documentation "Update reader or writer functions after a change of element-type or
endiannes."))

(sera:defconstructor db-entry
  (endianness      (member :big :little))
  (element-type    t) ; Anything better? :)
  (byte-reader     function)
  (sequence-reader function)
  (byte-writer     function)
  (sequence-writer function))

(defun nibbles-functions ()
  (let (functions)
    (flet ((nibbles-fn (name)
             (symbol-function
              (intern name (find-package :nibbles)))))
      (loop for signedness in '(unsigned-byte signed-byte) do
           (loop for size in '(16 24 32 64) do
                (loop for endianness in '(:big :little) do
                     (let ((type-spec
                            (format nil "~a~d/~a"
                                    (ecase signedness
                                      (unsigned-byte "UB")
                                      (signed-byte   "SB"))
                                    size
                                    (ecase endianness
                                      (:little "LE")
                                      (:big    "BE")))))
                       (push (db-entry endianness (list signedness size)
                                       (nibbles-fn (format nil "READ-~a" type-spec))
                                       ;; KLUDGE: READ-X-INTO-SEQUENCE returns the
                                       ;; sequence itself rather than the first unmodified
                                       ;; position so we need to create a small wrapper
                                       (flet ((read-sequence-wrapper (seq stream &key
                                                                          (start 0) end)
                                                (funcall
                                                 (nibbles-fn
                                                  (format nil "READ-~a-INTO-SEQUENCE"
                                                          type-spec))
                                                 seq stream :start start :end end)
                                                (or end (length seq))))
                                         #'read-sequence-wrapper)
                                       (nibbles-fn (format nil "WRITE-~a" type-spec))
                                       (nibbles-fn (format nil "WRITE-~a-SEQUENCE"
                                                           type-spec)))
                             functions))))))
    functions))

(defparameter *function-db*
  (append (nibbles-functions)
          ;; octet readers/writers are not covered by nibbles
          (list
           (db-entry :little '(unsigned-byte 8)
                     #'read-byte  #'read-sequence
                     #'write-byte #'write-sequence)
           (db-entry :big    '(unsigned-byte 8)
                     #'read-byte  #'read-sequence
                     #'write-byte #'write-sequence)
           (db-entry :little '(signed-byte 8)
                     #'read-sb8  #'read-sb8-sequence
                     #'write-sb8 #'write-sb8-sequence)
           (db-entry :big    '(signed-byte 8)
                     #'read-sb8  #'read-sb8-sequence
                     #'write-sb8 #'write-sb8-sequence))))

;; Hacks with types
(defun typesequalp (t1 t2)
  (and (subtypep t1 t2)
       (subtypep t2 t1)))

(defun find-entry (endianness type)
  (find-if
   (lambda (entry)
     (and (typesequalp (db-entry-element-type entry) type)
          (eq          (db-entry-endianness   entry) endianness)))
   *function-db*))

(defmethod update-functions ((stream nibbles-output-stream))
  (let ((entry (find-entry
                (nibbles-stream-endianness stream)
                (stream-element-type       stream))))
    (setf (nibbles-stream-byte-writer     stream)
          (db-entry-byte-writer     entry)
          (nibbles-stream-sequence-writer stream)
          (db-entry-sequence-writer entry))))

(defmethod update-functions ((stream nibbles-input-stream))
  (let ((entry (find-entry
                (nibbles-stream-endianness stream)
                (stream-element-type       stream))))
    (setf (nibbles-stream-byte-reader     stream)
          (db-entry-byte-reader     entry)
          (nibbles-stream-sequence-reader stream)
          (db-entry-sequence-reader entry))))

(defmethod tgs:stream-force-output ((stream nibbles-output-stream))
  (force-output (nibbles-stream-stream stream)))

(defmethod tgs:stream-finish-output ((stream nibbles-output-stream))
  (finish-output (nibbles-stream-stream stream)))

(defmethod tgs:stream-clear-output ((stream nibbles-output-stream))
  (clear-output (nibbles-stream-stream stream)))

(defmethod tgs:stream-clear-input ((stream nibbles-input-stream))
  (clear-input (nibbles-stream-stream stream)))

(defmethod stream-element-type ((stream nibbles-stream))
  (nibbles-stream-element-type stream))

(defmethod (setf nibbles-stream-element-type) :before (type (stream nibbles-stream))
  (unless (find-entry (nibbles-stream-endianness stream) type)
    (error 'nibbles-stream-error
           :format-control "Unsupported endianness-type pair: ~a + ~a"
           :format-arguments (list (nibbles-stream-endianness stream) type))))

(defmethod (setf nibbles-stream-element-type) :after (type (stream nibbles-stream))
  (declare (ignore type))
  (update-functions stream))

(defmethod (setf nibbles-stream-endianness) :before (endianness (stream nibbles-stream))
  (declare (type (member :little :big) endianness))
  (unless (find-entry endianness (stream-element-type stream))
    (error 'nibbles-stream-error
           :format-control "Unsupported endianness-type pair: ~a + ~a"
           :format-arguments (list endianness (stream-element-type stream)))))

(defmethod (setf nibbles-stream-endianness) :after (endianness (stream nibbles-stream))
  (declare (ignore endianness))
  (update-functions stream))

(defmethod tgs:stream-read-byte ((stream nibbles-input-stream))
  (funcall (nibbles-stream-byte-reader stream)
           (nibbles-stream-stream stream)))

(defmethod tgs:stream-write-byte ((stream nibbles-output-stream) byte)
  (funcall (nibbles-stream-byte-writer stream)
           byte (nibbles-stream-stream stream)))

(defmethod tgs:stream-read-sequence ((stream nibbles-input-stream)
                                     sequence start end &key &allow-other-keys)
  (funcall (nibbles-stream-sequence-reader stream)
           sequence (nibbles-stream-stream stream)
           :start (or start 0) :end end))

(defmethod tgs:stream-write-sequence ((stream nibbles-input-stream)
                                      sequence start end &key &allow-other-keys)
  (funcall (nibbles-stream-sequence-writer stream)
           sequence (nibbles-stream-stream stream)
           :start (or start 0) :end end))
