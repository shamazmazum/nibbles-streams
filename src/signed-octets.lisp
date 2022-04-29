(in-package :nibbles-streams)

;; Cover '(signed-byte 8) type not covered in nibbles for some reason.

(sera:-> ub8->sb8
         ((unsigned-byte 8))
         (values (signed-byte 8) &optional))
(defun ub8->sb8 (byte)
  (declare (optimize (speed 3))
           (type (unsigned-byte 8) byte))
  #-sbcl (assert (typep byte '(unsigned-byte 8)))
  (if (< byte 128)
      byte
      (logxor #xff (lognot byte))))

(sera:-> sb8->ub8
         ((signed-byte 8))
         (values (unsigned-byte 8) &optional))
(defun sb8->ub8 (byte)
  (declare (optimize (speed 3))
           (type (signed-byte 8) byte))
  #-sbcl (assert (typep byte '(signed-byte 8)))
  (if (< byte 0)
      (1+ (logxor #xff (- byte)))
      byte))

(defun read-sb8 (stream &optional (eof-error-p t) eof-value)
  (ub8->sb8 (read-byte stream eof-error-p eof-value)))

(defun write-sb8 (byte stream)
  (write-byte (sb8->ub8 byte) stream))

(defun read-sb8-sequence (seq stream &key (start 0) end)
  (let* ((try-length (- (or end (length seq)) start))
         (ub-seq (make-array try-length :element-type '(unsigned-byte 8)))
         (bytes-read (read-sequence ub-seq stream)))
    (map-into (make-array bytes-read
                          :element-type (array-element-type seq)
                          :displaced-to seq
                          :displaced-index-offset start)
              #'ub8->sb8
              ub-seq)
    (+ start bytes-read)))

(defun write-sb8-sequence (seq stream &key (start 0) end)
  (let* ((try-length (- (or end (length seq)) start))
         (ub-seq (make-array try-length :element-type '(unsigned-byte 8))))
    (write-sequence
     (map-into ub-seq #'sb8->ub8
               (make-array try-length
                           :element-type (array-element-type seq)
                           :displaced-to seq
                           :displaced-index-offset start))
     stream)))
