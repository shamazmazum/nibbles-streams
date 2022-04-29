(defun do-all()
  (ql:quickload :nibbles-streams/tests)
  (uiop:quit
   (if (uiop:call-function "nibbles-streams-tests:run-tests")
       0 1)))

(do-all)
