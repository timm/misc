;;------------------------------------------------------------------------------
;; ## Macros

;; Lambda short cut
(defmacro -> (args &body body) `(lambda ,args ,@body))

;; Ensure `lst` has a counter for `x`   
;; (so `(incf (has x lst))` can increment).
(defmacro has (x lst) 
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

;; Short cut for slot access within `self`.
(set-macro-character #\$
  (lambda (stream char) `(slot-value self ',(read stream t nil t))))


;; If anything in `nodu` dqw
(defmacro prog+ (&body body)
  #-sbcl `(progn ,@body)
  #+sbcl `(handler-case
            (progn ,@body)
            (error (e)
              (format t "❌ Error: ~A~%" e))))

;;------------------------------------------------------------------------------
;; ## Functions

;; ### Misc

;; Access command line
(defun args () (cdr #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))

;; Get i-th item of string/symbol (e.g. `(chr -1)` is the last item).
(defun chr (s i) (char (string s) (if (minusp i) (+ (length s) i) i)))

(defun chrp (s i c) (char= (chr s i) c))

;; ### Random Numbers

(defvar *seed* 1234567891)

;; Random floats.
(defun rand (&optional (n 1)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

;; Random integers.
(defun rint (&optional (n 100) &aux (base 1E10)) 
  (floor (* n (/ (rand base) base))))

;; ### String 2 Thing

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s))) 
  (let ((x (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (if (or (numberp x) (member x '(t nil ?))) x s1)))

(defun things (s &optional (sep #\,) (here 0)) ; --> list
  (let ((there (position sep s :start here)))
    (cons (thing (subseq s here there))
          (if there (things s sep (1+ there))))))

(defun mapcsv (file &optional (fun #'print) end)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (things (or (read-line s nil) (return end)))))))


;;------------------------------------------------------------------------------
;; ## main

(defun cli (options &aux it)
  (loop :for (key flag help b4) :in options :collect
    (list key flag help (if (setf it (member flag (args) :test #'string=))
                          (cond ((eq b4 t) nil)
                                ((eq b4 nil) t)
                                (t (thing (second it))))
                          b4))))
