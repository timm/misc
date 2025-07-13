;; <!-- vim: set lispwords+=has,loop,format ts=2 sw=2 sts=2 et : -->
(defvar *help* "
ezr.lisp: multi-objective explanation
(c)2025, Tim Menzies <timm@ieee.org>, MIT license")

(defvar  *options* '(
  (k     "-k"  "kth value"        2)
  (goal  "-g"  "start-up action"  "one")
  (seed  "-s"  "random number"    1234567891)
  (file  "-f"  "data file"        "../../../moot/optimize/misc/auto93.csv")))

;;------------------------------------------------------------------------------
;; ## Macros

(defmacro ? (x) `(fourth (assoc ',x *options*)))

(defmacro has (x lst) 
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

(set-macro-character #\$
  (lambda (stream char) `(slot-value self ',(read stream t nil t))))

(defmacro prog+ (&body body)
  #-sbcl `(progn ,@body)
  #+sbcl `(handler-case
            (progn ,@body)
            (error (e)
              (format t "❌ Error: ~A~%" e))))

;;-----------------------------------------------------------------------------
;; ## Structures

(defstruct data rows cols)
(defstruct cols x y all klass)
(defstruct sym (n 0) (at 0) (txt "") (has 0))
(defstruct num (n 0) (at 0) (txt "") (mu 0) (m2 0) (sd 0) 
                     (lo 1e32) (hi -1e32) (goal 1))

(defmethod more ((self t) &key) self)

(defmethod more ((self num) &key)
  (if (isChr $txt -1 #\-)  (setf $goal 0))
  self)

(defmethod more ((self data) &key src)
  (labels ((fn (row) (add self row)))
    (if (consp src) (mapcar #'fn src) (mapcsv #'fn src))
    self))

(defmethod more ((self cols) &key)
  (dolist (name $names self)
    (labels ((keep (col)
                   (push col $all)
                   (unless (ischr name -1 #\X) 
                     (if (member (chr name -1) (list #\! #\- #\+)) 
                       (push col $y)
                       (push col $x)))))
      (keep (more (funcall (if (upper-case-p (chr name 0)) #'make-num #'make-sym)
                           :txt name :at (length $all)))))))

;;------------------------------------------------------------------------------
;; ## Update

(defmethod add ((i data) (row cons) &key (inc 1) zap)
  (if $cols
    (push (add $cols row inc) $rows)
    (setf $cols (more (make-cols :names row)))))

;;------------------------------------------------------------------------------
;; ## Functions

;; ### Misc

(defun args () (cdr #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))

(defun chr (s i)   (char (string s) (if (minusp i) (+ (length s) i) i)))

(defun isChr (s i c) (char= (chr s i) c))

;; ### Random Numbers

(defvar *seed* (? seed))

(defun rint (&optional (n 100) &aux (base 1E10)) 
  (floor (* n (/ (rand base) base))))

(defun rand (&optional (n 1)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

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

;;---------------------------------------------------------------------------
;; ## Examples

(defun eg-h (_) 
  (format t "~a~%~%Options:~%" *help*)
  (loop :for (key flag help default) :in *options* :do
    (format t "~26a ~a~%"
      (format nil "   ~a  ~(~a~)=~a" flag key default) help)))

(defun eg--csv (_)
  (mapcsv (? file) #'print))

;;-----------------------------------------------------------------------------
;; ## Main
(defun cli (options &aux it)
  (loop :for (key flag help b4) :in options 
    :collect (list key flag help 
                   (if (setf it (member flag (args) :test #'string=))
                     (cond ((eq b4 t) nil)
                           ((eq b4 nil) t)
                           (t (thing (second it))))
                     b4))))

(when (and *load-pathname* *load-truename*
           (equal (truename *load-pathname*) *load-truename*))
  (setf *options* (cli *options*))
  (loop :for (flag arg) :on (args) :by #'cdr :do
    (let ((com (intern (format nil "EG~:@(~a~)" flag))))
      (when (fboundp com)
        (setf *seed* (? seed))
        (prog+ (funcall com (if arg (thing arg))))))))
