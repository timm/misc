;; <!-- vim: set lispwords+=has,loop,format ts=2 sw=2 sts=2 et : -->
(load "lib")

(defvar *help* "
ezr.lisp: multi-objective explanation
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license")

;; Options
(defvar  *options* '(
  (k     "-k"  "kth value"        2)
  (goal  "-g"  "start-up action"  "one")
  (seed  "-s"  "random number"    1234567891)
  (file  "-f"  "data file"        "../../../moot/optimize/misc/auto93.csv")))

;; Access options
(defmacro ? (x) `(fourth (assoc ',x *options*)))

;; Structs
(defstruct data rows cols)
(defstruct cols x y all names klass)
(defstruct sym (n 0) (at 0) (txt "") (has 0))
(defstruct num (n 0) (at 0) (txt "") (mu 0)
               (m2 0) (sd 0) (lo 1e32) (hi -1e32) (goal 1))

;;-----------------------------------------------------------------------------
;; ## Initialization

(defun nuSym (&optional inits &key (at 0) (txt " "))
  (adds inits (make-sym :at at :txt txt)))

(defun nuNum (&optional inits &key (at 0) (txt " "))
  (adds inits (make-num :at at :txt txt
                         :goal (if (chrp txt -1 #\-)  0 1))))

(defmethod nuData (&optional inits &aux (self (make-data)))
  (if (stringp src) 
    (mapcsv (lambda (x) (add self x)) src)
    (mapcar (lambda (x) (add self x)) src))
  self)

(defmethod nuCol (name &aux (self (make-cols :names row)))
  (dolist (txt $names self)
    (let* ((a   (chr txt 0))
           (z   (chr txt -1))
           (new (if (upper-case-p a) #'nuNum #'nuSym))
           (col (funcall new :txt txt :at (length $all))))
      (push col $all)
      (unless (eql z  #\X)
        (if (eql z #\!) (setf $klass col))
        (if (member z '(#\! #\- #\+)) 
          (push col $y)
          (push col $x))))))

;;------------------------------------------------------------------------------
;; ## Update

(defun adds (lst &optional it)
  (dolist (x lst it)
    (setf it (or it (if (numberp x) (make-num) (make-sym))))
    (add it x)))

(defmethod sub (self v &key zap) (add self v :zap zap :inc -1))

(defmethod add ((self data) (row cons) &key (inc 1) zap)
  (if $cols
    (push (add $cols row inc) $rows)
    (setf $cols (more (make-cols :names row))))
  row)

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
  (loop :for (key flag help b4) :in options :collect
    (list key flag help (if (setf it (member flag (args) :test #'string=))
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
