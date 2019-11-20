; vim: ts=2 sw=2 sts=2 expandtab:

(defvar *the*
       '(char (     skip #\?
                    less #\>
                    more #\>
                    num #\$
                    klass #\!)
         skip "?"
         some (     max 512 
                    step .5 
                    cohen .3 
                    trivial 1.05)
         seed 1))

(defmacro ? (&rest fs) `(??  getf *the* ,@fs))

(defmacro ?? (how obj f &rest fs)
  (if fs 
      `(?? ,how (,how ,obj ',f) ,@fs) 
      `(,how ,obj ',f)))

(defmacro while (test &body body) `(do () ((not ,test)) ,@body))

(defun word (s lo &optional hi) 
  (string-trim '(#\Space #\Tab #\Newline) (subseq s lo hi)))

(defun words (s &optional (lo 0) (hi (position #\, s :start (1+ lo))))
  (cons (word s lo hi) 
        (if hi (words s (1+ hi)))))

(defmacro with-lines ((line file) &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,file)
       (while (setf ,line (read-line ,str nil))
         (setf ,line (words ,line))
         ,@body))))

(defun has?     (x y) (eql (char x 0) y))
(defun more?    (x)   (has? x (? char more)))
(defun less?    (x)   (has? x (? char less)))
(defun klass?   (x)   (has? x (? char klass)))
(defun num?     (x)   (has? x (? char num)))
(defun goal?    (x)   (or (klass? x) (less? x) (more? x)))
(defun numeric? (x)   (or (num? x)   (less? x) (more? x)))
(defun sym?     (x)   (and (not (num? x)) (not (goal? x))))

(defun adds (&optional inits (i 'Some)) (incs (make-instance i) init 1))
(defun subs (&optional inits (i 'Some)) (incs (make-instance i) init -1))
(defun incs (i lst inc)
   (let ((f (slot-value i 'keys))
         (i (funcall f i)))
     (if (not (ewl 
   
(defun add  (i x) (inc! i x 1))
(defun sub  (i x) (inc! i x -1))

(defstruct num (n 0) (a 0) b)
(defstruct sym n a b)

(defmethod add ((i num) x) 
  (with-slots (n a b) i
    (incf n)
    (incf a x)))

;(print (add (make-num) 22))

(defun l () (load 'xy))
