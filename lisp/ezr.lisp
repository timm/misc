#!/usr/bin/env sbcl --script
;; <!-- vim: set lispwords+=has,loop,format ts=2 sw=2 sts=2 et : -->

(defvar *help* "
ezr.lisp: multi-objective explanation
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license")

;; Options
(defvar  *options* '(
  (k     "-k"  "kth value"        2)
  (goal  "-g"  "start-up action"  "one")
  (seed  "-s"  "random number"    1234567891)
  (file  "-f"  "data file"        "../../moot/optimize/misc/auto93.csv")))

;; Access options
(defmacro ? (x) `(fourth (assoc ',x *options*)))

;; Structs
(defstruct data rows cols)
(defstruct cols x y all names klass)
(defstruct sym (n 0) (at 0) (txt "") (has 0))
(defstruct num (n 0) (at 0) (txt "") (mu 0)
               (m2 0) (sd 0) (lo 1e32) (hi -1e32) (goal 1))

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

;; Sample from a Gaussian
(defun gauss (m d) 
  (+ m (* d (sqrt (* -2 (log (rand 1.0))) (cos (* 2 pi (rand 1.0)))))))

;; ### String 2 Thing

;; String -> atom
(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s))) 
  (let ((x (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (if (or (numberp x) (member x '(t nil ?))) x s1)))

;; string -> list of atoms (dividing on comma)
(defun things (s &optional (sep #\,) (here 0)) 
  (let ((there (position sep s :start here)))
    (cons (thing (subseq s here there))
          (if there (things s sep (1+ there))))))

;; csv file -> list of list of atom.
(defun mapcsv (file &optional (fun #'print) end)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (things (or (read-line s nil) 
                                   (return end)))))))

;;------------------------------------------------------------------------------
;; ## main

(defun cli (options &aux it)
  (loop :for (key flag help b4) :in options :collect
    (list key flag help (if (setf it (member flag (args) :test #'string=))
                          (cond ((eq b4 t) nil)
                                ((eq b4 nil) t)
                                (t (thing (second it))))
                          b4))))

;;-----------------------------------------------------------------------------
;; ## Initialization

;; Constructor for somwhere to store symbols.
(defun nuSym (&optional inits &key (at 0) (txt " "))
  (adds inits (make-sym :at at :txt txt)))

;; Constructor for somwhere to store num.
(defun nuNum (&optional inits &key (at 0) (txt " "))
  (adds inits (make-num :at at :txt txt
                         :goal (if (chrp txt -1 #\-)  0 1))))

;; Constructor for somwhere to store rows, summarizeed  in cols.
(defun nuData (&optional inits &aux (self (make-data)))
  (if (stringp inits) 
    (mapcsv (lambda (x) (add self x)) inits)
    (mapcar (lambda (x) (add self x)) inits))
  self)

;; Constructor that converts list of strings into NUMs or SYMs.
(defun nuCols (names &aux (self (make-cols :names names)))
  (dolist (txt names self)
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

;; Multiple updates.
(defun adds (lst &optional it)
  (dolist (x lst it)
    (setf it (or it (if (numberp x) (make-num) (make-sym))))
    (add it x)))

;; Subtraction is just adding "-1".
(defmethod sub (self v &key zap) (add self v :zap zap :inc -1))

;; Updating SYMs.
(defmethod add ((self sym) v &key (inc 1)) 
  (when (not (eq v '?))
    (incf (has v $has) inc))
  v)

;; Updating NUMs.
(defmethod add ((self num) v &key (inc 1))
  (when (not (eq v '?))
    (incf $n inc)
    (setf $lo (min v $lo)
          $hi (max v $hi))
    (if (and (< inc 0) (< $n 2))
      (setf $mu 0 $m2 0 $sd 0 $n 0)
      (let* ((d (- v $mu)))
        (setf $mu (+ $mu (* inc (/ d $n)))
              $m2 (+ $m2 (* inc (/ d (- v $mu))))
              $sd (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n))))))))
  v)

;; Updating DATA.
(defmethod add ((self data) (row cons) &key (inc 1) zap)
  (cond ((not $cols)          (return-from add (setf $cols (nuCols row))))
        ((> inc 0)            (push row $rows))
        ((and (<= inc 0) zap) (setf $rows (remove row $rows :test #'equal))))
  (add $cols row :inc inc))

;; Updating COLS.
(defmethod add ((self cols) row &key (inc 1))
  (mapcar (-> (col x) (add col x :inc inc)) $all row)
  row)

;;------------------------------------------------------------------------------
;; ## Query

;; ### Central Tendancy

;; Mean
(defmethod mid ((self num)) $mu)

;; Median
(defmethod mid ((self sym))
  (car (reduce (-> (a b) (if (> (cdr a) (cdr b)) a b)) $has)))

;; ### Variation away from central tendancy (a.k.a. diversity).

;; Standard deviation
(defmethod div ((self num)) $sd)

;; Entropy
(defmethod div ((self sym))
  (- (loop :for (_ . v) :in $has :sum  (* (/ v $n) (log (/ v $n) 2)))))

;;---------------------------------------------------------------------------
;; ## Examples

(defun eg-h (_) 
  (format t "~a~%~%Options:~%" *help*)
  (loop :for (key flag help default) :in *options* :do
    (format t "~26a ~a~%"
      (format nil "   ~a  ~(~a~)=~a" flag key default) help)))

(defun eg--csv (_) (mapcsv (? file) #'print))
(defun eg--the (_) (print *options*))
(defun eg--sym (_)
  (let ((sym (adds '(a a a a b b c))))
    (print sym)))

;;-----------------------------------------------------------------------------
;; ## Main

;; Update *options* from command-line.
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
