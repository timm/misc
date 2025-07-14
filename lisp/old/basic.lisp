;#!/usr/bin/env clisp
;  vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab : 

(defparameter *grammar1*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *grammar2*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> He Her)
    (He   -> obama george)
    (Her  -> michelle laura)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> He Her it these those that)))

(defparameter *grammar3*
  '((Sentence -> (Nounphrase Verbphrase))  
   (Nounphrase -> (Boy))              
   (Nounphrase -> (Girl))           
   (Boy -> john ajit)
   (Girl -> pima barkha)
   (Verbphrase -> (Verb Modlist Adverb with  Nounphrase))
   (Verb -> runs  walks )                 
   (Modlist ->  () (very Modlist))
   (Adverb -> (quickly  slowly))))



(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

;;; ==============================
(let* ((seed0      10013)
       (seed       seed0)
       (multiplier 16807.0d0)
       (modulus    2147483647.0d0))
  (defun reset-seed ()  (setf seed seed0))
  (defun randf      (n) (* n (- 1.0d0 (park-miller-randomizer))))
  (defun randi      (n) (floor (* n (/ (randf 1000.0) 1000))))
  (defun park-miller-randomizer ()
    "cycle= 2,147,483,646 numbers"
    (setf seed (mod (* multiplier seed) modulus))
    (/ seed modulus))
)
;;; ==============================

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

;;; ==============================

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
    Like mapcon, but uses append instead of nconc."
      (apply #'append (mapcar fn list)))

(setf *grammar* *grammar1*)
(print (generate 'sentence))
