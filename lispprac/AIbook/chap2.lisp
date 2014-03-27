(load "~/prac/tushar-repo/tushar-repo/lispprac/AIbook/chap1.lisp")

(defun random-elt (choices)
  "Choose an element from the list at random."
  (elt choices (random (length choices))))

(defun one-of (set)
  "Pick one element of the set and make a list of it."
  (list (random-elt set)))

(defun Article ()
    (one-of '(the a)))

(defun Noun ()
  (one-of '(man ball woman table)))

(defun Verb ()
  (one-of '(hit took saw liked)))

;;(defun noun-phrase ()
;;  (append (Article) (Noun)))

(defun verb-phrase ()
  (append) (Verb) (noun-phrase))

(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun prep ()
  (one-of '(to in by with on)))

(defun adj ()
  (one-of '(big little blue green adiabatic)))

(defun PP ()
  (append (prep) (noun-phrase)))

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

(defun noun-phrase ()
  (append (Article) (Adj*) (noun) (PP*)))

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for trivial subset of English")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially this is
   *simple-grammar* but we can switch to other grammars")

(defun rule-lhs (rule)
  "The left hand side of rule"
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of rule"
  (rest (rest rule)))
 
(defun rewrites (category)
  "Return a list of possible rewrites for this category"
  (rule-rhs (assoc category *grammar*)))

;; (defun generate (phrase)
;;   "Generate a random sentence or phrase"
;;   (cond ((listp phrase) (mappend #'generate phrase))
;; 	((rewrites phrase) (generate (random-elt (rewrites phrase))))
;; 	(t (list phrase))))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((choices (rewrites phrase)))
	(if (null choices)
	    (list phrase)
	    (generate (random-elt choices))))))

