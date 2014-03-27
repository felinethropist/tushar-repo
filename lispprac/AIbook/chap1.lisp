;execute this in the repl
;(setf p '(john q public))
;(setf names '((john q public) (malcom x)
;	      (admiral grace murray hopper) (spot)
;	      (aristotle) (a a milne) (z z top)
;	      (sir larry oliver) (miss scarlet)))

(defun last-name (name)
  "Select last name from a name represented as a list"
  (first (last name)))

;(defun first-name (name)
;  "Select first name from a name represented as a list"
;  (first name))

;(mapcar #'last-name names)

(defparameter *titles*
  '(mr mrs miss ms sir madam dr admiral major general)
  "A list of titles that can appear at the start of a name")

(defun first-name (name)
  "Select first name from a name represented as a list"
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(defun self-and-double (x)
  (list x (+ x x)))

(apply #'self-and-double '(3))

(mapcar #'self-and-double '(10 100 300))

(defun mappend (fn the-list)
  "Apply fn to each element of the list and append the result"
  (apply #'append (mapcar fn the-list)))

(mappend #'self-and-double '(10 100 300))

(defun number-and-negation (x)
  "If x is a number, return a list of x and -x"
  (if (numberp x)
      (list x (- x))
      nil))

(defun numbers-and-negations (input)
  "Given a list, return only the numbers and their negations"
  (mappend #'number-and-negation input))