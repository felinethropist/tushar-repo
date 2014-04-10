
;;change this path depending on location on each pc
(load "~/Downloads/practicals-1.0.3/Chapter08/packages.lisp")
(load "~/Downloads/practicals-1.0.3/Chapter08/macro-utilities.lisp")

(defun read-u2 (in)
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    u2))

(defun write-u2 (out value)
  (write-byte (ldb (byte 8 8) value) out)
  (write-byte (ldb (byte 8 0) value) out))

(defconstant +null+ (code-char 0))

(defun read-null-terminated-ascii (in)
  (with-output-to-string (s)
    (loop
       for char = (code-char (read-byte in))
       until (char= char +null+)
       do
	 (write-char char s))))

(defun write-null-terminated-ascii (string out)
  (loop
     for char
     across string
     do
       (write-byte (char-code char) out))
  (write-byte (char-code +null+) out))

(defclass id3-tag ()
  ((identifier
    :initarg :identifier
    :accessor identifier)
   (major-version
    :initarg :major-version
    :accessor major-version)
   (revision
    :initarg :revision
    :accessor revision)
   (flags
    :initarg :flags
    :accessor flags)
   (size
    :initarg :size
    :accessor size)
   (frames
    :initarg :frames
    :accessor frames)))

(defun read-id3-tag (in)
  (let
      ((tag (make-instance 'id3-tag)))
    (with-slots (identifier major-version revision flags size frames) tag
      (setf identifier (read-iso-8859-1-string in :length 3))
      (setf major-version (read-u1 in))
      (setf revision (read-u1 in))
      (setf flags (read-u1 in))
      (setf size (read-id3-encoded-size in))
      (setf frames (read-id3-frames in :tag-size size)))
    tag))

(in-package :cl-user)

(defpackage :com.gigamonkeys.binary-data
  (:use :common-lisp :com.gigamonkeys.macro-utilities)
  (:export :define-binary-class
	   :define-tagged-binary-class
	   :define-binary-type
	   :read-value
	   :write-value
	   :*in-progress-objects*
	   :parent-of-type
	   :current-binary-object
	   :+null+))

(defun as-keyword (sym)
  (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream"))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun mklist (x)
  (if (listp x)
      x
      (list x)))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to a stream"))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defmacro define-binary-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
	 ,(mapcar #'slot->defclass-slot slots))
       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
	 (let ((,objectvar (make-instance ',name)))
	   (with-slots ,(mapcar #'first slots) ,objectvar
	     ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
	   ,objectvar))
       (defmethod write-value ((,typevar (eql ',name)) ,streamvar ,objectvar &key)
	 (with-slots ,(mapcar #'first slots) ,objectvar
	   ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

