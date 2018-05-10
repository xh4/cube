(in-package :cube)

(defclass resource () ())

(defun symbolize (name &optional keyword-p)
  (if keyword-p
      (intern (string-upcase (param-case name)) :keyword)
      (intern (string-upcase (param-case name)) :cube)))

(defun decode-object (type object)
  (when object
    (match type
      ("string" object)
      ("boolean" object)
      ("object" object)
      ((cons "integer" format) object)
      ((cons "array" item-type) (mapcar (lambda (item) (decode-object item-type item)) object))
      (ref
       (let* ((class-symbol (symbolize ref))
              (instance (make-instance class-symbol)))
         (unmarshal instance object)
         instanceo)))))

(defgeneric unmarshal (object source))
