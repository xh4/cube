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
         (unmarshal object instance)
         instance)))))

(defgeneric marshal (stream object &key pretty))

(defgeneric unmarshal (source object))

(defmethod marshal (stream object &key (pretty nil))
  (yason:with-output (stream :indent pretty)
    (yason:encode object stream)))

(defgeneric watch (stream))
