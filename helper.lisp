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
  (json:encode-json object stream))

(defgeneric watch (stream))

(defun check-config ()
  (let ((should-try-load t))
    (tagbody
     check
       (handler-case
           (progn
             (check-type *api-endpoint-host* string)
             (check-type *api-endpoint-port* integer)
             (check-type *cluster-certificate-authority* (or pathname string))
             (check-type *client-certificate* (or pathname string))
             (check-type *client-key* (or pathname string))
             (go finally))
         (error (e)
           (if should-try-load
               (go load)
               (error e))))
     load
       (load-default-config)
       (setf should-try-load nil)
       (go check)
     finally
       (return-from check-config
         (list *api-endpoint-host*
               *api-endpoint-port*
               (namestring *cluster-certificate-authority*)
               (namestring *client-certificate*)
               (namestring *client-key*))))))
