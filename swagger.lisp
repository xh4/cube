(in-package :cl-user)

(ql:quickload '(:alexandria
                :optima.ppcre
                :cl-json
                :yason
                :cl-change-case
                :drakma
                :quri))

(defpackage cube.swagger
  (:use :cl)
  (:import-from :alexandria
                :hash-table-keys
                :hash-table-values
                :switch)
  (:import-from :optima
                :match)
  (:import-from :optima.ppcre
                :ppcre)
  (:import-from :cl-ppcre
                :regex-replace
                :all-matches-as-strings)
  (:import-from :cl-change-case
                :param-case))

(in-package cube.swagger)

;; Generating Reference Documentation for the Kubernetes API
;; https://kubernetes.io/docs/home/contribute/generated-reference/kubernetes-api/

(defparameter *api-files*
  (list
   #p"/Users/kevin/db.vision/cube/swagger-spec/v1.json"
   #p"/Users/kevin/db.vision/cube/swagger-spec/apps_v1.json"))

(defparameter *resources-output-path*
  #p"/Users/kevin/db.vision/cube/resources.lisp")

(defparameter *operations-output-path*
  #p"/Users/kevin/db.vision/cube/operations.lisp")

(defclass parameter ()
  ((name :initarg :name)
   (type :initarg :type)
   (param-type :initarg :param-type)
   (description :initarg :description)
   (required :initarg :required)
   (allow-multiple :initarg :allow-multiple)))

(defclass response ()
  ((code :initarg :code)
   (message :initarg :message)
   (model :initarg :model)))

(defclass operation ()
  ((type :initarg :type)
   (path :initarg :path)
   (method :initarg :method)
   (summary :initarg :summary)
   (nickname :initarg :nickname)
   (parameters :initarg :parameters)
   (responses :initarg :responses)
   (consumes :initarg :consumes)
   (produces :initarg :produces)))

(defclass api ()
  ((path :initarg :path)
   (description :initarg :description)
   (operations :initarg :operations)))

(defclass property ()
  ((name :initarg :name)
   (type :initarg :type)
   (description :initarg :description)
   (required :initarg :required)))

(defclass model ()
  ((api-version :initarg :api-version)
   (name :initarg :name)
   (description :initarg :description)
   (properties :initarg :properties :initform nil)))

(defclass spec ()
  ((api-version :initarg :api-version)
   (swagger-version :initarg :swagger-version)
   (base-path :initarg :base-path)
   (resource-path :initarg :resource-path)
   (apis :initarg :apis)
   (models :initarg :models)))

(defun make-parameter (table)
  (make-instance 'parameter
                 :name (gethash "name" table)
                 :type (gethash "type" table)
                 :param-type (gethash "paramType" table)
                 :description (gethash "description" table)
                 :required (gethash "required" table)
                 :allow-multiple (gethash "allowMultiple" table)))

(defun make-response (table)
  (make-instance 'response
                 :code (gethash "code" table)
                 :message (gethash "message" table)
                 :model (gethash "responseModel" table)))

(defun make-operation (table path)
  (make-instance 'operation
                 :type (gethash "type" table)
                 :path path
                 :method (gethash "method" table)
                 :summary (gethash "summary" table)
                 :nickname (gethash "nickname" table)
                 :parameters (mapcar #'make-parameter (gethash "parameters" table))
                 :responses (mapcar #'make-response (gethash "responseMessages" table))
                 :consumes (gethash "consumes" table)
                 :produces (gethash "produces" table)))

(defun make-api (table)
  (make-instance 'api
                 :path (gethash "path" table)
                 :description (gethash "description" table)
                 :operations (mapcar (lambda (op)
                                       (make-operation op (gethash "path" table)))
                                     (gethash "operations" table))))

(defun make-property (name table required)
  (let ((type (match (intersection '("type" "$ref") (hash-table-keys table) :test 'equal)
                ('("type")
                 (let ((type (gethash "type" table)))
                   (match type
                     ("string" type)
                     ("boolean" type)
                     ("object" type)
                     ("integer" (cons type (gethash "format" table)))
                     ("array"
                      (let ((a (first (hash-table-keys (gethash "items" table))))
                            (b (first (hash-table-values (gethash "items" table)))))
                        (match a
                          ("type"
                           (cons "array" b))
                          ("$ref"
                           (cons "array" (match b
                                           ((ppcre "(.*)\\.(.*)" version type) type))))))))))
                ('("$ref")
                 (let ((ref (gethash "$ref" table)))
                   (match ref
                     ((ppcre "(.*)\\.(.*)" version type) type)))))))
    (make-instance 'property
                   :name name
                   :type type
                   :description (gethash "description" table)
                   :required required)))

(defun make-model (table api-version)
  (let ((required (gethash "required" table))
        (name (match (gethash "id" table)
                ((ppcre "(.*)\\.(.*)" version name) name))))
    (make-instance 'model
                   :api-version api-version
                   :name name
                   :description (gethash "description" table)
                   :properties (loop for name being the hash-keys of (gethash "properties" table)
                                  using (hash-value property-table)
                                  collect (make-property name property-table (if (find name required :test 'equal) t nil))))))

(defun read-api-file (path)
  (let* ((table (yason:parse path))
         (api-version (gethash "apiVersion" table))
         (apis (gethash "apis" table))
         (models (hash-table-values (gethash "models" table))))
    (make-instance 'spec
                   :api-version (gethash "apiVersion" table)
                   :swagger-version (gethash "swaggerVersion" table)
                   :base-path (gethash "basePath" table)
                   :resource-path (gethash "resourcePath" table)
                   :apis (mapcar #'make-api apis)
                   :models (mapcar (lambda (model) (make-model model api-version)) models))))

(defun read-api-files (paths)
  (mapcar #'read-api-file paths))

(defun symbolize (name &optional keyword?)
  (if keyword?
      (intern (string-upcase (param-case name)) :keyword)
      (intern (string-upcase (param-case name)))))

(defun symbolize-parameter (parameter)
  "If param-type is `body`, use it's type as name"
  (if (equal (slot-value parameter 'param-type) "body")
      (match (slot-value parameter 'type)
        ((ppcre "(.*)\\.(.*)" version name)
         (symbolize name)))
      (symbolize (slot-value parameter 'name))))

(defun symbolize-property (property)
  (symbolize (slot-value property 'name)))

(defun make-check-type-specifier (symbol &optional required)
  (if (eq symbol 'list)
      symbol
      (if required
          symbol
          `(or ,symbol null))))

(defun make-check-type-for-parameter (parameter)
  (let ((required (slot-value parameter 'required)))
    `(check-type ,(symbolize-parameter parameter)
                 ,(switch ((slot-value parameter 'type) :test 'equal)
                    ("string" (make-check-type-specifier 'string required))
                    ("integer" (make-check-type-specifier 'integer required))
                    ("boolean" (make-check-type-specifier 'boolean required))
                    ("array" 'list)
                    (t (make-check-type-specifier
                        (match (slot-value parameter 'type)
                          ((ppcre "(.*)\\.(.*)" version name)
                           (symbolize name)))
                        required))))))

(defun make-check-type-for-property (property)
  (let ((required (slot-value property 'required)))
    (match (slot-value property 'type)
      ("string" (make-check-type-specifier 'string required))
      ("boolean" (make-check-type-specifier 'boolean required))
      ("object" (make-check-type-specifier 'hash-table required))
      ((cons "integer" format) (make-check-type-specifier 'integer required))
      ((cons "array" subtype) 'list)
      (type (make-check-type-specifier (symbolize type) required)))))

(defgeneric generate (object))

(defmethod generate ((property property))
  (let* ((slot-symbol (symbolize (slot-value property 'name)))
         (slot-keyword (symbolize (slot-value property 'name) t)))
    `(,slot-symbol :initarg ,slot-keyword
                   :type ,(make-check-type-for-property property)
                   :documentation ,(slot-value property 'description))))

(defmethod generate ((model model))
  (let* ((name (slot-value model 'name))
         (class-symbol (symbolize name))
         (class-documentation (slot-value model 'description)))
    `((defclass ,class-symbol (resource)
        (,@(when (find "apiVersion" (slot-value model 'properties)
                      :test (lambda (n p) (equal n (slot-value p 'name))))
             `((api-version :initform ,(slot-value model 'api-version) :allocation :class)))
         ,@(when (find "kind" (slot-value model 'properties)
                      :test (lambda (n p) (equal n (slot-value p 'name))))
             `((kind :initform ,(slot-value model 'name) :allocation :class)))
          ,@(loop for property in (slot-value model 'properties)
               when (not (find (slot-value property 'name) '("apiVersion" "kind") :test 'equal))
               collect
                 (generate property)))
        (:documentation ,class-documentation))

      (defmethod marshal (stream (object ,class-symbol))
        )

      (defmethod unmarshal ((source hash-table) (object ,class-symbol))
        ,@(loop for property in (slot-value model 'properties)
             collect
               `(multiple-value-bind (value present-p)
                    (gethash ,(slot-value property 'name) source)
                  (when present-p
                    (setf (slot-value object ',(symbolize (slot-value property 'name)))
                          (decode-object
                           ,(let ((type (slot-value property 'type)))
                              (cond
                                ((stringp type) type)
                                ((consp type) `(cons ,(car type) ,(cdr type)))))
                           value)))))))))

(defun make-argument-list (required-parameters optional-parameters)
  `(,@(loop for p in required-parameters collect (symbolize-parameter p))
      ,@(when optional-parameters
          `(&key ,@(loop for p in optional-parameters
                      collect (symbolize-parameter p))))))

(defun make-parameter-doc (parameter)
  (format nil "[~:[*~;!~]] ~A: ~A"
          (slot-value parameter 'required)
          (symbolize-parameter parameter)
          (if (equal (slot-value parameter 'param-type) "body")
              ;; TODO: Use model description as documentation
              (format nil "A value of type ~A" (match (slot-value parameter 'type)
                                                 ((ppcre "(.*)\\.(.*)" version name)
                                                  (string-upcase (param-case name)))))
              ;; Use parameter description as documentation
              (slot-value parameter 'description))))

(defun make-operation-doc (operation)
  (format nil "~A~:[~;~%~]~{~%    ~A~%~}"
          (slot-value operation 'summary)
          (slot-value operation 'parameters)
          (loop for p in (slot-value operation 'parameters)
             collect (make-parameter-doc p))))

(defun extract-path-arguments (path)
  (let ((placeholders (all-matches-as-strings "{(\\w+)}" path)))
    (loop for ph in placeholders
       collect (cons ph (symbolize (string-trim '(#\{ #\}) ph))))))

(defun make-path-format (path)
  (let ((arguments (extract-path-arguments path)))
    (loop for (placeholder . argument) in arguments
       do (setf path (regex-replace placeholder path "~A")))
    `(format nil ,path ,@(mapcar #'cdr arguments))))

(defmethod generate ((operation operation))
  (let* ((function-symbol (symbolize (slot-value operation 'nickname)))
         (parameters (slot-value operation 'parameters))
         (required-parameters (remove-if-not (lambda (p) (slot-value p 'required)) parameters))
         (optional-parameters (remove-if (lambda (p) (slot-value p 'required)) parameters))
         (argument-list (make-argument-list required-parameters optional-parameters))
         (path (slot-value operation 'path))
         (path-paramters (remove-if-not
                          (lambda (p)
                            (equal (slot-value p 'param-type) "path"))
                          parameters))
         (query-parameters (remove-if-not
                            (lambda (p)
                              (equal (slot-value p 'param-type) "query"))
                            parameters))
         (body-parameter (first (remove-if-not
                                 (lambda (p)
                                   (equal (slot-value p 'param-type) "body"))
                                 parameters))))
    `(defun ,function-symbol ,argument-list
       ,(make-operation-doc operation)
       ,@(loop for param in parameters
            collect (make-check-type-for-parameter param))
       (let* ((scheme *api-endpoint-scheme*)
              (host *api-endpoint-host*)
              (port *api-endpoint-port*)
              (path ,(if path-paramters
                         (make-path-format path)
                         path))
              (query nil))
         ,@(loop for param in query-parameters
              collect `(when ,(symbolize-parameter param)
                         (alexandria:appendf query
                                             (list (cons ,(slot-value param 'name)
                                                         ,(symbolize-parameter param))))))
         (let* ((query-string (quri:url-encode-params query))
                (url (format nil "~A://~A:~D~A~:[~;?~A~]" scheme host port path query query-string)))
           (multiple-value-bind (stream status-code headers)
               (drakma:http-request url
                                    :method ,(intern (slot-value operation 'method) :keyword)
                                    :connection-timeout 5
                                    :read-timeout 5
                                    :want-stream t
                                    ,@(when body-parameter
                                        `(:content (json:encode-json ,(symbolize-parameter body-parameter)))))
             (let* ((response (alexandria::read-stream-content-into-string stream))
                    (object (yason:parse response)))
               (format t "~A~%" response)
               ,(match (slot-value operation 'type)
                 ((ppcre "(.*)\\.(.*)" version type)
                  `(decode-object ,type object))))))))))

(defmethod generate ((api api))
  (loop for operation in (slot-value api 'operations)
       collect (generate operation)))

(defgeneric save (object &optional stream))

(defmethod save ((model model) &optional stream)
  (let ((*print-case* :downcase))
    (loop for form in (generate model)
       do
         (progn
           (pprint form stream)
           (format stream "~%")))))

(defmethod save ((api api) &optional stream)
  (let ((*print-case* :downcase))
    (loop for form in (generate api)
       do
         (progn
           (pprint form stream)
           (format stream "~%")))))

(defun make-one (path resources-output-stream operations-output-stream)
  (let* ((spec (read-api-file path)))
    (loop for model in (slot-value spec 'models)
       do
         (progn
           (save model resources-output-stream)
           (format resources-output-stream "~%")))
    (loop for api in (slot-value spec 'apis)
       do
         (progn
           (save api operations-output-stream)
           (format operations-output-stream "~%")))))

(defun make-all ()
  (with-open-file (resources-output-stream *resources-output-path*
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (with-open-file (operations-output-stream *operations-output-path*
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (loop for stream in (list resources-output-stream operations-output-stream)
         do
           (let ((*print-case* :downcase))
             (pprint '(in-package :cube) stream)
             (format stream "~%~%")))
      (loop for path in *api-files*
         do (make-one path resources-output-stream operations-output-stream)))))
