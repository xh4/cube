(in-package :cl-user)

(ql:quickload '(:alexandria
                :optima.ppcre
                :yason
                :cl-change-case
                :drakma
                :quri
                :defclass-std
                :closer-mop))

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
                :param-case
                :camel-case)
  (:import-from :defclass-std
                :defclass/std))

(in-package cube.swagger)

;; Generating Reference Documentation for the Kubernetes API
;; https://kubernetes.io/docs/home/contribute/generated-reference/kubernetes-api/

(defparameter *api-files*
  (list
   (merge-pathnames "swagger-spec/v1.json" (asdf:component-pathname (asdf:find-system "cube")))
   (merge-pathnames "swagger-spec/apps_v1.json" (asdf:component-pathname (asdf:find-system "cube")))))

(defparameter *resources-output-path*
  (merge-pathnames "resources.lisp" (asdf:component-pathname (asdf:find-system "cube"))))

(defparameter *operations-output-path*
  (merge-pathnames "operations.lisp" (asdf:component-pathname (asdf:find-system "cube"))))

(setf defclass-std:*with-prefix* t)

(defclass/std parameter ()
  ((name type param-type description required allow-multiple)))

(defclass/std response ()
  ((code message model)))

(defclass/std operation ()
  ((type path method summary nickname parameters responses consumes produces)))

(defclass/std api ()
  ((path description operations)))

(defclass/std property ()
  ((name type description required)))

(defclass/std model ()
  ((api-version name description properties)))

(defclass/std spec ()
  ((api-version swagger-version base-path resource-path apis models)))

(defun make-instance-from-hash-table (class table &optional mapping)
  (check-type class symbol)
  (check-type table hash-table)
  (let ((instance (make-instance class)))
    (let ((slots (closer-mop:class-slots (find-class class))))
      (loop for slot in slots
         for name = (closer-mop:slot-definition-name slot)
         for key = (or (cdr (assoc name mapping))
                       (camel-case (string name)))
         do
           (setf (slot-value instance name)
                 (gethash key table))))
    instance))

(defun make-parameter (table)
  (make-instance-from-hash-table 'parameter table))

(defun make-response (table)
  (make-instance-from-hash-table 'response table '((model . "responseModel"))))

(defun make-operation (table path)
  (let ((operation (make-instance-from-hash-table 'operation table
                                                  '((parameters . "_")
                                                    (responses . "_")))))
    (setf (operation-path operation) path)
    (setf (operation-parameters operation)
          (mapcar #'make-parameter (gethash "parameters" table)))
    (setf (operation-responses operation)
          (mapcar #'make-response (gethash "responses" table)))
    operation))

(defun make-api (table)
  (let ((api (make-instance-from-hash-table 'api table '((operations . "_")))))
    (setf (api-operations api)
          (mapcar (lambda (op)
                    (make-operation op (gethash "path" table)))
                  (gethash "operations" table)))
    api))

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
  (if (equal (parameter-param-type parameter) "body")
      (match (parameter-type parameter)
        ((ppcre "(.*)\\.(.*)" version name)
         (symbolize name)))
      (symbolize (parameter-name parameter))))

(defun symbolize-property (property)
  (symbolize (property-name property)))

(defun make-check-type-specifier (symbol &optional required)
  (if (eq symbol 'list)
      symbol
      (if required
          symbol
          `(or ,symbol null))))

(defun make-check-type-for-parameter (parameter)
  (let ((required (parameter-required parameter)))
    `(check-type ,(symbolize-parameter parameter)
                 ,(switch ((parameter-type parameter) :test 'equal)
                    ("string" (make-check-type-specifier 'string required))
                    ("integer" (make-check-type-specifier 'integer required))
                    ("boolean" (make-check-type-specifier 'boolean required))
                    ("array" 'list)
                    (t (make-check-type-specifier
                        (match (parameter-type parameter)
                          ((ppcre "(.*)\\.(.*)" version name)
                           (symbolize name)))
                        required))))))

(defun make-check-type-for-property (property)
  (let ((required (property-required property)))
    (match (property-type property)
      ("string" (make-check-type-specifier 'string required))
      ("boolean" (make-check-type-specifier 'boolean required))
      ("object" (make-check-type-specifier 'hash-table required))
      ((cons "integer" format) (make-check-type-specifier 'integer required))
      ((cons "array" subtype) 'list)
      (type (make-check-type-specifier (symbolize type) required)))))

(defgeneric generate (object))

(defmethod generate ((property property))
  (let* ((slot-symbol (symbolize (property-name property)))
         (slot-keyword (symbolize (property-name property) t)))
    `(,slot-symbol :initarg ,slot-keyword
                   :type ,(make-check-type-for-property property)
                   :documentation ,(property-description property))))

(defmethod generate ((model model))
  (let* ((name (model-name model))
         (class-symbol (symbolize name))
         (class-documentation (model-description model)))
    `((defclass ,class-symbol (resource)
        (,@(when (find "apiVersion" (model-properties model)
                      :test (lambda (n p) (equal n (property-name p))))
             `((api-version :initform ,(model-api-version model) :allocation :class)))
         ,@(when (find "kind" (model-properties model)
                      :test (lambda (n p) (equal n (property-name p))))
             `((kind :initform ,(model-name model) :allocation :class)))
           ,@(loop for property in (model-properties model)
               when (not (find (property-name property) '("apiVersion" "kind") :test 'equal))
               collect
                 (generate property)))
        ,@(when class-documentation
            `((:documentation ,class-documentation))))

      (defmethod yason:encode ((,class-symbol ,class-symbol) &optional (stream *standard-output*))
        (yason:with-object ()
          ,@(loop for property in (model-properties model)
               collect
                 `(when (slot-boundp ,class-symbol ',(symbolize (property-name property)))
                    (yason:encode-object-element ,(property-name property)
                                                 (slot-value ,class-symbol ',(symbolize (property-name property))))))))

      (defmethod unmarshal ((source hash-table) (object ,class-symbol))
        ,@(loop for property in (model-properties model)
             collect
               `(multiple-value-bind (value present-p)
                    (gethash ,(property-name property) source)
                  (when present-p
                    (setf (slot-value object ',(symbolize (property-name property)))
                          (decode-object
                           ,(let ((type (property-type property)))
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
          (parameter-required parameter)
          (symbolize-parameter parameter)
          (if (equal (parameter-param-type parameter) "body")
              ;; TODO: Use model description as documentation
              (format nil "A value of type ~A" (match (parameter-type parameter)
                                                 ((ppcre "(.*)\\.(.*)" version name)
                                                  (string-upcase (param-case name)))))
              ;; Use parameter description as documentation
              (parameter-description parameter))))

(defun make-operation-doc (operation)
  (format nil "~A~{~%~%    ~A~}"
          (operation-summary operation)
          (loop for p in (operation-parameters operation)
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

(defun parameter-conflict-p (parameter1 parameter2)
  (equal (parameter-name parameter1)
         (parameter-name parameter2)))

(defun find-duplicated-parameters (parameters)
  (loop for p1 in parameters
     for i from 0
     do
       (when (> (length parameters) i)
         (loop for p2 in (subseq parameters (1+ i))
            do
              (when (parameter-conflict-p p1 p2)
                (return-from find-duplicated-parameters (values p1 p2)))))))

(defun resolve-duplicated-parameters (parameters)
  (loop for (p1 p2) = (multiple-value-list (find-duplicated-parameters parameters))
     while p1
     do
       (progn
         (setf (parameter-name p1) (format nil "~A~D" (parameter-name p1) 1))
         (setf (parameter-name p2) (format nil "~A~D" (parameter-name p2) 2)))))

(defmethod generate ((operation operation))
  (let* ((function-symbol (symbolize (operation-nickname operation)))
         (parameters (operation-parameters operation))
         (path (operation-path operation)))

    (resolve-duplicated-parameters parameters)

    (let* ((required-parameters (remove-if-not #'parameter-required parameters))
           (optional-parameters (remove-if #'parameter-required parameters))
           (path-parameters (remove-if-not
                             (lambda (p)
                               (equal (parameter-param-type p) "path"))
                             parameters))
           (query-parameters (remove-if-not
                              (lambda (p)
                                (equal (parameter-param-type p) "query"))
                              parameters))
           (body-parameter (first (remove-if-not
                                   (lambda (p)
                                     (equal (parameter-param-type p) "body"))
                                   parameters)))
           (argument-list (make-argument-list required-parameters optional-parameters)))
      `(defun ,function-symbol ,argument-list
         ,(make-operation-doc operation)
         ,@(loop for param in parameters
              collect (make-check-type-for-parameter param))
         (let* ((scheme *api-endpoint-scheme*)
                (host *api-endpoint-host*)
                (port *api-endpoint-port*)
                (path ,(if path-parameters
                           (make-path-format path)
                           path))
                (query nil))
           ,@(loop for param in query-parameters
                collect `(when ,(symbolize-parameter param)
                           (alexandria:appendf query
                                               (list (cons ,(parameter-name param)
                                                           ,(symbolize-parameter param))))))
           (let* ((query-string (quri:url-encode-params query))
                  (url (format nil "~A://~A:~D~A~:[~;?~A~]" scheme host port path query query-string)))
             (multiple-value-bind (stream status-code headers)
                 (drakma:http-request url
                                      :method ,(intern (operation-method operation) :keyword)
                                      :content-type "application/json"
                                      :connection-timeout 5
                                      ;; :read-timeout 5
                                      :want-stream t
                                      :ca-file *cluster-certificate-authority*
                                      :certificate *client-certificate*
                                      :key *client-key*
                                      ,@(when body-parameter
                                          `(:content (with-output-to-string (s)
                                                       (marshal s ,(symbolize-parameter body-parameter))))))
               ,(if (equal (operation-type operation) "v1.WatchEvent")
                   'stream
                   `(let* ((response (alexandria::read-stream-content-into-string stream))
                          (object (yason:parse response)))
                     (decode-object (gethash "kind" object) object))))))))))

(defmethod generate ((api api))
  (loop for operation in (api-operations api)
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
    (loop for model in (spec-models spec)
       do
         (progn
           (save model resources-output-stream)
           (format resources-output-stream "~%")))
    (loop for api in (spec-apis spec)
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
