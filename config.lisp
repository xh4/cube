(in-package :cube)

(defgeneric read-config (source)
  (:documentation
   "Read a YAML configuration from a file or a list of files."))

;; https://kubernetes.io/docs/concepts/configuration/organize-cluster-access-kubeconfig/#merging-kubeconfig-files
(defun merge-configurations (&optional current new)
  "Destructively merge two k8s configurations.

Adds in CURRENT all the entries (K,V) from NEW for which K is not
currently associated to a value in CURRENT.

When called with zero arguments, produce an empty HASH-TABLE (this is
to satisfy REDUCE and easily build empty configurations)."
  (cond
    ((and current new)
     (maphash (lambda (key new-value)
                (multiple-value-bind (current-value exists-p)
                    (gethash key current)
                  (declare (ignore current-value))
                  (unless exists-p
                    (setf (gethash key current) new-value))))
              new)
     current)
    (t (make-hash-table :test #'equal))))

(defmethod read-config ((sequence sequence))
  "Read and merge a sequence of configuration files."
  ;; merge according to priority rules.
  (reduce #'merge-configurations
          ;; may error when deserializing
          (mapcar #'read-config
                  ;; ignore non-existing files
                  (delete nil (map 'list #'probe-file sequence)))))

(defmethod read-config ((path pathname))
  "Read a single configuration file."
  (cl-yy:yaml-load-file path :size-limit (* 1024 1024)))

(defmethod read-config :around ((path pathname))
  "Add an IGNORE restart around READ-CONFIG for pathnames."
  (restart-case (call-next-method)
    (ignore ()
      :report "Ignore this configuration file."
      ;; return empty configuration
      (merge-configurations))))

(defun read-default-config ()
  "Read the default configuration in this environment.

First, try to read configuration from one or more files listed in the
KUBECONFIG environment variable, if that variable is set.

Otherwise, try to read the configuration file from the home user
directory (~/.kube/config).

If this file does not exist, return a default configuration."
  (let ((source (or (uiop:getenv-pathnames "KUBECONFIG")
                    (probe-file (merge-pathnames #P".kube/config"
                                                 (user-homedir-pathname))))))
    (if source
        (read-config source)
        (alist-hash-table
         '(("apiVersion" "v1")
           ("kind" "Config"))
         :test #'equal))))

(defun make-credential-file (data base ext)
  (let* ((content (base64-string-to-string data))
         (md5 (byte-array-to-hex-string
               (digest-sequence :md5 (ascii-string-to-byte-array content))))
         (filename (format nil "~A-~A.~A" base md5 ext))
         (path (merge-pathnames filename (uiop:temporary-directory))))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream content)
      path)))

(defun load-config (config &key context)
  (let ((context-name (or context (gethash "current-context" config))))
    (when (null context-name) (error "Missing current context in config"))
    (let* ((contexts (gethash "contexts" config))
           (context (loop for ctx in contexts
                       when (equal (gethash "name" ctx) context-name)
                       do (return (gethash "context" ctx)))))
      (when (null context) (error "Can't find context ~S in config" context-name))
      (let* ((clusters (gethash "clusters" config))
             (cluster-name (gethash "cluster" context))
             (cluster (loop for cluster in clusters
                         when (equal (gethash "name" cluster) cluster-name)
                         do (return (gethash "cluster" cluster)))))
        (when (null cluster) (error "Can't find cluster ~S in config" cluster-name))
        (when-let ((server (gethash "server" cluster)))
          (let ((uri (quri:uri server)))
            (setf *api-endpoint-host* (quri:uri-host uri))
            (setf *api-endpoint-port* (quri:uri-port uri))))
        (when-let ((ca-data (gethash "certificate-authority-data" cluster)))
          (let ((path (make-credential-file ca-data "ca" "crt")))
            (setf *cluster-certificate-authority* path)))
        (when-let ((ca-path (gethash "certificate-authority" cluster)))
          (setf *cluster-certificate-authority* (pathname ca-path))))
      (let* ((users (gethash "users" config))
             (user-name (gethash "user" context))
             (user (loop for user in users
                      when (equal (gethash "name" user) user-name)
                      do (return (gethash "user" user)))))
        (when (null user) (error "Can't find user ~S in config" user-name))
        (when-let ((crt-data (gethash "client-certificate-data" user)))
          (let ((path (make-credential-file crt-data "client" "crt")))
            (setf *client-certificate* path)))
        (when-let ((crt-path (gethash "client-certificate" user)))
          (setf *client-certificate* (pathname crt-path)))
        (when-let ((key-data (gethash "client-key-data" user)))
          (let ((path (make-credential-file key-data "client" "key")))
            (setf *client-key* path)))
        (when-let ((key-path (gethash "client-key" user)))
          (setf *client-key* (pathname key-path)))))
    t))

(defun load-default-config (&key context)
  (load-config (read-default-config) :context context))
