(in-package :cube)

(defparameter *api-endpoint-host* nil)

(defparameter *api-endpoint-port* 8443)

(defparameter *cluster-certificate-authority* (merge-pathnames ".minikube/ca.crt" (user-homedir-pathname)))

(defparameter *client-certificate* (merge-pathnames ".minikube/client.crt" (user-homedir-pathname)))

(defparameter *client-key* (merge-pathnames ".minikube/client.key" (user-homedir-pathname)))
