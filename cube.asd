(in-package :cl-user)
(defpackage cube.asd
  (:use :cl :asdf))
(in-package cube.asd)

(defsystem cube
  :description "Common LISP Kubernetes Client"
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "hexiangyu@coobii.com"
  :depends-on (:alexandria
               :optima.ppcre
               :drakma
               :quri
               :ironclad
               :cl-json
               :cl-base64
               :cl-yaclyaml
               :cl-hash-util
               :cl-change-case)
  :serial t
  :components ((:file "package")
               (:file "variables")
               (:file "config")
               (:file "helper")
               (:file "resources")
               (:file "operations")))
