# Common LISP Kubernetes Client

Kubernetes client library for Common LISP generated from the Swagger specification.

## Usage

After download or clone this repository and add it's path to `asdf:*central-registry*`:

```lisp
* (ql:quickload :cube)
; => (:CUBE)
```

## Config & Context

By default, the library will use the `current-context` in the default config file (`~/.kube/config`).

you can use a different context by

```lisp
* (load-default-config :context "macrokube")
```

or use a diffent config file

```lisp
* (load-config #p"/path/to/my/config")
```

## Examples

Creating a Redis Master Deployment

```lisp
(let* ((labels '(("app" . "redis")
                 ("role" . "master")
                 ("tier" . "backend")))
       (container (make-instance 'container
                                 :name "master"
                                 :image "k8s.gcr.io/redis:e2e"
                                 :resources
                                 (make-instance 'resource-requirements
                                                :requests '(("cpu" . "100m")
                                                            ("memory" . "100Mi")))
                                 :ports (list
                                         (make-instance 'container-port
                                                        :container-port 6379))))
       (pod-spec (make-instance 'pod-spec
                                :containers (list container)))
       (pod-template-spec (make-instance 'pod-template-spec
                                         :metadata (make-instance 'object-meta
                                                                  :labels labels)
                                         :spec pod-spec))
       (deployment-spec (make-instance 'deployment-spec
                                       :selector (make-instance 'label-selector
                                                                :match-labels labels)
                                       :replicas 1
                                       :template pod-template-spec))
       (deployment (make-instance 'deployment
                                  :metadata (make-instance 'object-meta
                                                           :name "redis-master")
                                  :spec deployment-spec)))
  (create-namespaced-deployment deployment "default"))
```

## OpenSSL
