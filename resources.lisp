
(in-package :cube)


(defclass service-account
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ServiceAccount" :allocation :class)
           (automount-service-account-token
            :initarg
            :automount-service-account-token
            :type
            (or boolean null)
            :documentation
            "AutomountServiceAccountToken indicates whether pods running as this service account should have an API token automatically mounted. Can be overridden at the pod level.")
           (secrets
            :initarg
            :secrets
            :type
            list
            :documentation
            "Secrets is the list of secrets allowed to be used by pods running using this ServiceAccount. More info: https://kubernetes.io/docs/concepts/configuration/secret")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata")
           (image-pull-secrets
            :initarg
            :image-pull-secrets
            :type
            list
            :documentation
            "ImagePullSecrets is a list of references to secrets in the same namespace to use for pulling any images in pods that reference this ServiceAccount. ImagePullSecrets are distinct from Secrets because Secrets can be mounted in the pod, but ImagePullSecrets are only accessed by the kubelet. More info: https://kubernetes.io/docs/concepts/containers/images/#specifying-imagepullsecrets-on-a-pod"))
          (:documentation
           "ServiceAccount binds together: * a name, understood by users, and perhaps by peripheral systems, for an identity * a principal that can be authenticated and authorized * a set of secrets"))

(defmethod unmarshal
  ((object service-account) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "automountServiceAccountToken" source)
    (when present-p
      (setf (slot-value object 'automount-service-account-token)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "secrets" source)
    (when present-p
      (setf (slot-value object 'secrets)
            (decode-object (cons "array" "ObjectReference") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value))))
  (multiple-value-bind (value present-p)
      (gethash "imagePullSecrets" source)
    (when present-p
      (setf (slot-value object 'image-pull-secrets)
            (decode-object
             (cons "array" "LocalObjectReference")
             value)))))


(defclass pod-affinity
          (resource)
          ((preferred-during-scheduling-ignored-during-execution
            :initarg
            :preferred-during-scheduling-ignored-during-execution
            :type
            list
            :documentation
            "The scheduler will prefer to schedule pods to nodes that satisfy the affinity expressions specified by this field, but it may choose a node that violates one or more of the expressions. The node that is most preferred is the one with the greatest sum of weights, i.e. for each node that meets all of the scheduling requirements (resource request, requiredDuringScheduling affinity expressions, etc.), compute a sum by iterating through the elements of this field and adding \"weight\" to the sum if the node has pods which matches the corresponding podAffinityTerm; the node(s) with the highest sum are the most preferred.")
           (required-during-scheduling-ignored-during-execution
            :initarg
            :required-during-scheduling-ignored-during-execution
            :type
            list
            :documentation
            "If the affinity requirements specified by this field are not met at scheduling time, the pod will not be scheduled onto the node. If the affinity requirements specified by this field cease to be met at some point during pod execution (e.g. due to a pod label update), the system may or may not try to eventually evict the pod from its node. When there are multiple elements, the lists of nodes corresponding to each podAffinityTerm are intersected, i.e. all terms must be satisfied."))
          (:documentation
           "Pod affinity is a group of inter pod affinity scheduling rules."))

(defmethod unmarshal
  ((object pod-affinity) source)
  (multiple-value-bind (value present-p)
      (gethash "preferredDuringSchedulingIgnoredDuringExecution"
               source)
    (when present-p
      (setf (slot-value object
                        'preferred-during-scheduling-ignored-during-execution)
            (decode-object
             (cons "array" "WeightedPodAffinityTerm")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "requiredDuringSchedulingIgnoredDuringExecution" source)
    (when present-p
      (setf (slot-value object
                        'required-during-scheduling-ignored-during-execution)
            (decode-object (cons "array" "PodAffinityTerm") value)))))


(defclass object-meta
          (resource)
          ((deletion-timestamp
            :initarg
            :deletion-timestamp
            :type
            (or string null)
            :documentation
            "DeletionTimestamp is RFC 3339 date and time at which this resource will be deleted. This field is set by the server when a graceful deletion is requested by the user, and is not directly settable by a client. The resource is expected to be deleted (no longer visible from resource lists, and not reachable by name) after the time in this field, once the finalizers list is empty. As long as the finalizers list contains items, deletion is blocked. Once the deletionTimestamp is set, this value may not be unset or be set further into the future, although it may be shortened or the resource may be deleted prior to this time. For example, a user may request that a pod is deleted in 30 seconds. The Kubelet will react by sending a graceful termination signal to the containers in the pod. After that 30 seconds, the Kubelet will send a hard termination signal (SIGKILL) to the container and after cleanup, remove the pod from the API. In the presence of network partitions, this object may still exist after this timestamp, until an administrator or automated process can determine the resource is fully terminated. If not set, graceful deletion of the object has not been requested.

Populated by the system when a graceful deletion is requested. Read-only. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata")
           (self-link
            :initarg
            :self-link
            :type
            (or string null)
            :documentation
            "SelfLink is a URL representing this object. Populated by the system. Read-only.")
           (creation-timestamp
            :initarg
            :creation-timestamp
            :type
            (or string null)
            :documentation
            "CreationTimestamp is a timestamp representing the server time when this object was created. It is not guaranteed to be set in happens-before order across separate operations. Clients may not set this value. It is represented in RFC3339 form and is in UTC.

Populated by the system. Read-only. Null for lists. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata")
           (annotations
            :initarg
            :annotations
            :type
            (or hash-table null)
            :documentation
            "Annotations is an unstructured key value map stored with a resource that may be set by external tools to store and retrieve arbitrary metadata. They are not queryable and should be preserved when modifying objects. More info: http://kubernetes.io/docs/user-guide/annotations")
           (initializers
            :initarg
            :initializers
            :type
            (or initializers null)
            :documentation
            "An initializer is a controller which enforces some system invariant at object creation time. This field is a list of initializers that have not yet acted on this object. If nil or empty, this object has been completely initialized. Otherwise, the object is considered uninitialized and is hidden (in list/watch and get calls) from clients that haven't explicitly asked to observe uninitialized objects.

When an object is created, the system will populate this list with the current set of initializers. Only privileged users may set or modify this list. Once it is empty, it may not be modified further by any user.")
           (labels :initarg
             :labels
             :type
             (or hash-table null)
             :documentation
             "Map of string keys and values that can be used to organize and categorize (scope and select) objects. May match selectors of replication controllers and services. More info: http://kubernetes.io/docs/user-guide/labels")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name must be unique within a namespace. Is required when creating resources, although some resources may allow a client to request the generation of an appropriate name automatically. Name is primarily intended for creation idempotence and configuration definition. Cannot be updated. More info: http://kubernetes.io/docs/user-guide/identifiers#names")
           (cluster-name
            :initarg
            :cluster-name
            :type
            (or string null)
            :documentation
            "The name of the cluster which the object belongs to. This is used to distinguish resources with same name and namespace in different clusters. This field is not set anywhere right now and apiserver is going to ignore it if set in create or update request.")
           (owner-references
            :initarg
            :owner-references
            :type
            list
            :documentation
            "List of objects depended by this object. If ALL objects in the list have been deleted, this object will be garbage collected. If this object is managed by a controller, then an entry in this list will point to this controller, with the controller field set to true. There cannot be more than one managing controller.")
           (deletion-grace-period-seconds
            :initarg
            :deletion-grace-period-seconds
            :type
            (or integer null)
            :documentation
            "Number of seconds allowed for this object to gracefully terminate before it will be removed from the system. Only set when deletionTimestamp is also set. May only be shortened. Read-only.")
           (namespace
            :initarg
            :namespace
            :type
            (or string null)
            :documentation
            "Namespace defines the space within each name must be unique. An empty namespace is equivalent to the \"default\" namespace, but \"default\" is the canonical representation. Not all objects are required to be scoped to a namespace - the value of this field for those objects will be empty.

Must be a DNS_LABEL. Cannot be updated. More info: http://kubernetes.io/docs/user-guide/namespaces")
           (finalizers
            :initarg
            :finalizers
            :type
            list
            :documentation
            "Must be empty before the object is deleted from the registry. Each entry is an identifier for the responsible component that will remove the entry from the list. If the deletionTimestamp of the object is non-nil, entries in this list can only be removed.")
           (resource-version
            :initarg
            :resource-version
            :type
            (or string null)
            :documentation
            "An opaque value that represents the internal version of this object that can be used by clients to determine when objects have changed. May be used for optimistic concurrency, change detection, and the watch operation on a resource or set of resources. Clients must treat these values as opaque and passed unmodified back to the server. They may only be valid for a particular resource or set of resources.

Populated by the system. Read-only. Value must be treated as opaque by clients and . More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#concurrency-control-and-consistency")
           (generate-name
            :initarg
            :generate-name
            :type
            (or string null)
            :documentation
            "GenerateName is an optional prefix, used by the server, to generate a unique name ONLY IF the Name field has not been provided. If this field is used, the name returned to the client will be different than the name passed. This value will also be combined with a unique suffix. The provided value has the same validation rules as the Name field, and may be truncated by the length of the suffix required to make the value unique on the server.

If this field is specified and the generated name exists, the server will NOT return a 409 - instead, it will either return 201 Created or 500 with Reason ServerTimeout indicating a unique name could not be found in the time allotted, and the client should retry (optionally after the time indicated in the Retry-After header).

Applied only if Name is not specified. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#idempotency")
           (generation
            :initarg
            :generation
            :type
            (or integer null)
            :documentation
            "A sequence number representing a specific generation of the desired state. Populated by the system. Read-only.")
           (uid
            :initarg
            :uid
            :type
            (or string null)
            :documentation
            "UID is the unique in time and space value for this object. It is typically generated by the server on successful creation of a resource and is not allowed to change on PUT operations.

Populated by the system. Read-only. More info: http://kubernetes.io/docs/user-guide/identifiers#uids"))
          (:documentation
           "ObjectMeta is metadata that all persisted resources must have, which includes all objects users must create."))

(defmethod unmarshal
  ((object object-meta) source)
  (multiple-value-bind (value present-p)
      (gethash "deletionTimestamp" source)
    (when present-p
      (setf (slot-value object 'deletion-timestamp)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "selfLink" source)
    (when present-p
      (setf (slot-value object 'self-link)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "creationTimestamp" source)
    (when present-p
      (setf (slot-value object 'creation-timestamp)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "annotations" source)
    (when present-p
      (setf (slot-value object 'annotations)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "initializers" source)
    (when present-p
      (setf (slot-value object 'initializers)
            (decode-object "Initializers" value))))
  (multiple-value-bind (value present-p)
      (gethash "labels" source)
    (when present-p
      (setf (slot-value object 'labels)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "clusterName" source)
    (when present-p
      (setf (slot-value object 'cluster-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "ownerReferences" source)
    (when present-p
      (setf (slot-value object 'owner-references)
            (decode-object (cons "array" "OwnerReference") value))))
  (multiple-value-bind (value present-p)
      (gethash "deletionGracePeriodSeconds" source)
    (when present-p
      (setf (slot-value object 'deletion-grace-period-seconds)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "namespace" source)
    (when present-p
      (setf (slot-value object 'namespace)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "finalizers" source)
    (when present-p
      (setf (slot-value object 'finalizers)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "resourceVersion" source)
    (when present-p
      (setf (slot-value object 'resource-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "generateName" source)
    (when present-p
      (setf (slot-value object 'generate-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "generation" source)
    (when present-p
      (setf (slot-value object 'generation)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "uid" source)
    (when present-p
      (setf (slot-value object 'uid) (decode-object "string" value)))))


(defclass http-get-action
          (resource)
          ((host
            :initarg
            :host
            :type
            (or string null)
            :documentation
            "Host name to connect to, defaults to the pod IP. You probably want to set \"Host\" in httpHeaders instead.")
           (path
            :initarg
            :path
            :type
            (or string null)
            :documentation
            "Path to access on the HTTP server.")
           (scheme
            :initarg
            :scheme
            :type
            (or string null)
            :documentation
            "Scheme to use for connecting to the host. Defaults to HTTP.")
           (port
            :initarg
            :port
            :type
            string
            :documentation
            "Name or number of the port to access on the container. Number must be in the range 1 to 65535. Name must be an IANA_SVC_NAME.")
           (http-headers
            :initarg
            :http-headers
            :type
            list
            :documentation
            "Custom headers to set in the request. HTTP allows repeated headers."))
          (:documentation
           "HTTPGetAction describes an action based on HTTP Get requests."))

(defmethod unmarshal
  ((object http-get-action) source)
  (multiple-value-bind (value present-p)
      (gethash "host" source)
    (when present-p
      (setf (slot-value object 'host) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "scheme" source)
    (when present-p
      (setf (slot-value object 'scheme)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "port" source)
    (when present-p
      (setf (slot-value object 'port) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "httpHeaders" source)
    (when present-p
      (setf (slot-value object 'http-headers)
            (decode-object (cons "array" "HTTPHeader") value)))))


(defclass event-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "EventList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of events")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation "EventList is a list of events."))

(defmethod unmarshal
  ((object event-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "Event") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass endpoint-address
          (resource)
          ((hostname
            :initarg
            :hostname
            :type
            (or string null)
            :documentation
            "The Hostname of this endpoint")
           (node-name
            :initarg
            :node-name
            :type
            (or string null)
            :documentation
            "Optional: Node hosting this endpoint. This can be used to determine endpoints local to a node.")
           (ip
            :initarg
            :ip
            :type
            string
            :documentation
            "The IP of this endpoint. May not be loopback (127.0.0.0/8), link-local (169.254.0.0/16), or link-local multicast ((224.0.0.0/24). IPv6 is also accepted but not fully supported on all platforms. Also, certain kubernetes components, like kube-proxy, are not IPv6 ready.")
           (target-ref
            :initarg
            :target-ref
            :type
            (or object-reference null)
            :documentation
            "Reference to object providing the endpoint."))
          (:documentation
           "EndpointAddress is a tuple that describes single IP address."))

(defmethod unmarshal
  ((object endpoint-address) source)
  (multiple-value-bind (value present-p)
      (gethash "hostname" source)
    (when present-p
      (setf (slot-value object 'hostname)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "nodeName" source)
    (when present-p
      (setf (slot-value object 'node-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "ip" source)
    (when present-p
      (setf (slot-value object 'ip) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "targetRef" source)
    (when present-p
      (setf (slot-value object 'target-ref)
            (decode-object "ObjectReference" value)))))


(defclass container-state-terminated
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "Message regarding the last termination of the container")
           (signal :initarg
                   :signal
                   :type
                   (or integer null)
                   :documentation
                   "Signal from the last termination of the container")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "(brief) reason from the last termination of the container")
           (container-id
            :initarg
            :container-id
            :type
            (or string null)
            :documentation
            "Container's ID in the format 'docker://<container_id>'")
           (exit-code
            :initarg
            :exit-code
            :type
            integer
            :documentation
            "Exit status from the last termination of the container")
           (finished-at
            :initarg
            :finished-at
            :type
            (or string null)
            :documentation
            "Time at which the container last terminated")
           (started-at
            :initarg
            :started-at
            :type
            (or string null)
            :documentation
            "Time at which previous execution of the container started"))
          (:documentation
           "ContainerStateTerminated is a terminated state of a container."))

(defmethod unmarshal
  ((object container-state-terminated) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "signal" source)
    (when present-p
      (setf (slot-value object 'signal)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "containerID" source)
    (when present-p
      (setf (slot-value object 'container-id)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "exitCode" source)
    (when present-p
      (setf (slot-value object 'exit-code)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "finishedAt" source)
    (when present-p
      (setf (slot-value object 'finished-at)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "startedAt" source)
    (when present-p
      (setf (slot-value object 'started-at)
            (decode-object "string" value)))))


(defclass replication-controller-spec
          (resource)
          ((selector
            :initarg
            :selector
            :type
            (or hash-table null)
            :documentation
            "Selector is a label query over pods that should match the Replicas count. If Selector is empty, it is defaulted to the labels present on the Pod template. Label keys and values that must match in order to be controlled by this replication controller, if empty defaulted to labels on Pod template. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/labels/#label-selectors")
           (replicas
            :initarg
            :replicas
            :type
            (or integer null)
            :documentation
            "Replicas is the number of desired replicas. This is a pointer to distinguish between explicit zero and unspecified. Defaults to 1. More info: https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller#what-is-a-replicationcontroller")
           (template
            :initarg
            :template
            :type
            (or pod-template-spec null)
            :documentation
            "Template is the object that describes the pod that will be created if insufficient replicas are detected. This takes precedence over a TemplateRef. More info: https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller#pod-template")
           (min-ready-seconds
            :initarg
            :min-ready-seconds
            :type
            (or integer null)
            :documentation
            "Minimum number of seconds for which a newly created pod should be ready without any of its container crashing, for it to be considered available. Defaults to 0 (pod will be considered available as soon as it is ready)"))
          (:documentation
           "ReplicationControllerSpec is the specification of a replication controller."))

(defmethod unmarshal
  ((object replication-controller-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "selector" source)
    (when present-p
      (setf (slot-value object 'selector)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "template" source)
    (when present-p
      (setf (slot-value object 'template)
            (decode-object "PodTemplateSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "minReadySeconds" source)
    (when present-p
      (setf (slot-value object 'min-ready-seconds)
            (decode-object (cons "integer" "int32") value)))))


(defclass scale-io-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            local-object-reference
            :documentation
            "SecretRef references to the secret for ScaleIO user and other sensitive information. If this is not provided, Login operation will fail.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (protection-domain
            :initarg
            :protection-domain
            :type
            (or string null)
            :documentation
            "The name of the ScaleIO Protection Domain for the configured storage.")
           (storage-pool
            :initarg
            :storage-pool
            :type
            (or string null)
            :documentation
            "The ScaleIO Storage Pool associated with the protection domain.")
           (ssl-enabled
            :initarg
            :ssl-enabled
            :type
            (or boolean null)
            :documentation
            "Flag to enable/disable SSL communication with Gateway, default false")
           (volume-name
            :initarg
            :volume-name
            :type
            (or string null)
            :documentation
            "The name of a volume already created in the ScaleIO system that is associated with this volume source.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified.")
           (storage-mode
            :initarg
            :storage-mode
            :type
            (or string null)
            :documentation
            "Indicates whether the storage for a volume should be ThickProvisioned or ThinProvisioned.")
           (system
            :initarg
            :system
            :type
            string
            :documentation
            "The name of the storage system as configured in ScaleIO.")
           (gateway
            :initarg
            :gateway
            :type
            string
            :documentation
            "The host address of the ScaleIO API Gateway."))
          (:documentation
           "ScaleIOVolumeSource represents a persistent ScaleIO volume"))

(defmethod unmarshal
  ((object scale-io-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "protectionDomain" source)
    (when present-p
      (setf (slot-value object 'protection-domain)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "storagePool" source)
    (when present-p
      (setf (slot-value object 'storage-pool)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "sslEnabled" source)
    (when present-p
      (setf (slot-value object 'ssl-enabled)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeName" source)
    (when present-p
      (setf (slot-value object 'volume-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "storageMode" source)
    (when present-p
      (setf (slot-value object 'storage-mode)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "system" source)
    (when present-p
      (setf (slot-value object 'system)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "gateway" source)
    (when present-p
      (setf (slot-value object 'gateway)
            (decode-object "string" value)))))


(defclass portworx-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "FSType represents the filesystem type to mount Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\". Implicitly inferred to be \"ext4\" if unspecified.")
           (volume-id
            :initarg
            :volume-id
            :type
            string
            :documentation
            "VolumeID uniquely identifies a Portworx volume"))
          (:documentation
           "PortworxVolumeSource represents a Portworx volume resource."))

(defmethod unmarshal
  ((object portworx-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeID" source)
    (when present-p
      (setf (slot-value object 'volume-id)
            (decode-object "string" value)))))


(defclass downward-api-projection
          (resource)
          ((items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is a list of DownwardAPIVolume file"))
          (:documentation
           "Represents downward API info for projecting into a projected volume. Note that this is identical to a downwardAPI volume source without the default mode."))

(defmethod unmarshal
  ((object downward-api-projection) source)
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object
             (cons "array" "DownwardAPIVolumeFile")
             value)))))


(defclass vsphere-virtual-disk-volume-source
          (resource)
          ((storage-policy-name
            :initarg
            :storage-policy-name
            :type
            (or string null)
            :documentation
            "Storage Policy Based Management (SPBM) profile name.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified.")
           (storage-policy-id
            :initarg
            :storage-policy-id
            :type
            (or string null)
            :documentation
            "Storage Policy Based Management (SPBM) profile ID associated with the StoragePolicyName.")
           (volume-path
            :initarg
            :volume-path
            :type
            string
            :documentation
            "Path that identifies vSphere volume vmdk"))
          (:documentation "Represents a vSphere volume resource."))

(defmethod unmarshal
  ((object vsphere-virtual-disk-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "storagePolicyName" source)
    (when present-p
      (setf (slot-value object 'storage-policy-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "storagePolicyID" source)
    (when present-p
      (setf (slot-value object 'storage-policy-id)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumePath" source)
    (when present-p
      (setf (slot-value object 'volume-path)
            (decode-object "string" value)))))


(defclass service
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Service" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or service-status null)
            :documentation
            "Most recently observed status of the service. Populated by the system. Read-only. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (spec
            :initarg
            :spec
            :type
            (or service-spec null)
            :documentation
            "Spec defines the behavior of a service. https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "Service is a named abstraction of software service (for example, mysql) consisting of local port (for example 3306) that the proxy listens on, and the selector that determines which pods will answer requests sent through the proxy."))

(defmethod unmarshal
  ((object service) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "ServiceStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "ServiceSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass container-state-running
          (resource)
          ((started-at
            :initarg
            :started-at
            :type
            (or string null)
            :documentation
            "Time at which the container was last (re-)started"))
          (:documentation
           "ContainerStateRunning is a running state of a container."))

(defmethod unmarshal
  ((object container-state-running) source)
  (multiple-value-bind (value present-p)
      (gethash "startedAt" source)
    (when present-p
      (setf (slot-value object 'started-at)
            (decode-object "string" value)))))


(defclass azure-data-disk-kind (resource) nil (:documentation nil))

(defmethod unmarshal ((object azure-data-disk-kind) source))


(defclass iscsi-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or local-object-reference null)
            :documentation
            "CHAP Secret for iSCSI target and initiator authentication")
           (portals
            :initarg
            :portals
            :type
            list
            :documentation
            "iSCSI Target Portal List. The portal is either an IP or ip_addr:port if the port is other than default (typically TCP ports 860 and 3260).")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the ReadOnly setting in VolumeMounts. Defaults to false.")
           (lun
            :initarg
            :lun
            :type
            integer
            :documentation
            "iSCSI Target Lun number.")
           (target-portal
            :initarg
            :target-portal
            :type
            string
            :documentation
            "iSCSI Target Portal. The Portal is either an IP or ip_addr:port if the port is other than default (typically TCP ports 860 and 3260).")
           (iscsi-interface
            :initarg
            :iscsi-interface
            :type
            (or string null)
            :documentation
            "iSCSI Interface Name that uses an iSCSI transport. Defaults to 'default' (tcp).")
           (iqn
            :initarg
            :iqn
            :type
            string
            :documentation
            "Target iSCSI Qualified Name.")
           (chap-auth-discovery
            :initarg
            :chap-auth-discovery
            :type
            (or boolean null)
            :documentation
            "whether support iSCSI Discovery CHAP authentication")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type of the volume that you want to mount. Tip: Ensure that the filesystem type is supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://kubernetes.io/docs/concepts/storage/volumes#iscsi")
           (initiator-name
            :initarg
            :initiator-name
            :type
            (or string null)
            :documentation
            "Custom iSCSI Initiator Name. If initiatorName is specified with iscsiInterface simultaneously, new iSCSI interface <target portal>:<volume name> will be created for the connection.")
           (chap-auth-session
            :initarg
            :chap-auth-session
            :type
            (or boolean null)
            :documentation
            "whether support iSCSI Session CHAP authentication"))
          (:documentation
           "Represents an ISCSI disk. ISCSI volumes can only be mounted as read/write once. ISCSI volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object iscsi-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "portals" source)
    (when present-p
      (setf (slot-value object 'portals)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "lun" source)
    (when present-p
      (setf (slot-value object 'lun)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "targetPortal" source)
    (when present-p
      (setf (slot-value object 'target-portal)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "iscsiInterface" source)
    (when present-p
      (setf (slot-value object 'iscsi-interface)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "iqn" source)
    (when present-p
      (setf (slot-value object 'iqn) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "chapAuthDiscovery" source)
    (when present-p
      (setf (slot-value object 'chap-auth-discovery)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "initiatorName" source)
    (when present-p
      (setf (slot-value object 'initiator-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "chapAuthSession" source)
    (when present-p
      (setf (slot-value object 'chap-auth-session)
            (decode-object "boolean" value)))))


(defclass component-status-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ComponentStatusList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of ComponentStatus objects.")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "Status of all the conditions for the component as a list of ComponentStatus objects."))

(defmethod unmarshal
  ((object component-status-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "ComponentStatus") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass env-var
          (resource)
          ((value-from
            :initarg
            :value-from
            :type
            (or env-var-source null)
            :documentation
            "Source for the environment variable's value. Cannot be used if value is not empty.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "Name of the environment variable. Must be a C_IDENTIFIER.")
           (value
            :initarg
            :value
            :type
            (or string null)
            :documentation
            "Variable references $(VAR_NAME) are expanded using the previous defined environment variables in the container and any service environment variables. If a variable cannot be resolved, the reference in the input string will be unchanged. The $(VAR_NAME) syntax can be escaped with a double $$, ie: $$(VAR_NAME). Escaped references will never be expanded, regardless of whether the variable exists or not. Defaults to \"\"."))
          (:documentation
           "EnvVar represents an environment variable present in a Container."))

(defmethod unmarshal
  ((object env-var) source)
  (multiple-value-bind (value present-p)
      (gethash "valueFrom" source)
    (when present-p
      (setf (slot-value object 'value-from)
            (decode-object "EnvVarSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "value" source)
    (when present-p
      (setf (slot-value object 'value)
            (decode-object "string" value)))))


(defclass pod-affinity-term
          (resource)
          ((topology-key
            :initarg
            :topology-key
            :type
            string
            :documentation
            "This pod should be co-located (affinity) or not co-located (anti-affinity) with the pods matching the labelSelector in the specified namespaces, where co-located is defined as running on a node whose value of the label with key topologyKey matches that of any node on which any of the selected pods is running. Empty topologyKey is not allowed.")
           (label-selector
            :initarg
            :label-selector
            :type
            (or label-selector null)
            :documentation
            "A label query over a set of resources, in this case pods.")
           (namespaces
            :initarg
            :namespaces
            :type
            list
            :documentation
            "namespaces specifies which namespaces the labelSelector applies to (matches against); null or empty list means \"this pod's namespace\""))
          (:documentation
           "Defines a set of pods (namely those matching the labelSelector relative to the given namespace(s)) that this pod should be co-located (affinity) or not co-located (anti-affinity) with, where co-located is defined as running on a node whose value of the label with key <topologyKey> matches that of any node on which a pod of the set of pods is running"))

(defmethod unmarshal
  ((object pod-affinity-term) source)
  (multiple-value-bind (value present-p)
      (gethash "topologyKey" source)
    (when present-p
      (setf (slot-value object 'topology-key)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "labelSelector" source)
    (when present-p
      (setf (slot-value object 'label-selector)
            (decode-object "LabelSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "namespaces" source)
    (when present-p
      (setf (slot-value object 'namespaces)
            (decode-object (cons "array" "string") value)))))


(defclass uid (resource) nil (:documentation nil))

(defmethod unmarshal ((object uid) source))


(defclass pod-spec
          (resource)
          ((host-ipc
            :initarg
            :host-ipc
            :type
            (or boolean null)
            :documentation
            "Use the host's ipc namespace. Optional: Default to false.")
           (host-network
            :initarg
            :host-network
            :type
            (or boolean null)
            :documentation
            "Host networking requested for this pod. Use the host's network namespace. If this option is set, the ports that will be used must be specified. Default to false.")
           (service-account
            :initarg
            :service-account
            :type
            (or string null)
            :documentation
            "DeprecatedServiceAccount is a depreciated alias for ServiceAccountName. Deprecated: Use serviceAccountName instead.")
           (active-deadline-seconds
            :initarg
            :active-deadline-seconds
            :type
            (or integer null)
            :documentation
            "Optional duration in seconds the pod may be active on the node relative to StartTime before the system will actively try to mark it failed and kill associated containers. Value must be a positive integer.")
           (termination-grace-period-seconds
            :initarg
            :termination-grace-period-seconds
            :type
            (or integer null)
            :documentation
            "Optional duration in seconds the pod needs to terminate gracefully. May be decreased in delete request. Value must be non-negative integer. The value zero indicates delete immediately. If this value is nil, the default grace period will be used instead. The grace period is the duration in seconds after the processes running in the pod are sent a termination signal and the time when the processes are forcibly halted with a kill signal. Set this value longer than the expected cleanup time for your process. Defaults to 30 seconds.")
           (tolerations
            :initarg
            :tolerations
            :type
            list
            :documentation
            "If specified, the pod's tolerations.")
           (node-selector
            :initarg
            :node-selector
            :type
            (or hash-table null)
            :documentation
            "NodeSelector is a selector which must be true for the pod to fit on a node. Selector which must match a node's labels for the pod to be scheduled on that node. More info: https://kubernetes.io/docs/concepts/configuration/assign-pod-node/")
           (priority
            :initarg
            :priority
            :type
            (or integer null)
            :documentation
            "The priority value. Various system components use this field to find the priority of the pod. When Priority Admission Controller is enabled, it prevents users from setting this field. The admission controller populates this field from PriorityClassName. The higher the value, the higher the priority.")
           (host-aliases
            :initarg
            :host-aliases
            :type
            list
            :documentation
            "HostAliases is an optional list of hosts and IPs that will be injected into the pod's hosts file if specified. This is only valid for non-hostNetwork pods.")
           (automount-service-account-token
            :initarg
            :automount-service-account-token
            :type
            (or boolean null)
            :documentation
            "AutomountServiceAccountToken indicates whether a service account token should be automatically mounted.")
           (affinity
            :initarg
            :affinity
            :type
            (or affinity null)
            :documentation
            "If specified, the pod's scheduling constraints")
           (hostname
            :initarg
            :hostname
            :type
            (or string null)
            :documentation
            "Specifies the hostname of the Pod If not specified, the pod's hostname will be set to a system-defined value.")
           (security-context
            :initarg
            :security-context
            :type
            (or pod-security-context null)
            :documentation
            "SecurityContext holds pod-level security attributes and common container settings. Optional: Defaults to empty.  See type description for default values of each field.")
           (service-account-name
            :initarg
            :service-account-name
            :type
            (or string null)
            :documentation
            "ServiceAccountName is the name of the ServiceAccount to use to run this pod. More info: https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/")
           (priority-class-name
            :initarg
            :priority-class-name
            :type
            (or string null)
            :documentation
            "If specified, indicates the pod's priority. \"system-node-critical\" and \"system-cluster-critical\" are two special keywords which indicate the highest priorities with the former being the highest priority. Any other name must be defined by creating a PriorityClass object with that name. If not specified, the pod priority will be default or zero if there is no default.")
           (scheduler-name
            :initarg
            :scheduler-name
            :type
            (or string null)
            :documentation
            "If specified, the pod will be dispatched by specified scheduler. If not specified, the pod will be dispatched by default scheduler.")
           (node-name
            :initarg
            :node-name
            :type
            (or string null)
            :documentation
            "NodeName is a request to schedule this pod onto a specific node. If it is non-empty, the scheduler simply schedules this pod onto that node, assuming that it fits resource requirements.")
           (subdomain
            :initarg
            :subdomain
            :type
            (or string null)
            :documentation
            "If specified, the fully qualified Pod hostname will be \"<hostname>.<subdomain>.<pod namespace>.svc.<cluster domain>\". If not specified, the pod will not have a domainname at all.")
           (restart-policy
            :initarg
            :restart-policy
            :type
            (or string null)
            :documentation
            "Restart policy for all containers within the pod. One of Always, OnFailure, Never. Default to Always. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle/#restart-policy")
           (init-containers
            :initarg
            :init-containers
            :type
            list
            :documentation
            "List of initialization containers belonging to the pod. Init containers are executed in order prior to containers being started. If any init container fails, the pod is considered to have failed and is handled according to its restartPolicy. The name for an init container or normal container must be unique among all containers. Init containers may not have Lifecycle actions, Readiness probes, or Liveness probes. The resourceRequirements of an init container are taken into account during scheduling by finding the highest request/limit for each resource type, and then using the max of of that value or the sum of the normal containers. Limits are applied to init containers in a similar fashion. Init containers cannot currently be added or removed. Cannot be updated. More info: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/")
           (dns-config
            :initarg
            :dns-config
            :type
            (or pod-dns-config null)
            :documentation
            "Specifies the DNS parameters of a pod. Parameters specified here will be merged to the generated DNS configuration based on DNSPolicy.")
           (containers
            :initarg
            :containers
            :type
            list
            :documentation
            "List of containers belonging to the pod. Containers cannot currently be added or removed. There must be at least one container in a Pod. Cannot be updated.")
           (host-pid
            :initarg
            :host-pid
            :type
            (or boolean null)
            :documentation
            "Use the host's pid namespace. Optional: Default to false.")
           (dns-policy
            :initarg
            :dns-policy
            :type
            (or string null)
            :documentation
            "Set DNS policy for the pod. Defaults to \"ClusterFirst\". Valid values are 'ClusterFirstWithHostNet', 'ClusterFirst', 'Default' or 'None'. DNS parameters given in DNSConfig will be merged with the policy selected with DNSPolicy. To have DNS options set along with hostNetwork, you have to specify DNS policy explicitly to 'ClusterFirstWithHostNet'.")
           (volumes
            :initarg
            :volumes
            :type
            list
            :documentation
            "List of volumes that can be mounted by containers belonging to the pod. More info: https://kubernetes.io/docs/concepts/storage/volumes")
           (image-pull-secrets
            :initarg
            :image-pull-secrets
            :type
            list
            :documentation
            "ImagePullSecrets is an optional list of references to secrets in the same namespace to use for pulling any of the images used by this PodSpec. If specified, these secrets will be passed to individual puller implementations for them to use. For example, in the case of docker, only DockerConfig type secrets are honored. More info: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod")
           (share-process-namespace
            :initarg
            :share-process-namespace
            :type
            (or boolean null)
            :documentation
            "Share a single process namespace between all of the containers in a pod. When this is set containers will be able to view and signal processes from other containers in the same pod, and the first process in each container will not be assigned PID 1. HostPID and ShareProcessNamespace cannot both be set. Optional: Default to false. This field is alpha-level and is honored only by servers that enable the PodShareProcessNamespace feature."))
          (:documentation "PodSpec is a description of a pod."))

(defmethod unmarshal
  ((object pod-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "hostIPC" source)
    (when present-p
      (setf (slot-value object 'host-ipc)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "hostNetwork" source)
    (when present-p
      (setf (slot-value object 'host-network)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "serviceAccount" source)
    (when present-p
      (setf (slot-value object 'service-account)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "activeDeadlineSeconds" source)
    (when present-p
      (setf (slot-value object 'active-deadline-seconds)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "terminationGracePeriodSeconds" source)
    (when present-p
      (setf (slot-value object 'termination-grace-period-seconds)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "tolerations" source)
    (when present-p
      (setf (slot-value object 'tolerations)
            (decode-object (cons "array" "Toleration") value))))
  (multiple-value-bind (value present-p)
      (gethash "nodeSelector" source)
    (when present-p
      (setf (slot-value object 'node-selector)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "priority" source)
    (when present-p
      (setf (slot-value object 'priority)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "hostAliases" source)
    (when present-p
      (setf (slot-value object 'host-aliases)
            (decode-object (cons "array" "HostAlias") value))))
  (multiple-value-bind (value present-p)
      (gethash "automountServiceAccountToken" source)
    (when present-p
      (setf (slot-value object 'automount-service-account-token)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "affinity" source)
    (when present-p
      (setf (slot-value object 'affinity)
            (decode-object "Affinity" value))))
  (multiple-value-bind (value present-p)
      (gethash "hostname" source)
    (when present-p
      (setf (slot-value object 'hostname)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "securityContext" source)
    (when present-p
      (setf (slot-value object 'security-context)
            (decode-object "PodSecurityContext" value))))
  (multiple-value-bind (value present-p)
      (gethash "serviceAccountName" source)
    (when present-p
      (setf (slot-value object 'service-account-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "priorityClassName" source)
    (when present-p
      (setf (slot-value object 'priority-class-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "schedulerName" source)
    (when present-p
      (setf (slot-value object 'scheduler-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "nodeName" source)
    (when present-p
      (setf (slot-value object 'node-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "subdomain" source)
    (when present-p
      (setf (slot-value object 'subdomain)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "restartPolicy" source)
    (when present-p
      (setf (slot-value object 'restart-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "initContainers" source)
    (when present-p
      (setf (slot-value object 'init-containers)
            (decode-object (cons "array" "Container") value))))
  (multiple-value-bind (value present-p)
      (gethash "dnsConfig" source)
    (when present-p
      (setf (slot-value object 'dns-config)
            (decode-object "PodDNSConfig" value))))
  (multiple-value-bind (value present-p)
      (gethash "containers" source)
    (when present-p
      (setf (slot-value object 'containers)
            (decode-object (cons "array" "Container") value))))
  (multiple-value-bind (value present-p)
      (gethash "hostPID" source)
    (when present-p
      (setf (slot-value object 'host-pid)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "dnsPolicy" source)
    (when present-p
      (setf (slot-value object 'dns-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumes" source)
    (when present-p
      (setf (slot-value object 'volumes)
            (decode-object (cons "array" "Volume") value))))
  (multiple-value-bind (value present-p)
      (gethash "imagePullSecrets" source)
    (when present-p
      (setf (slot-value object 'image-pull-secrets)
            (decode-object
             (cons "array" "LocalObjectReference")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "shareProcessNamespace" source)
    (when present-p
      (setf (slot-value object 'share-process-namespace)
            (decode-object "boolean" value)))))


(defclass container-status
          (resource)
          ((restart-count
            :initarg
            :restart-count
            :type
            integer
            :documentation
            "The number of times the container has been restarted, currently based on the number of dead containers that have not yet been removed. Note that this is calculated from dead containers. But those containers are subject to garbage collection. This value will get capped at 5 by GC.")
           (last-state
            :initarg
            :last-state
            :type
            (or container-state null)
            :documentation
            "Details about the container's last termination condition.")
           (image
            :initarg
            :image
            :type
            string
            :documentation
            "The image the container is running. More info: https://kubernetes.io/docs/concepts/containers/images")
           (image-id
            :initarg
            :image-id
            :type
            string
            :documentation
            "ImageID of the container's image.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "This must be a DNS_LABEL. Each container in a pod must have a unique name. Cannot be updated.")
           (ready
            :initarg
            :ready
            :type
            boolean
            :documentation
            "Specifies whether the container has passed its readiness probe.")
           (state
            :initarg
            :state
            :type
            (or container-state null)
            :documentation
            "Details about the container's current condition.")
           (container-id
            :initarg
            :container-id
            :type
            (or string null)
            :documentation
            "Container's ID in the format 'docker://<container_id>'."))
          (:documentation
           "ContainerStatus contains details for the current status of this container."))

(defmethod unmarshal
  ((object container-status) source)
  (multiple-value-bind (value present-p)
      (gethash "restartCount" source)
    (when present-p
      (setf (slot-value object 'restart-count)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "lastState" source)
    (when present-p
      (setf (slot-value object 'last-state)
            (decode-object "ContainerState" value))))
  (multiple-value-bind (value present-p)
      (gethash "image" source)
    (when present-p
      (setf (slot-value object 'image)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "imageID" source)
    (when present-p
      (setf (slot-value object 'image-id)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "ready" source)
    (when present-p
      (setf (slot-value object 'ready)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "state" source)
    (when present-p
      (setf (slot-value object 'state)
            (decode-object "ContainerState" value))))
  (multiple-value-bind (value present-p)
      (gethash "containerID" source)
    (when present-p
      (setf (slot-value object 'container-id)
            (decode-object "string" value)))))


(defclass persistent-volume-access-mode
          (resource)
          nil
          (:documentation nil))

(defmethod unmarshal ((object persistent-volume-access-mode) source))


(defclass empty-dir-volume-source
          (resource)
          ((size-limit
            :initarg
            :size-limit
            :type
            (or string null)
            :documentation
            "Total amount of local storage required for this EmptyDir volume. The size limit is also applicable for memory medium. The maximum usage on memory medium EmptyDir would be the minimum value between the SizeLimit specified here and the sum of memory limits of all containers in a pod. The default is nil which means that the limit is undefined. More info: http://kubernetes.io/docs/user-guide/volumes#emptydir")
           (medium
            :initarg
            :medium
            :type
            (or string null)
            :documentation
            "What type of storage medium should back this directory. The default is \"\" which means to use the node's default medium. Must be an empty string (default) or Memory. More info: https://kubernetes.io/docs/concepts/storage/volumes#emptydir"))
          (:documentation
           "Represents an empty directory for a pod. Empty directory volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object empty-dir-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "sizeLimit" source)
    (when present-p
      (setf (slot-value object 'size-limit)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "medium" source)
    (when present-p
      (setf (slot-value object 'medium)
            (decode-object "string" value)))))


(defclass ceph-fs-persistent-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or secret-reference null)
            :documentation
            "Optional: SecretRef is reference to the authentication secret for User, default is empty. More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (monitors
            :initarg
            :monitors
            :type
            list
            :documentation
            "Required: Monitors is a collection of Ceph monitors More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (path
            :initarg
            :path
            :type
            (or string null)
            :documentation
            "Optional: Used as the mounted root, rather than the full Ceph tree, default is /")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts. More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (user
            :initarg
            :user
            :type
            (or string null)
            :documentation
            "Optional: User is the rados user name, default is admin More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (secret-file
            :initarg
            :secret-file
            :type
            (or string null)
            :documentation
            "Optional: SecretFile is the path to key ring for User, default is /etc/ceph/user.secret More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it"))
          (:documentation
           "Represents a Ceph Filesystem mount that lasts the lifetime of a pod Cephfs volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object ceph-fs-persistent-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "SecretReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "monitors" source)
    (when present-p
      (setf (slot-value object 'monitors)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "user" source)
    (when present-p
      (setf (slot-value object 'user) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "secretFile" source)
    (when present-p
      (setf (slot-value object 'secret-file)
            (decode-object "string" value)))))


(defclass env-from-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or secret-env-source null)
            :documentation
            "The Secret to select from")
           (config-map-ref
            :initarg
            :config-map-ref
            :type
            (or config-map-env-source null)
            :documentation
            "The ConfigMap to select from")
           (prefix
            :initarg
            :prefix
            :type
            (or string null)
            :documentation
            "An optional identifier to prepend to each key in the ConfigMap. Must be a C_IDENTIFIER."))
          (:documentation
           "EnvFromSource represents the source of a set of ConfigMaps"))

(defmethod unmarshal
  ((object env-from-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "SecretEnvSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "configMapRef" source)
    (when present-p
      (setf (slot-value object 'config-map-ref)
            (decode-object "ConfigMapEnvSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "prefix" source)
    (when present-p
      (setf (slot-value object 'prefix)
            (decode-object "string" value)))))


(defclass persistent-volume-mode (resource) nil (:documentation nil))

(defmethod unmarshal ((object persistent-volume-mode) source))


(defclass nfs-volume-source
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "Path that is exported by the NFS server. More info: https://kubernetes.io/docs/concepts/storage/volumes#nfs")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the NFS export to be mounted with read-only permissions. Defaults to false. More info: https://kubernetes.io/docs/concepts/storage/volumes#nfs")
           (server
            :initarg
            :server
            :type
            string
            :documentation
            "Server is the hostname or IP address of the NFS server. More info: https://kubernetes.io/docs/concepts/storage/volumes#nfs"))
          (:documentation
           "Represents an NFS mount that lasts the lifetime of a pod. NFS volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object nfs-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "server" source)
    (when present-p
      (setf (slot-value object 'server)
            (decode-object "string" value)))))


(defclass rbd-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or local-object-reference null)
            :documentation
            "SecretRef is name of the authentication secret for RBDUser. If provided overrides keyring. Default is nil. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (monitors
            :initarg
            :monitors
            :type
            list
            :documentation
            "A collection of Ceph monitors. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the ReadOnly setting in VolumeMounts. Defaults to false. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (keyring
            :initarg
            :keyring
            :type
            (or string null)
            :documentation
            "Keyring is the path to key ring for RBDUser. Default is /etc/ceph/keyring. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (image
            :initarg
            :image
            :type
            string
            :documentation
            "The rados image name. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (pool
            :initarg
            :pool
            :type
            (or string null)
            :documentation
            "The rados pool name. Default is rbd. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (user
            :initarg
            :user
            :type
            (or string null)
            :documentation
            "The rados user name. Default is admin. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type of the volume that you want to mount. Tip: Ensure that the filesystem type is supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://kubernetes.io/docs/concepts/storage/volumes#rbd"))
          (:documentation
           "Represents a Rados Block Device mount that lasts the lifetime of a pod. RBD volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object rbd-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "monitors" source)
    (when present-p
      (setf (slot-value object 'monitors)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "keyring" source)
    (when present-p
      (setf (slot-value object 'keyring)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "image" source)
    (when present-p
      (setf (slot-value object 'image)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "pool" source)
    (when present-p
      (setf (slot-value object 'pool) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "user" source)
    (when present-p
      (setf (slot-value object 'user) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass config-map-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ConfigMapList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is the list of ConfigMaps.")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "ConfigMapList is a resource containing a list of ConfigMap objects."))

(defmethod unmarshal
  ((object config-map-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "ConfigMap") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass node-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "NodeList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of nodes")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "NodeList is the whole list of all Nodes which have been registered with master."))

(defmethod unmarshal
  ((object node-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "Node") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass pod-template
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "PodTemplate" :allocation :class)
           (template
            :initarg
            :template
            :type
            (or pod-template-spec null)
            :documentation
            "Template defines the pods that will be created from this pod template. https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "PodTemplate describes a template for creating copies of a predefined pod."))

(defmethod unmarshal
  ((object pod-template) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "template" source)
    (when present-p
      (setf (slot-value object 'template)
            (decode-object "PodTemplateSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass quobyte-volume-source
          (resource)
          ((volume
            :initarg
            :volume
            :type
            string
            :documentation
            "Volume is a string that references an already created Quobyte volume by name.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the Quobyte volume to be mounted with read-only permissions. Defaults to false.")
           (group
            :initarg
            :group
            :type
            (or string null)
            :documentation
            "Group to map volume access to Default is no group")
           (user
            :initarg
            :user
            :type
            (or string null)
            :documentation
            "User to map volume access to Defaults to serivceaccount user")
           (registry
            :initarg
            :registry
            :type
            string
            :documentation
            "Registry represents a single or multiple Quobyte Registry services specified as a string as host:port pair (multiple entries are separated with commas) which acts as the central registry for volumes"))
          (:documentation
           "Represents a Quobyte mount that lasts the lifetime of a pod. Quobyte volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object quobyte-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "volume" source)
    (when present-p
      (setf (slot-value object 'volume)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "group" source)
    (when present-p
      (setf (slot-value object 'group)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "user" source)
    (when present-p
      (setf (slot-value object 'user) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "registry" source)
    (when present-p
      (setf (slot-value object 'registry)
            (decode-object "string" value)))))


(defclass rbd-persistent-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or secret-reference null)
            :documentation
            "SecretRef is name of the authentication secret for RBDUser. If provided overrides keyring. Default is nil. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (monitors
            :initarg
            :monitors
            :type
            list
            :documentation
            "A collection of Ceph monitors. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the ReadOnly setting in VolumeMounts. Defaults to false. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (keyring
            :initarg
            :keyring
            :type
            (or string null)
            :documentation
            "Keyring is the path to key ring for RBDUser. Default is /etc/ceph/keyring. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (image
            :initarg
            :image
            :type
            string
            :documentation
            "The rados image name. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (pool
            :initarg
            :pool
            :type
            (or string null)
            :documentation
            "The rados pool name. Default is rbd. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (user
            :initarg
            :user
            :type
            (or string null)
            :documentation
            "The rados user name. Default is admin. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type of the volume that you want to mount. Tip: Ensure that the filesystem type is supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://kubernetes.io/docs/concepts/storage/volumes#rbd"))
          (:documentation
           "Represents a Rados Block Device mount that lasts the lifetime of a pod. RBD volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object rbd-persistent-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "SecretReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "monitors" source)
    (when present-p
      (setf (slot-value object 'monitors)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "keyring" source)
    (when present-p
      (setf (slot-value object 'keyring)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "image" source)
    (when present-p
      (setf (slot-value object 'image)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "pool" source)
    (when present-p
      (setf (slot-value object 'pool) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "user" source)
    (when present-p
      (setf (slot-value object 'user) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass secret-volume-source
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the Secret or it's keys must be defined")
           (secret-name
            :initarg
            :secret-name
            :type
            (or string null)
            :documentation
            "Name of the secret in the pod's namespace to use. More info: https://kubernetes.io/docs/concepts/storage/volumes#secret")
           (default-mode
            :initarg
            :default-mode
            :type
            (or integer null)
            :documentation
            "Optional: mode bits to use on created files by default. Must be a value between 0 and 0777. Defaults to 0644. Directories within the path are not affected by this setting. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set.")
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "If unspecified, each key-value pair in the Data field of the referenced Secret will be projected into the volume as a file whose name is the key and content is the value. If specified, the listed keys will be projected into the specified paths, and unlisted keys will not be present. If a key is specified which is not present in the Secret, the volume setup will error unless it is marked optional. Paths must be relative and may not contain the '..' path or start with '..'."))
          (:documentation
           "Adapts a Secret into a volume.

The contents of the target Secret's Data field will be presented in a volume as files using the keys in the Data field as the file names. Secret volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object secret-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "secretName" source)
    (when present-p
      (setf (slot-value object 'secret-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "defaultMode" source)
    (when present-p
      (setf (slot-value object 'default-mode)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "KeyToPath") value)))))


(defclass fc-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (target-ww-ns
            :initarg
            :target-ww-ns
            :type
            list
            :documentation
            "Optional: FC target worldwide names (WWNs)")
           (lun
            :initarg
            :lun
            :type
            (or integer null)
            :documentation
            "Optional: FC target lun number")
           (wwids
            :initarg
            :wwids
            :type
            list
            :documentation
            "Optional: FC volume world wide identifiers (wwids) Either wwids or combination of targetWWNs and lun must be set, but not both simultaneously.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified."))
          (:documentation
           "Represents a Fibre Channel volume. Fibre Channel volumes can only be mounted as read/write once. Fibre Channel volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object fc-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "targetWWNs" source)
    (when present-p
      (setf (slot-value object 'target-ww-ns)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "lun" source)
    (when present-p
      (setf (slot-value object 'lun)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "wwids" source)
    (when present-p
      (setf (slot-value object 'wwids)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass iscsi-persistent-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or secret-reference null)
            :documentation
            "CHAP Secret for iSCSI target and initiator authentication")
           (portals
            :initarg
            :portals
            :type
            list
            :documentation
            "iSCSI Target Portal List. The Portal is either an IP or ip_addr:port if the port is other than default (typically TCP ports 860 and 3260).")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the ReadOnly setting in VolumeMounts. Defaults to false.")
           (lun
            :initarg
            :lun
            :type
            integer
            :documentation
            "iSCSI Target Lun number.")
           (target-portal
            :initarg
            :target-portal
            :type
            string
            :documentation
            "iSCSI Target Portal. The Portal is either an IP or ip_addr:port if the port is other than default (typically TCP ports 860 and 3260).")
           (iscsi-interface
            :initarg
            :iscsi-interface
            :type
            (or string null)
            :documentation
            "iSCSI Interface Name that uses an iSCSI transport. Defaults to 'default' (tcp).")
           (iqn
            :initarg
            :iqn
            :type
            string
            :documentation
            "Target iSCSI Qualified Name.")
           (chap-auth-discovery
            :initarg
            :chap-auth-discovery
            :type
            (or boolean null)
            :documentation
            "whether support iSCSI Discovery CHAP authentication")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type of the volume that you want to mount. Tip: Ensure that the filesystem type is supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://kubernetes.io/docs/concepts/storage/volumes#iscsi")
           (initiator-name
            :initarg
            :initiator-name
            :type
            (or string null)
            :documentation
            "Custom iSCSI Initiator Name. If initiatorName is specified with iscsiInterface simultaneously, new iSCSI interface <target portal>:<volume name> will be created for the connection.")
           (chap-auth-session
            :initarg
            :chap-auth-session
            :type
            (or boolean null)
            :documentation
            "whether support iSCSI Session CHAP authentication"))
          (:documentation
           "ISCSIPersistentVolumeSource represents an ISCSI disk. ISCSI volumes can only be mounted as read/write once. ISCSI volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object iscsi-persistent-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "SecretReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "portals" source)
    (when present-p
      (setf (slot-value object 'portals)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "lun" source)
    (when present-p
      (setf (slot-value object 'lun)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "targetPortal" source)
    (when present-p
      (setf (slot-value object 'target-portal)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "iscsiInterface" source)
    (when present-p
      (setf (slot-value object 'iscsi-interface)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "iqn" source)
    (when present-p
      (setf (slot-value object 'iqn) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "chapAuthDiscovery" source)
    (when present-p
      (setf (slot-value object 'chap-auth-discovery)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "initiatorName" source)
    (when present-p
      (setf (slot-value object 'initiator-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "chapAuthSession" source)
    (when present-p
      (setf (slot-value object 'chap-auth-session)
            (decode-object "boolean" value)))))


(defclass session-affinity-config
          (resource)
          ((client-ip
            :initarg
            :client-ip
            :type
            (or client-ip-config null)
            :documentation
            "clientIP contains the configurations of Client IP based session affinity."))
          (:documentation
           "SessionAffinityConfig represents the configurations of session affinity."))

(defmethod unmarshal
  ((object session-affinity-config) source)
  (multiple-value-bind (value present-p)
      (gethash "clientIP" source)
    (when present-p
      (setf (slot-value object 'client-ip)
            (decode-object "ClientIPConfig" value)))))


(defclass mount-propagation-mode (resource) nil (:documentation nil))

(defmethod unmarshal ((object mount-propagation-mode) source))


(defclass service-status
          (resource)
          ((load-balancer
            :initarg
            :load-balancer
            :type
            (or load-balancer-status null)
            :documentation
            "LoadBalancer contains the current status of the load-balancer, if one is present."))
          (:documentation
           "ServiceStatus represents the current status of a service."))

(defmethod unmarshal
  ((object service-status) source)
  (multiple-value-bind (value present-p)
      (gethash "loadBalancer" source)
    (when present-p
      (setf (slot-value object 'load-balancer)
            (decode-object "LoadBalancerStatus" value)))))


(defclass volume
          (resource)
          ((storageos
            :initarg
            :storageos
            :type
            (or storage-os-volume-source null)
            :documentation
            "StorageOS represents a StorageOS volume attached and mounted on Kubernetes nodes.")
           (cinder
            :initarg
            :cinder
            :type
            (or cinder-volume-source null)
            :documentation
            "Cinder represents a cinder volume attached and mounted on kubelets host machine More info: https://releases.k8s.io/HEAD/examples/mysql-cinder-pd/README.md")
           (aws-elastic-block-store
            :initarg
            :aws-elastic-block-store
            :type
            (or aws-elastic-block-store-volume-source null)
            :documentation
            "AWSElasticBlockStore represents an AWS Disk resource that is attached to a kubelet's host machine and then exposed to the pod. More info: https://kubernetes.io/docs/concepts/storage/volumes#awselasticblockstore")
           (azure-disk
            :initarg
            :azure-disk
            :type
            (or azure-disk-volume-source null)
            :documentation
            "AzureDisk represents an Azure Data Disk mount on the host and bind mount to the pod.")
           (quobyte
            :initarg
            :quobyte
            :type
            (or quobyte-volume-source null)
            :documentation
            "Quobyte represents a Quobyte mount on the host that shares a pod's lifetime")
           (config-map
            :initarg
            :config-map
            :type
            (or config-map-volume-source null)
            :documentation
            "ConfigMap represents a configMap that should populate this volume")
           (flex-volume
            :initarg
            :flex-volume
            :type
            (or flex-volume-source null)
            :documentation
            "FlexVolume represents a generic volume resource that is provisioned/attached using an exec based plugin.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "Volume's name. Must be a DNS_LABEL and unique within the pod. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names")
           (projected
            :initarg
            :projected
            :type
            (or projected-volume-source null)
            :documentation
            "Items for all in one resources secrets, configmaps, and downward API")
           (downward-api
            :initarg
            :downward-api
            :type
            (or downward-api-volume-source null)
            :documentation
            "DownwardAPI represents downward API about the pod that should populate this volume")
           (rbd
            :initarg
            :rbd
            :type
            (or rbd-volume-source null)
            :documentation
            "RBD represents a Rados Block Device mount on the host that shares a pod's lifetime. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md")
           (persistent-volume-claim
            :initarg
            :persistent-volume-claim
            :type
            (or persistent-volume-claim-volume-source null)
            :documentation
            "PersistentVolumeClaimVolumeSource represents a reference to a PersistentVolumeClaim in the same namespace. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistentvolumeclaims")
           (azure-file
            :initarg
            :azure-file
            :type
            (or azure-file-volume-source null)
            :documentation
            "AzureFile represents an Azure File Service mount on the host and bind mount to the pod.")
           (glusterfs
            :initarg
            :glusterfs
            :type
            (or glusterfs-volume-source null)
            :documentation
            "Glusterfs represents a Glusterfs mount on the host that shares a pod's lifetime. More info: https://releases.k8s.io/HEAD/examples/volumes/glusterfs/README.md")
           (secret
            :initarg
            :secret
            :type
            (or secret-volume-source null)
            :documentation
            "Secret represents a secret that should populate this volume. More info: https://kubernetes.io/docs/concepts/storage/volumes#secret")
           (host-path
            :initarg
            :host-path
            :type
            (or host-path-volume-source null)
            :documentation
            "HostPath represents a pre-existing file or directory on the host machine that is directly exposed to the container. This is generally used for system agents or other privileged things that are allowed to see the host machine. Most containers will NOT need this. More info: https://kubernetes.io/docs/concepts/storage/volumes#hostpath")
           (portworx-volume
            :initarg
            :portworx-volume
            :type
            (or portworx-volume-source null)
            :documentation
            "PortworxVolume represents a portworx volume attached and mounted on kubelets host machine")
           (flocker
            :initarg
            :flocker
            :type
            (or flocker-volume-source null)
            :documentation
            "Flocker represents a Flocker volume attached to a kubelet's host machine. This depends on the Flocker control service being running")
           (nfs
            :initarg
            :nfs
            :type
            (or nfs-volume-source null)
            :documentation
            "NFS represents an NFS mount on the host that shares a pod's lifetime More info: https://kubernetes.io/docs/concepts/storage/volumes#nfs")
           (gce-persistent-disk
            :initarg
            :gce-persistent-disk
            :type
            (or gce-persistent-disk-volume-source null)
            :documentation
            "GCEPersistentDisk represents a GCE Disk resource that is attached to a kubelet's host machine and then exposed to the pod. More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk")
           (fc
            :initarg
            :fc
            :type
            (or fc-volume-source null)
            :documentation
            "FC represents a Fibre Channel resource that is attached to a kubelet's host machine and then exposed to the pod.")
           (scale-io
            :initarg
            :scale-io
            :type
            (or scale-io-volume-source null)
            :documentation
            "ScaleIO represents a ScaleIO persistent volume attached and mounted on Kubernetes nodes.")
           (cephfs
            :initarg
            :cephfs
            :type
            (or ceph-fs-volume-source null)
            :documentation
            "CephFS represents a Ceph FS mount on the host that shares a pod's lifetime")
           (git-repo
            :initarg
            :git-repo
            :type
            (or git-repo-volume-source null)
            :documentation
            "GitRepo represents a git repository at a particular revision.")
           (iscsi
            :initarg
            :iscsi
            :type
            (or iscsi-volume-source null)
            :documentation
            "ISCSI represents an ISCSI Disk resource that is attached to a kubelet's host machine and then exposed to the pod. More info: https://releases.k8s.io/HEAD/examples/volumes/iscsi/README.md")
           (empty-dir
            :initarg
            :empty-dir
            :type
            (or empty-dir-volume-source null)
            :documentation
            "EmptyDir represents a temporary directory that shares a pod's lifetime. More info: https://kubernetes.io/docs/concepts/storage/volumes#emptydir")
           (photon-persistent-disk
            :initarg
            :photon-persistent-disk
            :type
            (or photon-persistent-disk-volume-source null)
            :documentation
            "PhotonPersistentDisk represents a PhotonController persistent disk attached and mounted on kubelets host machine")
           (vsphere-volume
            :initarg
            :vsphere-volume
            :type
            (or vsphere-virtual-disk-volume-source null)
            :documentation
            "VsphereVolume represents a vSphere volume attached and mounted on kubelets host machine"))
          (:documentation
           "Volume represents a named volume in a pod that may be accessed by any container in the pod."))

(defmethod unmarshal
  ((object volume) source)
  (multiple-value-bind (value present-p)
      (gethash "storageos" source)
    (when present-p
      (setf (slot-value object 'storageos)
            (decode-object "StorageOSVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "cinder" source)
    (when present-p
      (setf (slot-value object 'cinder)
            (decode-object "CinderVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "awsElasticBlockStore" source)
    (when present-p
      (setf (slot-value object 'aws-elastic-block-store)
            (decode-object "AWSElasticBlockStoreVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "azureDisk" source)
    (when present-p
      (setf (slot-value object 'azure-disk)
            (decode-object "AzureDiskVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "quobyte" source)
    (when present-p
      (setf (slot-value object 'quobyte)
            (decode-object "QuobyteVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "configMap" source)
    (when present-p
      (setf (slot-value object 'config-map)
            (decode-object "ConfigMapVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "flexVolume" source)
    (when present-p
      (setf (slot-value object 'flex-volume)
            (decode-object "FlexVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "projected" source)
    (when present-p
      (setf (slot-value object 'projected)
            (decode-object "ProjectedVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "downwardAPI" source)
    (when present-p
      (setf (slot-value object 'downward-api)
            (decode-object "DownwardAPIVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "rbd" source)
    (when present-p
      (setf (slot-value object 'rbd)
            (decode-object "RBDVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "persistentVolumeClaim" source)
    (when present-p
      (setf (slot-value object 'persistent-volume-claim)
            (decode-object
             "PersistentVolumeClaimVolumeSource"
             value))))
  (multiple-value-bind (value present-p)
      (gethash "azureFile" source)
    (when present-p
      (setf (slot-value object 'azure-file)
            (decode-object "AzureFileVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "glusterfs" source)
    (when present-p
      (setf (slot-value object 'glusterfs)
            (decode-object "GlusterfsVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "secret" source)
    (when present-p
      (setf (slot-value object 'secret)
            (decode-object "SecretVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "hostPath" source)
    (when present-p
      (setf (slot-value object 'host-path)
            (decode-object "HostPathVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "portworxVolume" source)
    (when present-p
      (setf (slot-value object 'portworx-volume)
            (decode-object "PortworxVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "flocker" source)
    (when present-p
      (setf (slot-value object 'flocker)
            (decode-object "FlockerVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "nfs" source)
    (when present-p
      (setf (slot-value object 'nfs)
            (decode-object "NFSVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "gcePersistentDisk" source)
    (when present-p
      (setf (slot-value object 'gce-persistent-disk)
            (decode-object "GCEPersistentDiskVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "fc" source)
    (when present-p
      (setf (slot-value object 'fc)
            (decode-object "FCVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "scaleIO" source)
    (when present-p
      (setf (slot-value object 'scale-io)
            (decode-object "ScaleIOVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "cephfs" source)
    (when present-p
      (setf (slot-value object 'cephfs)
            (decode-object "CephFSVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "gitRepo" source)
    (when present-p
      (setf (slot-value object 'git-repo)
            (decode-object "GitRepoVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "iscsi" source)
    (when present-p
      (setf (slot-value object 'iscsi)
            (decode-object "ISCSIVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "emptyDir" source)
    (when present-p
      (setf (slot-value object 'empty-dir)
            (decode-object "EmptyDirVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "photonPersistentDisk" source)
    (when present-p
      (setf (slot-value object 'photon-persistent-disk)
            (decode-object "PhotonPersistentDiskVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "vsphereVolume" source)
    (when present-p
      (setf (slot-value object 'vsphere-volume)
            (decode-object "VsphereVirtualDiskVolumeSource" value)))))


(defclass node-daemon-endpoints
          (resource)
          ((kubelet-endpoint
            :initarg
            :kubelet-endpoint
            :type
            (or daemon-endpoint null)
            :documentation
            "Endpoint on which Kubelet is listening."))
          (:documentation
           "NodeDaemonEndpoints lists ports opened by daemons running on the Node."))

(defmethod unmarshal
  ((object node-daemon-endpoints) source)
  (multiple-value-bind (value present-p)
      (gethash "kubeletEndpoint" source)
    (when present-p
      (setf (slot-value object 'kubelet-endpoint)
            (decode-object "DaemonEndpoint" value)))))


(defclass host-path-type (resource) nil (:documentation nil))

(defmethod unmarshal ((object host-path-type) source))


(defclass limit-range
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "LimitRange" :allocation :class)
           (spec
            :initarg
            :spec
            :type
            (or limit-range-spec null)
            :documentation
            "Spec defines the limits enforced. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "LimitRange sets resource usage limits for each kind of resource in a Namespace."))

(defmethod unmarshal
  ((object limit-range) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "LimitRangeSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass node-config-source
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "NodeConfigSource" :allocation :class)
           (config-map-ref
            :initarg
            :config-map-ref
            :type
            (or object-reference null)
            :documentation
            nil))
          (:documentation
           "NodeConfigSource specifies a source of node configuration. Exactly one subfield (excluding metadata) must be non-nil."))

(defmethod unmarshal
  ((object node-config-source) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "configMapRef" source)
    (when present-p
      (setf (slot-value object 'config-map-ref)
            (decode-object "ObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value)))))


(defclass scale-io-persistent-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            secret-reference
            :documentation
            "SecretRef references to the secret for ScaleIO user and other sensitive information. If this is not provided, Login operation will fail.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (protection-domain
            :initarg
            :protection-domain
            :type
            (or string null)
            :documentation
            "The name of the ScaleIO Protection Domain for the configured storage.")
           (storage-pool
            :initarg
            :storage-pool
            :type
            (or string null)
            :documentation
            "The ScaleIO Storage Pool associated with the protection domain.")
           (ssl-enabled
            :initarg
            :ssl-enabled
            :type
            (or boolean null)
            :documentation
            "Flag to enable/disable SSL communication with Gateway, default false")
           (volume-name
            :initarg
            :volume-name
            :type
            (or string null)
            :documentation
            "The name of a volume already created in the ScaleIO system that is associated with this volume source.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified.")
           (storage-mode
            :initarg
            :storage-mode
            :type
            (or string null)
            :documentation
            "Indicates whether the storage for a volume should be ThickProvisioned or ThinProvisioned.")
           (system
            :initarg
            :system
            :type
            string
            :documentation
            "The name of the storage system as configured in ScaleIO.")
           (gateway
            :initarg
            :gateway
            :type
            string
            :documentation
            "The host address of the ScaleIO API Gateway."))
          (:documentation
           "ScaleIOPersistentVolumeSource represents a persistent ScaleIO volume"))

(defmethod unmarshal
  ((object scale-io-persistent-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "SecretReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "protectionDomain" source)
    (when present-p
      (setf (slot-value object 'protection-domain)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "storagePool" source)
    (when present-p
      (setf (slot-value object 'storage-pool)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "sslEnabled" source)
    (when present-p
      (setf (slot-value object 'ssl-enabled)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeName" source)
    (when present-p
      (setf (slot-value object 'volume-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "storageMode" source)
    (when present-p
      (setf (slot-value object 'storage-mode)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "system" source)
    (when present-p
      (setf (slot-value object 'system)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "gateway" source)
    (when present-p
      (setf (slot-value object 'gateway)
            (decode-object "string" value)))))


(defclass endpoint-subset
          (resource)
          ((ports
            :initarg
            :ports
            :type
            list
            :documentation
            "Port numbers available on the related IP addresses.")
           (addresses
            :initarg
            :addresses
            :type
            list
            :documentation
            "IP addresses which offer the related ports that are marked as ready. These endpoints should be considered safe for load balancers and clients to utilize.")
           (not-ready-addresses
            :initarg
            :not-ready-addresses
            :type
            list
            :documentation
            "IP addresses which offer the related ports but are not currently marked as ready because they have not yet finished starting, have recently failed a readiness check, or have recently failed a liveness check."))
          (:documentation
           "EndpointSubset is a group of addresses with a common set of ports. The expanded set of endpoints is the Cartesian product of Addresses x Ports. For example, given:
  {
    Addresses: [{\"ip\": \"10.10.1.1\"}, {\"ip\": \"10.10.2.2\"}],
    Ports:     [{\"name\": \"a\", \"port\": 8675}, {\"name\": \"b\", \"port\": 309}]
  }
The resulting set of endpoints can be viewed as:
    a: [ 10.10.1.1:8675, 10.10.2.2:8675 ],
    b: [ 10.10.1.1:309, 10.10.2.2:309 ]"))

(defmethod unmarshal
  ((object endpoint-subset) source)
  (multiple-value-bind (value present-p)
      (gethash "ports" source)
    (when present-p
      (setf (slot-value object 'ports)
            (decode-object (cons "array" "EndpointPort") value))))
  (multiple-value-bind (value present-p)
      (gethash "addresses" source)
    (when present-p
      (setf (slot-value object 'addresses)
            (decode-object (cons "array" "EndpointAddress") value))))
  (multiple-value-bind (value present-p)
      (gethash "notReadyAddresses" source)
    (when present-p
      (setf (slot-value object 'not-ready-addresses)
            (decode-object (cons "array" "EndpointAddress") value)))))


(defclass service-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ServiceList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of services")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation "ServiceList holds a list of services."))

(defmethod unmarshal
  ((object service-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "Service") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass persistent-volume-claim-status
          (resource)
          ((conditions
            :initarg
            :conditions
            :type
            list
            :documentation
            "Current Condition of persistent volume claim. If underlying persistent volume is being resized then the Condition will be set to 'ResizeStarted'.")
           (phase :initarg
                  :phase
                  :type
                  (or string null)
                  :documentation
                  "Phase represents the current phase of PersistentVolumeClaim.")
           (capacity
            :initarg
            :capacity
            :type
            (or hash-table null)
            :documentation
            "Represents the actual resources of the underlying volume.")
           (access-modes
            :initarg
            :access-modes
            :type
            list
            :documentation
            "AccessModes contains the actual access modes the volume backing the PVC has. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#access-modes-1"))
          (:documentation
           "PersistentVolumeClaimStatus is the current status of a persistent volume claim."))

(defmethod unmarshal
  ((object persistent-volume-claim-status) source)
  (multiple-value-bind (value present-p)
      (gethash "conditions" source)
    (when present-p
      (setf (slot-value object 'conditions)
            (decode-object
             (cons "array" "PersistentVolumeClaimCondition")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "phase" source)
    (when present-p
      (setf (slot-value object 'phase)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "capacity" source)
    (when present-p
      (setf (slot-value object 'capacity)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "accessModes" source)
    (when present-p
      (setf (slot-value object 'access-modes)
            (decode-object
             (cons "array" "PersistentVolumeAccessMode")
             value)))))


(defclass client-ip-config
          (resource)
          ((timeout-seconds
            :initarg
            :timeout-seconds
            :type
            (or integer null)
            :documentation
            "timeoutSeconds specifies the seconds of ClientIP type session sticky time. The value must be >0 && <=86400(for 1 day) if ServiceAffinity == \"ClientIP\". Default value is 10800(for 3 hours)."))
          (:documentation
           "ClientIPConfig represents the configurations of Client IP based session affinity."))

(defmethod unmarshal
  ((object client-ip-config) source)
  (multiple-value-bind (value present-p)
      (gethash "timeoutSeconds" source)
    (when present-p
      (setf (slot-value object 'timeout-seconds)
            (decode-object (cons "integer" "int32") value)))))


(defclass config-map
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ConfigMap" :allocation :class)
           (data
            :initarg
            :data
            :type
            (or hash-table null)
            :documentation
            "Data contains the configuration data. Each key must consist of alphanumeric characters, '-', '_' or '.'. Values with non-UTF-8 byte sequences must use the BinaryData field. The keys stored in Data must not overlap with the keys in the BinaryData field, this is enforced during validation process.")
           (binary-data
            :initarg
            :binary-data
            :type
            (or hash-table null)
            :documentation
            "BinaryData contains the binary data. Each key must consist of alphanumeric characters, '-', '_' or '.'. BinaryData can contain byte sequences that are not in the UTF-8 range. The keys stored in BinaryData must not overlap with the ones in the Data field, this is enforced during validation process. Using this field will require 1.10+ apiserver and kubelet.")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "ConfigMap holds configuration data for pods to consume."))

(defmethod unmarshal
  ((object config-map) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "data" source)
    (when present-p
      (setf (slot-value object 'data) (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "binaryData" source)
    (when present-p
      (setf (slot-value object 'binary-data)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass object-reference
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ObjectReference" :allocation :class)
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names")
           (field-path
            :initarg
            :field-path
            :type
            (or string null)
            :documentation
            "If referring to a piece of an object instead of an entire object, this string should contain a valid JSON/Go field access statement, such as desiredState.manifest.containers[2]. For example, if the object reference is to a container within a pod, this would take on a value like: \"spec.containers{name}\" (where \"name\" refers to the name of the container that triggered the event) or if no container name is specified \"spec.containers[2]\" (container with index 2 in this pod). This syntax is chosen only to have some well-defined way of referencing a part of an object.")
           (namespace
            :initarg
            :namespace
            :type
            (or string null)
            :documentation
            "Namespace of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/namespaces/")
           (resource-version
            :initarg
            :resource-version
            :type
            (or string null)
            :documentation
            "Specific resourceVersion to which this reference is made, if any. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#concurrency-control-and-consistency")
           (uid
            :initarg
            :uid
            :type
            (or string null)
            :documentation
            "UID of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#uids"))
          (:documentation
           "ObjectReference contains enough information to let you inspect or modify the referred object."))

(defmethod unmarshal
  ((object object-reference) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fieldPath" source)
    (when present-p
      (setf (slot-value object 'field-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "namespace" source)
    (when present-p
      (setf (slot-value object 'namespace)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "resourceVersion" source)
    (when present-p
      (setf (slot-value object 'resource-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "uid" source)
    (when present-p
      (setf (slot-value object 'uid) (decode-object "string" value)))))


(defclass resource-field-selector
          (resource)
          ((container-name
            :initarg
            :container-name
            :type
            (or string null)
            :documentation
            "Container name: required for volumes, optional for env vars")
           (resource
            :initarg
            :resource
            :type
            string
            :documentation
            "Required: resource to select")
           (divisor
            :initarg
            :divisor
            :type
            (or string null)
            :documentation
            "Specifies the output format of the exposed resources, defaults to \"1\""))
          (:documentation
           "ResourceFieldSelector represents container resources (cpu, memory) and their output format"))

(defmethod unmarshal
  ((object resource-field-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "containerName" source)
    (when present-p
      (setf (slot-value object 'container-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "resource" source)
    (when present-p
      (setf (slot-value object 'resource)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "divisor" source)
    (when present-p
      (setf (slot-value object 'divisor)
            (decode-object "string" value)))))


(defclass persistent-volume-claim-condition
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "Human-readable message indicating details about last transition.")
           (last-probe-time
            :initarg
            :last-probe-time
            :type
            (or string null)
            :documentation
            "Last time we probed the condition.")
           (last-transition-time
            :initarg
            :last-transition-time
            :type
            (or string null)
            :documentation
            "Last time the condition transitioned from one status to another.")
           (type :initarg :type :type string :documentation nil)
           (status :initarg :status :type string :documentation nil)
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "Unique, this should be a short, machine understandable string that gives the reason for condition's last transition. If it reports \"ResizeStarted\" that means the underlying persistent volume is being resized."))
          (:documentation
           "PersistentVolumeClaimCondition contails details about state of pvc"))

(defmethod unmarshal
  ((object persistent-volume-claim-condition) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastProbeTime" source)
    (when present-p
      (setf (slot-value object 'last-probe-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastTransitionTime" source)
    (when present-p
      (setf (slot-value object 'last-transition-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value)))))


(defclass namespace-status
          (resource)
          ((phase :initarg
                  :phase
                  :type
                  (or string null)
                  :documentation
                  "Phase is the current lifecycle phase of the namespace. More info: https://kubernetes.io/docs/tasks/administer-cluster/namespaces/"))
          (:documentation
           "NamespaceStatus is information about the current status of a Namespace."))

(defmethod unmarshal
  ((object namespace-status) source)
  (multiple-value-bind (value present-p)
      (gethash "phase" source)
    (when present-p
      (setf (slot-value object 'phase)
            (decode-object "string" value)))))


(defclass storage-os-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or local-object-reference null)
            :documentation
            "SecretRef specifies the secret to use for obtaining the StorageOS API credentials.  If not specified, default values will be attempted.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (volume-namespace
            :initarg
            :volume-namespace
            :type
            (or string null)
            :documentation
            "VolumeNamespace specifies the scope of the volume within StorageOS.  If no namespace is specified then the Pod's namespace will be used.  This allows the Kubernetes name scoping to be mirrored within StorageOS for tighter integration. Set VolumeName to any name to override the default behaviour. Set to \"default\" if you are not using namespaces within StorageOS. Namespaces that do not pre-exist within StorageOS will be created.")
           (volume-name
            :initarg
            :volume-name
            :type
            (or string null)
            :documentation
            "VolumeName is the human-readable name of the StorageOS volume.  Volume names are only unique within a namespace.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified."))
          (:documentation
           "Represents a StorageOS persistent volume resource."))

(defmethod unmarshal
  ((object storage-os-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeNamespace" source)
    (when present-p
      (setf (slot-value object 'volume-namespace)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeName" source)
    (when present-p
      (setf (slot-value object 'volume-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass resource-quota-scope (resource) nil (:documentation nil))

(defmethod unmarshal ((object resource-quota-scope) source))


(defclass container
          (resource)
          ((resources
            :initarg
            :resources
            :type
            (or resource-requirements null)
            :documentation
            "Compute Resources required by this container. Cannot be updated. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#resources")
           (liveness-probe
            :initarg
            :liveness-probe
            :type
            (or probe null)
            :documentation
            "Periodic probe of container liveness. Container will be restarted if the probe fails. Cannot be updated. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes")
           (volume-devices
            :initarg
            :volume-devices
            :type
            list
            :documentation
            "volumeDevices is the list of block devices to be used by the container. This is an alpha feature and may change in the future.")
           (volume-mounts
            :initarg
            :volume-mounts
            :type
            list
            :documentation
            "Pod volumes to mount into the container's filesystem. Cannot be updated.")
           (tty
            :initarg
            :tty
            :type
            (or boolean null)
            :documentation
            "Whether this container should allocate a TTY for itself, also requires 'stdin' to be true. Default is false.")
           (stdin
            :initarg
            :stdin
            :type
            (or boolean null)
            :documentation
            "Whether this container should allocate a buffer for stdin in the container runtime. If this is not set, reads from stdin in the container will always result in EOF. Default is false.")
           (env-from
            :initarg
            :env-from
            :type
            list
            :documentation
            "List of sources to populate environment variables in the container. The keys defined within a source must be a C_IDENTIFIER. All invalid keys will be reported as an event when the container is starting. When a key exists in multiple sources, the value associated with the last source will take precedence. Values defined by an Env with a duplicate key will take precedence. Cannot be updated.")
           (image
            :initarg
            :image
            :type
            (or string null)
            :documentation
            "Docker image name. More info: https://kubernetes.io/docs/concepts/containers/images This field is optional to allow higher level config management to default or override container images in workload controllers like Deployments and StatefulSets.")
           (stdin-once
            :initarg
            :stdin-once
            :type
            (or boolean null)
            :documentation
            "Whether the container runtime should close the stdin channel after it has been opened by a single attach. When stdin is true the stdin stream will remain open across multiple attach sessions. If stdinOnce is set to true, stdin is opened on container start, is empty until the first client attaches to stdin, and then remains open and accepts data until the client disconnects, at which time stdin is closed and remains closed until the container is restarted. If this flag is false, a container processes that reads from stdin will never receive an EOF. Default is false")
           (security-context
            :initarg
            :security-context
            :type
            (or security-context null)
            :documentation
            "Security options the pod should run with. More info: https://kubernetes.io/docs/concepts/policy/security-context/ More info: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/")
           (termination-message-policy
            :initarg
            :termination-message-policy
            :type
            (or string null)
            :documentation
            "Indicate how the termination message should be populated. File will use the contents of terminationMessagePath to populate the container status message on both success and failure. FallbackToLogsOnError will use the last chunk of container log output if the termination message file is empty and the container exited with an error. The log output is limited to 2048 bytes or 80 lines, whichever is smaller. Defaults to File. Cannot be updated.")
           (ports
            :initarg
            :ports
            :type
            list
            :documentation
            "List of ports to expose from the container. Exposing a port here gives the system additional information about the network connections a container uses, but is primarily informational. Not specifying a port here DOES NOT prevent that port from being exposed. Any port which is listening on the default \"0.0.0.0\" address inside a container will be accessible from the network. Cannot be updated.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "Name of the container specified as a DNS_LABEL. Each container in a pod must have a unique name (DNS_LABEL). Cannot be updated.")
           (env
            :initarg
            :env
            :type
            list
            :documentation
            "List of environment variables to set in the container. Cannot be updated.")
           (readiness-probe
            :initarg
            :readiness-probe
            :type
            (or probe null)
            :documentation
            "Periodic probe of container service readiness. Container will be removed from service endpoints if the probe fails. Cannot be updated. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes")
           (image-pull-policy
            :initarg
            :image-pull-policy
            :type
            (or string null)
            :documentation
            "Image pull policy. One of Always, Never, IfNotPresent. Defaults to Always if :latest tag is specified, or IfNotPresent otherwise. Cannot be updated. More info: https://kubernetes.io/docs/concepts/containers/images#updating-images")
           (command
            :initarg
            :command
            :type
            list
            :documentation
            "Entrypoint array. Not executed within a shell. The docker image's ENTRYPOINT is used if this is not provided. Variable references $(VAR_NAME) are expanded using the container's environment. If a variable cannot be resolved, the reference in the input string will be unchanged. The $(VAR_NAME) syntax can be escaped with a double $$, ie: $$(VAR_NAME). Escaped references will never be expanded, regardless of whether the variable exists or not. Cannot be updated. More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell")
           (termination-message-path
            :initarg
            :termination-message-path
            :type
            (or string null)
            :documentation
            "Optional: Path at which the file to which the container's termination message will be written is mounted into the container's filesystem. Message written is intended to be brief final status, such as an assertion failure message. Will be truncated by the node if greater than 4096 bytes. The total message length across all containers will be limited to 12kb. Defaults to /dev/termination-log. Cannot be updated.")
           (args
            :initarg
            :args
            :type
            list
            :documentation
            "Arguments to the entrypoint. The docker image's CMD is used if this is not provided. Variable references $(VAR_NAME) are expanded using the container's environment. If a variable cannot be resolved, the reference in the input string will be unchanged. The $(VAR_NAME) syntax can be escaped with a double $$, ie: $$(VAR_NAME). Escaped references will never be expanded, regardless of whether the variable exists or not. Cannot be updated. More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell")
           (lifecycle
            :initarg
            :lifecycle
            :type
            (or lifecycle null)
            :documentation
            "Actions that the management system should take in response to container lifecycle events. Cannot be updated.")
           (working-dir
            :initarg
            :working-dir
            :type
            (or string null)
            :documentation
            "Container's working directory. If not specified, the container runtime's default will be used, which might be configured in the container image. Cannot be updated."))
          (:documentation
           "A single application container that you want to run within a pod."))

(defmethod unmarshal
  ((object container) source)
  (multiple-value-bind (value present-p)
      (gethash "resources" source)
    (when present-p
      (setf (slot-value object 'resources)
            (decode-object "ResourceRequirements" value))))
  (multiple-value-bind (value present-p)
      (gethash "livenessProbe" source)
    (when present-p
      (setf (slot-value object 'liveness-probe)
            (decode-object "Probe" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeDevices" source)
    (when present-p
      (setf (slot-value object 'volume-devices)
            (decode-object (cons "array" "VolumeDevice") value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeMounts" source)
    (when present-p
      (setf (slot-value object 'volume-mounts)
            (decode-object (cons "array" "VolumeMount") value))))
  (multiple-value-bind (value present-p)
      (gethash "tty" source)
    (when present-p
      (setf (slot-value object 'tty) (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "stdin" source)
    (when present-p
      (setf (slot-value object 'stdin)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "envFrom" source)
    (when present-p
      (setf (slot-value object 'env-from)
            (decode-object (cons "array" "EnvFromSource") value))))
  (multiple-value-bind (value present-p)
      (gethash "image" source)
    (when present-p
      (setf (slot-value object 'image)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "stdinOnce" source)
    (when present-p
      (setf (slot-value object 'stdin-once)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "securityContext" source)
    (when present-p
      (setf (slot-value object 'security-context)
            (decode-object "SecurityContext" value))))
  (multiple-value-bind (value present-p)
      (gethash "terminationMessagePolicy" source)
    (when present-p
      (setf (slot-value object 'termination-message-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "ports" source)
    (when present-p
      (setf (slot-value object 'ports)
            (decode-object (cons "array" "ContainerPort") value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "env" source)
    (when present-p
      (setf (slot-value object 'env)
            (decode-object (cons "array" "EnvVar") value))))
  (multiple-value-bind (value present-p)
      (gethash "readinessProbe" source)
    (when present-p
      (setf (slot-value object 'readiness-probe)
            (decode-object "Probe" value))))
  (multiple-value-bind (value present-p)
      (gethash "imagePullPolicy" source)
    (when present-p
      (setf (slot-value object 'image-pull-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "command" source)
    (when present-p
      (setf (slot-value object 'command)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "terminationMessagePath" source)
    (when present-p
      (setf (slot-value object 'termination-message-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "args" source)
    (when present-p
      (setf (slot-value object 'args)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "lifecycle" source)
    (when present-p
      (setf (slot-value object 'lifecycle)
            (decode-object "Lifecycle" value))))
  (multiple-value-bind (value present-p)
      (gethash "workingDir" source)
    (when present-p
      (setf (slot-value object 'working-dir)
            (decode-object "string" value)))))


(defclass limit-range-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "LimitRangeList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is a list of LimitRange objects. More info: https://kubernetes.io/docs/concepts/configuration/manage-compute-resources-container/")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "LimitRangeList is a list of LimitRange items."))

(defmethod unmarshal
  ((object limit-range-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "LimitRange") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass initializer
          (resource)
          ((name
            :initarg
            :name
            :type
            string
            :documentation
            "name of the process that is responsible for initializing this object."))
          (:documentation
           "Initializer is information about an initializer that has not yet completed."))

(defmethod unmarshal
  ((object initializer) source)
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass delete-options
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "DeleteOptions" :allocation :class)
           (preconditions
            :initarg
            :preconditions
            :type
            (or preconditions null)
            :documentation
            "Must be fulfilled before a deletion is carried out. If not possible, a 409 Conflict status will be returned.")
           (grace-period-seconds
            :initarg
            :grace-period-seconds
            :type
            (or integer null)
            :documentation
            "The duration in seconds before the object should be deleted. Value must be non-negative integer. The value zero indicates delete immediately. If this value is nil, the default grace period for the specified type will be used. Defaults to a per object value if not specified. zero means delete immediately.")
           (propagation-policy
            :initarg
            :propagation-policy
            :type
            (or deletion-propagation null)
            :documentation
            "Whether and how garbage collection will be performed. Either this field or OrphanDependents may be set, but not both. The default policy is decided by the existing finalizer set in the metadata.finalizers and the resource-specific default policy. Acceptable values are: 'Orphan' - orphan the dependents; 'Background' - allow the garbage collector to delete the dependents in the background; 'Foreground' - a cascading policy that deletes all dependents in the foreground.")
           (orphan-dependents
            :initarg
            :orphan-dependents
            :type
            (or boolean null)
            :documentation
            "Deprecated: please use the PropagationPolicy, this field will be deprecated in 1.7. Should the dependent objects be orphaned. If true/false, the \"orphan\" finalizer will be added to/removed from the object's finalizers list. Either this field or PropagationPolicy may be set, but not both."))
          (:documentation
           "DeleteOptions may be provided when deleting an API object."))

(defmethod unmarshal
  ((object delete-options) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "preconditions" source)
    (when present-p
      (setf (slot-value object 'preconditions)
            (decode-object "Preconditions" value))))
  (multiple-value-bind (value present-p)
      (gethash "gracePeriodSeconds" source)
    (when present-p
      (setf (slot-value object 'grace-period-seconds)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "propagationPolicy" source)
    (when present-p
      (setf (slot-value object 'propagation-policy)
            (decode-object "DeletionPropagation" value))))
  (multiple-value-bind (value present-p)
      (gethash "orphanDependents" source)
    (when present-p
      (setf (slot-value object 'orphan-dependents)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value)))))


(defclass downward-api-volume-file
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "Required: Path is  the relative path name of the file to be created. Must not be absolute or contain the '..' path. Must be utf-8 encoded. The first item of the relative path must not start with '..'")
           (mode
            :initarg
            :mode
            :type
            (or integer null)
            :documentation
            "Optional: mode bits to use on this file, must be a value between 0 and 0777. If not specified, the volume defaultMode will be used. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set.")
           (field-ref
            :initarg
            :field-ref
            :type
            (or object-field-selector null)
            :documentation
            "Required: Selects a field of the pod: only annotations, labels, name and namespace are supported.")
           (resource-field-ref
            :initarg
            :resource-field-ref
            :type
            (or resource-field-selector null)
            :documentation
            "Selects a resource of the container: only resources limits and requests (limits.cpu, limits.memory, requests.cpu and requests.memory) are currently supported."))
          (:documentation
           "DownwardAPIVolumeFile represents information to create the file containing the pod field"))

(defmethod unmarshal
  ((object downward-api-volume-file) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "mode" source)
    (when present-p
      (setf (slot-value object 'mode)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "fieldRef" source)
    (when present-p
      (setf (slot-value object 'field-ref)
            (decode-object "ObjectFieldSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "resourceFieldRef" source)
    (when present-p
      (setf (slot-value object 'resource-field-ref)
            (decode-object "ResourceFieldSelector" value)))))


(defclass namespace
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Namespace" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or namespace-status null)
            :documentation
            "Status describes the current status of a Namespace. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (spec
            :initarg
            :spec
            :type
            (or namespace-spec null)
            :documentation
            "Spec defines the behavior of the Namespace. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "Namespace provides a scope for Names. Use of multiple namespaces is optional."))

(defmethod unmarshal
  ((object namespace) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "NamespaceStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "NamespaceSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass config-map-projection
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the ConfigMap or it's keys must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names")
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "If unspecified, each key-value pair in the Data field of the referenced ConfigMap will be projected into the volume as a file whose name is the key and content is the value. If specified, the listed keys will be projected into the specified paths, and unlisted keys will not be present. If a key is specified which is not present in the ConfigMap, the volume setup will error unless it is marked optional. Paths must be relative and may not contain the '..' path or start with '..'."))
          (:documentation
           "Adapts a ConfigMap into a projected volume.

The contents of the target ConfigMap's Data field will be presented in a projected volume as files using the keys in the Data field as the file names, unless the items element is populated with specific mappings of keys to paths. Note that this is identical to a configmap volume source without the default mode."))

(defmethod unmarshal
  ((object config-map-projection) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "KeyToPath") value)))))


(defclass flex-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or local-object-reference null)
            :documentation
            "Optional: SecretRef is reference to the secret object containing sensitive information to pass to the plugin scripts. This may be empty if no secret object is specified. If the secret object contains more than one secret, all secrets are passed to the plugin scripts.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (options
            :initarg
            :options
            :type
            (or hash-table null)
            :documentation
            "Optional: Extra command options if any.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". The default filesystem depends on FlexVolume script.")
           (driver
            :initarg
            :driver
            :type
            string
            :documentation
            "Driver is the name of the driver to use for this volume."))
          (:documentation
           "FlexVolume represents a generic volume resource that is provisioned/attached using an exec based plugin."))

(defmethod unmarshal
  ((object flex-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "options" source)
    (when present-p
      (setf (slot-value object 'options)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "driver" source)
    (when present-p
      (setf (slot-value object 'driver)
            (decode-object "string" value)))))


(defclass endpoint-port
          (resource)
          ((name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "The name of this port (corresponds to ServicePort.Name). Must be a DNS_LABEL. Optional only if one port is defined.")
           (port
            :initarg
            :port
            :type
            integer
            :documentation
            "The port number of the endpoint.")
           (protocol
            :initarg
            :protocol
            :type
            (or string null)
            :documentation
            "The IP protocol for this port. Must be UDP or TCP. Default is TCP."))
          (:documentation
           "EndpointPort is a tuple that describes a single port."))

(defmethod unmarshal
  ((object endpoint-port) source)
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "port" source)
    (when present-p
      (setf (slot-value object 'port)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "protocol" source)
    (when present-p
      (setf (slot-value object 'protocol)
            (decode-object "string" value)))))


(defclass azure-file-persistent-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (share-name
            :initarg
            :share-name
            :type
            string
            :documentation
            "Share Name")
           (secret-name
            :initarg
            :secret-name
            :type
            string
            :documentation
            "the name of secret that contains Azure Storage Account Name and Key")
           (secret-namespace
            :initarg
            :secret-namespace
            :type
            string
            :documentation
            "the namespace of the secret that contains Azure Storage Account Name and Key default is the same as the Pod"))
          (:documentation
           "AzureFile represents an Azure File Service mount on the host and bind mount to the pod."))

(defmethod unmarshal
  ((object azure-file-persistent-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "shareName" source)
    (when present-p
      (setf (slot-value object 'share-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "secretName" source)
    (when present-p
      (setf (slot-value object 'secret-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "secretNamespace" source)
    (when present-p
      (setf (slot-value object 'secret-namespace)
            (decode-object "string" value)))))


(defclass node-selector-requirement
          (resource)
          ((key
            :initarg
            :key
            :type
            string
            :documentation
            "The label key that the selector applies to.")
           (values :initarg
                   :values
                   :type
                   list
                   :documentation
                   "An array of string values. If the operator is In or NotIn, the values array must be non-empty. If the operator is Exists or DoesNotExist, the values array must be empty. If the operator is Gt or Lt, the values array must have a single element, which will be interpreted as an integer. This array is replaced during a strategic merge patch.")
           (operator
            :initarg
            :operator
            :type
            string
            :documentation
            "Represents a key's relationship to a set of values. Valid operators are In, NotIn, Exists, DoesNotExist. Gt, and Lt."))
          (:documentation
           "A node selector requirement is a selector that contains values, a key, and an operator that relates the key and values."))

(defmethod unmarshal
  ((object node-selector-requirement) source)
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "values" source)
    (when present-p
      (setf (slot-value object 'values)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "operator" source)
    (when present-p
      (setf (slot-value object 'operator)
            (decode-object "string" value)))))


(defclass service-port
          (resource)
          ((target-port
            :initarg
            :target-port
            :type
            (or string null)
            :documentation
            "Number or name of the port to access on the pods targeted by the service. Number must be in the range 1 to 65535. Name must be an IANA_SVC_NAME. If this is a string, it will be looked up as a named port in the target Pod's container ports. If this is not specified, the value of the 'port' field is used (an identity map). This field is ignored for services with clusterIP=None, and should be omitted or set equal to the 'port' field. More info: https://kubernetes.io/docs/concepts/services-networking/service/#defining-a-service")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "The name of this port within the service. This must be a DNS_LABEL. All ports within a ServiceSpec must have unique names. This maps to the 'Name' field in EndpointPort objects. Optional if only one ServicePort is defined on this service.")
           (node-port
            :initarg
            :node-port
            :type
            (or integer null)
            :documentation
            "The port on each node on which this service is exposed when type=NodePort or LoadBalancer. Usually assigned by the system. If specified, it will be allocated to the service if unused or else creation of the service will fail. Default is to auto-allocate a port if the ServiceType of this Service requires one. More info: https://kubernetes.io/docs/concepts/services-networking/service/#type-nodeport")
           (port
            :initarg
            :port
            :type
            integer
            :documentation
            "The port that will be exposed by this service.")
           (protocol
            :initarg
            :protocol
            :type
            (or string null)
            :documentation
            "The IP protocol for this port. Supports \"TCP\" and \"UDP\". Default is TCP."))
          (:documentation
           "ServicePort contains information on service's port."))

(defmethod unmarshal
  ((object service-port) source)
  (multiple-value-bind (value present-p)
      (gethash "targetPort" source)
    (when present-p
      (setf (slot-value object 'target-port)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "nodePort" source)
    (when present-p
      (setf (slot-value object 'node-port)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "port" source)
    (when present-p
      (setf (slot-value object 'port)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "protocol" source)
    (when present-p
      (setf (slot-value object 'protocol)
            (decode-object "string" value)))))


(defclass list-meta
          (resource)
          ((self-link
            :initarg
            :self-link
            :type
            (or string null)
            :documentation
            "selfLink is a URL representing this object. Populated by the system. Read-only.")
           (continue :initarg
                     :continue
                     :type
                     (or string null)
                     :documentation
                     "continue may be set if the user set a limit on the number of items returned, and indicates that the server has more data available. The value is opaque and may be used to issue another request to the endpoint that served this list to retrieve the next set of available objects. Continuing a list may not be possible if the server configuration has changed or more than a few minutes have passed. The resourceVersion field returned when using this continue value will be identical to the value in the first response.")
           (resource-version
            :initarg
            :resource-version
            :type
            (or string null)
            :documentation
            "String that identifies the server's internal version of this object that can be used by clients to determine when objects have changed. Value must be treated as opaque by clients and passed unmodified back to the server. Populated by the system. Read-only. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#concurrency-control-and-consistency"))
          (:documentation
           "ListMeta describes metadata that synthetic resources must have, including lists and various status objects. A resource may have only one of {ObjectMeta, ListMeta}."))

(defmethod unmarshal
  ((object list-meta) source)
  (multiple-value-bind (value present-p)
      (gethash "selfLink" source)
    (when present-p
      (setf (slot-value object 'self-link)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "continue" source)
    (when present-p
      (setf (slot-value object 'continue)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "resourceVersion" source)
    (when present-p
      (setf (slot-value object 'resource-version)
            (decode-object "string" value)))))


(defclass pod-security-context
          (resource)
          ((run-as-non-root
            :initarg
            :run-as-non-root
            :type
            (or boolean null)
            :documentation
            "Indicates that the container must run as a non-root user. If true, the Kubelet will validate the image at runtime to ensure that it does not run as UID 0 (root) and fail to start the container if it does. If unset or false, no such validation will be performed. May also be set in SecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence.")
           (fs-group
            :initarg
            :fs-group
            :type
            (or integer null)
            :documentation
            "A special supplemental group that applies to all containers in a pod. Some volume types allow the Kubelet to change the ownership of that volume to be owned by the pod:

1. The owning GID will be the FSGroup 2. The setgid bit is set (new files created in the volume will be owned by FSGroup) 3. The permission bits are OR'd with rw-rw ")
           (run-as-group
            :initarg
            :run-as-group
            :type
            (or integer null)
            :documentation
            "The GID to run the entrypoint of the container process. Uses runtime default if unset. May also be set in SecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence for that container.")
           (se-linux-options
            :initarg
            :se-linux-options
            :type
            (or se-linux-options null)
            :documentation
            "The SELinux context to be applied to all containers. If unspecified, the container runtime will allocate a random SELinux context for each container.  May also be set in SecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence for that container.")
           (supplemental-groups
            :initarg
            :supplemental-groups
            :type
            list
            :documentation
            "A list of groups applied to the first process run in each container, in addition to the container's primary GID.  If unspecified, no groups will be added to any container.")
           (run-as-user
            :initarg
            :run-as-user
            :type
            (or integer null)
            :documentation
            "The UID to run the entrypoint of the container process. Defaults to user specified in image metadata if unspecified. May also be set in SecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence for that container."))
          (:documentation
           "PodSecurityContext holds pod-level security attributes and common container settings. Some fields are also present in container.securityContext.  Field values of container.securityContext take precedence over field values of PodSecurityContext."))

(defmethod unmarshal
  ((object pod-security-context) source)
  (multiple-value-bind (value present-p)
      (gethash "runAsNonRoot" source)
    (when present-p
      (setf (slot-value object 'run-as-non-root)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsGroup" source)
    (when present-p
      (setf (slot-value object 'fs-group)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "runAsGroup" source)
    (when present-p
      (setf (slot-value object 'run-as-group)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "seLinuxOptions" source)
    (when present-p
      (setf (slot-value object 'se-linux-options)
            (decode-object "SELinuxOptions" value))))
  (multiple-value-bind (value present-p)
      (gethash "supplementalGroups" source)
    (when present-p
      (setf (slot-value object 'supplemental-groups)
            (decode-object (cons "array" "integer") value))))
  (multiple-value-bind (value present-p)
      (gethash "runAsUser" source)
    (when present-p
      (setf (slot-value object 'run-as-user)
            (decode-object (cons "integer" "int64") value)))))


(defclass status
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Status" :allocation :class)
           (message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human-readable description of the status of this operation.")
           (status
            :initarg
            :status
            :type
            (or string null)
            :documentation
            "Status of the operation. One of: \"Success\" or \"Failure\". More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (code
            :initarg
            :code
            :type
            (or integer null)
            :documentation
            "Suggested HTTP return code for this status, 0 if not set.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "A machine-readable description of why this operation is in the \"Failure\" status. If this value is empty there is no information available. A Reason clarifies an HTTP status code but does not override it.")
           (details
            :initarg
            :details
            :type
            (or status-details null)
            :documentation
            "Extended data associated with the reason.  Each reason may define its own extended details. This field is optional and the data returned is not guaranteed to conform to any schema except that defined by the reason type.")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "Status is a return value for calls that don't return other objects."))

(defmethod unmarshal
  ((object status) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "code" source)
    (when present-p
      (setf (slot-value object 'code)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "details" source)
    (when present-p
      (setf (slot-value object 'details)
            (decode-object "StatusDetails" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass node
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Node" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or node-status null)
            :documentation
            "Most recently observed status of the node. Populated by the system. Read-only. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (spec
            :initarg
            :spec
            :type
            (or node-spec null)
            :documentation
            "Spec defines the behavior of a node. https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "Node is a worker node in Kubernetes. Each node will have a unique identifier in the cache (i.e. in etcd)."))

(defmethod unmarshal
  ((object node) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "NodeStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "NodeSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass lifecycle
          (resource)
          ((pre-stop
            :initarg
            :pre-stop
            :type
            (or handler null)
            :documentation
            "PreStop is called immediately before a container is terminated. The container is terminated after the handler completes. The reason for termination is passed to the handler. Regardless of the outcome of the handler, the container is eventually terminated. Other management of the container blocks until the hook completes. More info: https://kubernetes.io/docs/concepts/containers/container-lifecycle-hooks/#container-hooks")
           (post-start
            :initarg
            :post-start
            :type
            (or handler null)
            :documentation
            "PostStart is called immediately after a container is created. If the handler fails, the container is terminated and restarted according to its restart policy. Other management of the container blocks until the hook completes. More info: https://kubernetes.io/docs/concepts/containers/container-lifecycle-hooks/#container-hooks"))
          (:documentation
           "Lifecycle describes actions that the management system should take in response to container lifecycle events. For the PostStart and PreStop lifecycle handlers, management of the container blocks until the action is complete, unless the container process fails, in which case the handler is aborted."))

(defmethod unmarshal
  ((object lifecycle) source)
  (multiple-value-bind (value present-p)
      (gethash "preStop" source)
    (when present-p
      (setf (slot-value object 'pre-stop)
            (decode-object "Handler" value))))
  (multiple-value-bind (value present-p)
      (gethash "postStart" source)
    (when present-p
      (setf (slot-value object 'post-start)
            (decode-object "Handler" value)))))


(defclass persistent-volume-claim-spec
          (resource)
          ((resources
            :initarg
            :resources
            :type
            (or resource-requirements null)
            :documentation
            "Resources represents the minimum resources the volume should have. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#resources")
           (storage-class-name
            :initarg
            :storage-class-name
            :type
            (or string null)
            :documentation
            "Name of the StorageClass required by the claim. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#class-1")
           (volume-mode
            :initarg
            :volume-mode
            :type
            (or persistent-volume-mode null)
            :documentation
            "volumeMode defines what type of volume is required by the claim. Value of Filesystem is implied when not included in claim spec. This is an alpha feature and may change in the future.")
           (selector
            :initarg
            :selector
            :type
            (or label-selector null)
            :documentation
            "A label query over volumes to consider for binding.")
           (volume-name
            :initarg
            :volume-name
            :type
            (or string null)
            :documentation
            "VolumeName is the binding reference to the PersistentVolume backing this claim.")
           (access-modes
            :initarg
            :access-modes
            :type
            list
            :documentation
            "AccessModes contains the desired access modes the volume should have. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#access-modes-1"))
          (:documentation
           "PersistentVolumeClaimSpec describes the common attributes of storage devices and allows a Source for provider-specific attributes"))

(defmethod unmarshal
  ((object persistent-volume-claim-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "resources" source)
    (when present-p
      (setf (slot-value object 'resources)
            (decode-object "ResourceRequirements" value))))
  (multiple-value-bind (value present-p)
      (gethash "storageClassName" source)
    (when present-p
      (setf (slot-value object 'storage-class-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeMode" source)
    (when present-p
      (setf (slot-value object 'volume-mode)
            (decode-object "PersistentVolumeMode" value))))
  (multiple-value-bind (value present-p)
      (gethash "selector" source)
    (when present-p
      (setf (slot-value object 'selector)
            (decode-object "LabelSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeName" source)
    (when present-p
      (setf (slot-value object 'volume-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "accessModes" source)
    (when present-p
      (setf (slot-value object 'access-modes)
            (decode-object
             (cons "array" "PersistentVolumeAccessMode")
             value)))))


(defclass ceph-fs-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or local-object-reference null)
            :documentation
            "Optional: SecretRef is reference to the authentication secret for User, default is empty. More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (monitors
            :initarg
            :monitors
            :type
            list
            :documentation
            "Required: Monitors is a collection of Ceph monitors More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (path
            :initarg
            :path
            :type
            (or string null)
            :documentation
            "Optional: Used as the mounted root, rather than the full Ceph tree, default is /")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts. More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (user
            :initarg
            :user
            :type
            (or string null)
            :documentation
            "Optional: User is the rados user name, default is admin More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (secret-file
            :initarg
            :secret-file
            :type
            (or string null)
            :documentation
            "Optional: SecretFile is the path to key ring for User, default is /etc/ceph/user.secret More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it"))
          (:documentation
           "Represents a Ceph Filesystem mount that lasts the lifetime of a pod Cephfs volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object ceph-fs-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "monitors" source)
    (when present-p
      (setf (slot-value object 'monitors)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "user" source)
    (when present-p
      (setf (slot-value object 'user) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "secretFile" source)
    (when present-p
      (setf (slot-value object 'secret-file)
            (decode-object "string" value)))))


(defclass resource-requirements
          (resource)
          ((requests
            :initarg
            :requests
            :type
            (or hash-table null)
            :documentation
            "Requests describes the minimum amount of compute resources required. If Requests is omitted for a container, it defaults to Limits if that is explicitly specified, otherwise to an implementation-defined value. More info: https://kubernetes.io/docs/concepts/configuration/manage-compute-resources-container/")
           (limits
            :initarg
            :limits
            :type
            (or hash-table null)
            :documentation
            "Limits describes the maximum amount of compute resources allowed. More info: https://kubernetes.io/docs/concepts/configuration/manage-compute-resources-container/"))
          (:documentation
           "ResourceRequirements describes the compute resource requirements."))

(defmethod unmarshal
  ((object resource-requirements) source)
  (multiple-value-bind (value present-p)
      (gethash "requests" source)
    (when present-p
      (setf (slot-value object 'requests)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "limits" source)
    (when present-p
      (setf (slot-value object 'limits)
            (decode-object "object" value)))))


(defclass node-selector
          (resource)
          ((node-selector-terms
            :initarg
            :node-selector-terms
            :type
            list
            :documentation
            "Required. A list of node selector terms. The terms are ORed."))
          (:documentation
           "A node selector represents the union of the results of one or more label queries over a set of nodes; that is, it represents the OR of the selectors represented by the node selector terms."))

(defmethod unmarshal
  ((object node-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "nodeSelectorTerms" source)
    (when present-p
      (setf (slot-value object 'node-selector-terms)
            (decode-object (cons "array" "NodeSelectorTerm") value)))))


(defclass limit-range-spec
          (resource)
          ((limits
            :initarg
            :limits
            :type
            list
            :documentation
            "Limits is the list of LimitRangeItem objects that are enforced."))
          (:documentation
           "LimitRangeSpec defines a min/max usage limit for resources that match on kind."))

(defmethod unmarshal
  ((object limit-range-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "limits" source)
    (when present-p
      (setf (slot-value object 'limits)
            (decode-object (cons "array" "LimitRangeItem") value)))))


(defclass resource-quota-status
          (resource)
          ((used
            :initarg
            :used
            :type
            (or hash-table null)
            :documentation
            "Used is the current observed total usage of the resource in the namespace.")
           (hard
            :initarg
            :hard
            :type
            (or hash-table null)
            :documentation
            "Hard is the set of enforced hard limits for each named resource. More info: https://kubernetes.io/docs/concepts/policy/resource-quotas/"))
          (:documentation
           "ResourceQuotaStatus defines the enforced hard limits and observed use."))

(defmethod unmarshal
  ((object resource-quota-status) source)
  (multiple-value-bind (value present-p)
      (gethash "used" source)
    (when present-p
      (setf (slot-value object 'used) (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "hard" source)
    (when present-p
      (setf (slot-value object 'hard) (decode-object "object" value)))))


(defclass preconditions
          (resource)
          ((uid
            :initarg
            :uid
            :type
            (or uid null)
            :documentation
            "Specifies the target UID."))
          (:documentation
           "Preconditions must be fulfilled before an operation (update, delete, etc.) is carried out."))

(defmethod unmarshal
  ((object preconditions) source)
  (multiple-value-bind (value present-p)
      (gethash "uid" source)
    (when present-p
      (setf (slot-value object 'uid) (decode-object "UID" value)))))


(defclass status-cause
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human-readable description of the cause of the error.  This field may be presented as-is to a reader.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "A machine-readable description of the cause of the error. If this value is empty there is no information available.")
           (field
            :initarg
            :field
            :type
            (or string null)
            :documentation
            "The field of the resource that has caused this error, as named by its JSON serialization. May include dot and postfix notation for nested attributes. Arrays are zero-indexed.  Fields may appear more than once in an array of causes due to fields having multiple errors. Optional.

Examples:
  \"name\" - the field \"name\" on the current resource
  \"items[0].name\" - the field \"name\" on the first array entry in \"items\""))
          (:documentation
           "StatusCause provides more information about an api.Status failure, including cases when multiple errors are encountered."))

(defmethod unmarshal
  ((object status-cause) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "field" source)
    (when present-p
      (setf (slot-value object 'field)
            (decode-object "string" value)))))


(defclass pod
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Pod" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or pod-status null)
            :documentation
            "Most recently observed status of the pod. This data may not be up to date. Populated by the system. Read-only. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (spec
            :initarg
            :spec
            :type
            (or pod-spec null)
            :documentation
            "Specification of the desired behavior of the pod. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "Pod is a collection of containers that can run on a host. This resource is created by clients and scheduled onto hosts."))

(defmethod unmarshal
  ((object pod) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "PodStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "PodSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass downward-api-volume-source
          (resource)
          ((default-mode
            :initarg
            :default-mode
            :type
            (or integer null)
            :documentation
            "Optional: mode bits to use on created files by default. Must be a value between 0 and 0777. Defaults to 0644. Directories within the path are not affected by this setting. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set.")
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is a list of downward API volume file"))
          (:documentation
           "DownwardAPIVolumeSource represents a volume containing downward API info. Downward API volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object downward-api-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "defaultMode" source)
    (when present-p
      (setf (slot-value object 'default-mode)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object
             (cons "array" "DownwardAPIVolumeFile")
             value)))))


(defclass preferred-scheduling-term
          (resource)
          ((preference
            :initarg
            :preference
            :type
            node-selector-term
            :documentation
            "A node selector term, associated with the corresponding weight.")
           (weight
            :initarg
            :weight
            :type
            integer
            :documentation
            "Weight associated with matching the corresponding nodeSelectorTerm, in the range 1-100."))
          (:documentation
           "An empty preferred scheduling term matches all objects with implicit weight 0 (i.e. it's a no-op). A null preferred scheduling term matches no objects (i.e. is also a no-op)."))

(defmethod unmarshal
  ((object preferred-scheduling-term) source)
  (multiple-value-bind (value present-p)
      (gethash "preference" source)
    (when present-p
      (setf (slot-value object 'preference)
            (decode-object "NodeSelectorTerm" value))))
  (multiple-value-bind (value present-p)
      (gethash "weight" source)
    (when present-p
      (setf (slot-value object 'weight)
            (decode-object (cons "integer" "int32") value)))))


(defclass owner-reference
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "OwnerReference" :allocation :class)
           (block-owner-deletion
            :initarg
            :block-owner-deletion
            :type
            (or boolean null)
            :documentation
            "If true, AND if the owner has the \"foregroundDeletion\" finalizer, then the owner cannot be deleted from the key-value store until this reference is removed. Defaults to false. To set this field, a user needs \"delete\" permission of the owner, otherwise 422 (Unprocessable Entity) will be returned.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "Name of the referent. More info: http://kubernetes.io/docs/user-guide/identifiers#names")
           (controller
            :initarg
            :controller
            :type
            (or boolean null)
            :documentation
            "If true, this reference points to the managing controller.")
           (uid
            :initarg
            :uid
            :type
            string
            :documentation
            "UID of the referent. More info: http://kubernetes.io/docs/user-guide/identifiers#uids"))
          (:documentation
           "OwnerReference contains enough information to let you identify an owning object. Currently, an owning object must be in the same namespace, so there is no namespace field."))

(defmethod unmarshal
  ((object owner-reference) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "blockOwnerDeletion" source)
    (when present-p
      (setf (slot-value object 'block-owner-deletion)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "controller" source)
    (when present-p
      (setf (slot-value object 'controller)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "uid" source)
    (when present-p
      (setf (slot-value object 'uid) (decode-object "string" value)))))


(defclass event-source
          (resource)
          ((host
            :initarg
            :host
            :type
            (or string null)
            :documentation
            "Node name on which the event is generated.")
           (component
            :initarg
            :component
            :type
            (or string null)
            :documentation
            "Component from which the event is generated."))
          (:documentation
           "EventSource contains information for an event."))

(defmethod unmarshal
  ((object event-source) source)
  (multiple-value-bind (value present-p)
      (gethash "host" source)
    (when present-p
      (setf (slot-value object 'host) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "component" source)
    (when present-p
      (setf (slot-value object 'component)
            (decode-object "string" value)))))


(defclass scale
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Scale" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or scale-status null)
            :documentation
            "current status of the scale. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status. Read-only.")
           (spec
            :initarg
            :spec
            :type
            (or scale-spec null)
            :documentation
            "defines the behavior of the scale. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status.")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object metadata; More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata."))
          (:documentation
           "Scale represents a scaling request for a resource."))

(defmethod unmarshal
  ((object scale) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "ScaleStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "ScaleSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass host-path-volume-source
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "Path of the directory on the host. If the path is a symlink, it will follow the link to the real path. More info: https://kubernetes.io/docs/concepts/storage/volumes#hostpath")
           (type
            :initarg
            :type
            :type
            (or host-path-type null)
            :documentation
            "Type for HostPath Volume Defaults to \"\" More info: https://kubernetes.io/docs/concepts/storage/volumes#hostpath"))
          (:documentation
           "Represents a host path mapped into a pod. Host path volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object host-path-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type)
            (decode-object "HostPathType" value)))))


(defclass namespace-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "NamespaceList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is the list of Namespace objects in the list. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/namespaces/")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation "NamespaceList is a list of Namespaces."))

(defmethod unmarshal
  ((object namespace-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "Namespace") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass attached-volume
          (resource)
          ((device-path
            :initarg
            :device-path
            :type
            string
            :documentation
            "DevicePath represents the device path where the volume should be available")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "Name of the attached volume"))
          (:documentation
           "AttachedVolume describes a volume attached to a node"))

(defmethod unmarshal
  ((object attached-volume) source)
  (multiple-value-bind (value present-p)
      (gethash "devicePath" source)
    (when present-p
      (setf (slot-value object 'device-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass replication-controller
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ReplicationController" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or replication-controller-status null)
            :documentation
            "Status is the most recently observed status of the replication controller. This data may be out of date by some window of time. Populated by the system. Read-only. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (spec
            :initarg
            :spec
            :type
            (or replication-controller-spec null)
            :documentation
            "Spec defines the specification of the desired behavior of the replication controller. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "If the Labels of a ReplicationController are empty, they are defaulted to be the same as the Pod(s) that the replication controller manages. Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "ReplicationController represents the configuration of a replication controller."))

(defmethod unmarshal
  ((object replication-controller) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "ReplicationControllerStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "ReplicationControllerSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass volume-projection
          (resource)
          ((config-map
            :initarg
            :config-map
            :type
            (or config-map-projection null)
            :documentation
            "information about the configMap data to project")
           (downward-api
            :initarg
            :downward-api
            :type
            (or downward-api-projection null)
            :documentation
            "information about the downwardAPI data to project")
           (secret
            :initarg
            :secret
            :type
            (or secret-projection null)
            :documentation
            "information about the secret data to project"))
          (:documentation
           "Projection that may be projected along with other supported volume types"))

(defmethod unmarshal
  ((object volume-projection) source)
  (multiple-value-bind (value present-p)
      (gethash "configMap" source)
    (when present-p
      (setf (slot-value object 'config-map)
            (decode-object "ConfigMapProjection" value))))
  (multiple-value-bind (value present-p)
      (gethash "downwardAPI" source)
    (when present-p
      (setf (slot-value object 'downward-api)
            (decode-object "DownwardAPIProjection" value))))
  (multiple-value-bind (value present-p)
      (gethash "secret" source)
    (when present-p
      (setf (slot-value object 'secret)
            (decode-object "SecretProjection" value)))))


(defclass service-spec
          (resource)
          ((session-affinity-config
            :initarg
            :session-affinity-config
            :type
            (or session-affinity-config null)
            :documentation
            "sessionAffinityConfig contains the configurations of session affinity.")
           (type
            :initarg
            :type
            :type
            (or string null)
            :documentation
            "type determines how the Service is exposed. Defaults to ClusterIP. Valid options are ExternalName, ClusterIP, NodePort, and LoadBalancer. \"ExternalName\" maps to the specified externalName. \"ClusterIP\" allocates a cluster-internal IP address for load-balancing to endpoints. Endpoints are determined by the selector or if that is not specified, by manual construction of an Endpoints object. If clusterIP is \"None\", no virtual IP is allocated and the endpoints are published as a set of endpoints rather than a stable IP. \"NodePort\" builds on ClusterIP and allocates a port on every node which routes to the clusterIP. \"LoadBalancer\" builds on NodePort and creates an external load-balancer (if supported in the current cloud) which routes to the clusterIP. More info: https://kubernetes.io/docs/concepts/services-networking/service/#publishing-services ")
           (load-balancer-source-ranges
            :initarg
            :load-balancer-source-ranges
            :type
            list
            :documentation
            "If specified and supported by the platform, this will restrict traffic through the cloud-provider load-balancer will be restricted to the specified client IPs. This field will be ignored if the cloud-provider does not support the feature.\" More info: https://kubernetes.io/docs/tasks/access-application-cluster/configure-cloud-provider-firewall/")
           (selector
            :initarg
            :selector
            :type
            (or hash-table null)
            :documentation
            "Route service traffic to pods with label keys and values matching this selector. If empty or not present, the service is assumed to have an external process managing its endpoints, which Kubernetes will not modify. Only applies to types ClusterIP, NodePort, and LoadBalancer. Ignored if type is ExternalName. More info: https://kubernetes.io/docs/concepts/services-networking/service/")
           (ports
            :initarg
            :ports
            :type
            list
            :documentation
            "The list of ports that are exposed by this service. More info: https://kubernetes.io/docs/concepts/services-networking/service/#virtual-ips-and-service-proxies")
           (publish-not-ready-addresses
            :initarg
            :publish-not-ready-addresses
            :type
            (or boolean null)
            :documentation
            "publishNotReadyAddresses, when set to true, indicates that DNS implementations must publish the notReadyAddresses of subsets for the Endpoints associated with the Service. The default value is false. The primary use case for setting this field is to use a StatefulSet's Headless Service to propagate SRV records for its Pods without respect to their readiness for purpose of peer discovery. This field will replace the service.alpha.kubernetes.io/tolerate-unready-endpoints when that annotation is deprecated and all clients have been converted to use this field.")
           (external-i-ps
            :initarg
            :external-i-ps
            :type
            list
            :documentation
            "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.")
           (session-affinity
            :initarg
            :session-affinity
            :type
            (or string null)
            :documentation
            "Supports \"ClientIP\" and \"None\". Used to maintain session affinity. Enable client IP based session affinity. Must be ClientIP or None. Defaults to None. More info: https://kubernetes.io/docs/concepts/services-networking/service/#virtual-ips-and-service-proxies")
           (external-traffic-policy
            :initarg
            :external-traffic-policy
            :type
            (or string null)
            :documentation
            "externalTrafficPolicy denotes if this Service desires to route external traffic to node-local or cluster-wide endpoints. \"Local\" preserves the client source IP and avoids a second hop for LoadBalancer and Nodeport type services, but risks potentially imbalanced traffic spreading. \"Cluster\" obscures the client source IP and may cause a second hop to another node, but should have good overall load-spreading.")
           (load-balancer-ip
            :initarg
            :load-balancer-ip
            :type
            (or string null)
            :documentation
            "Only applies to Service Type: LoadBalancer LoadBalancer will get created with the IP specified in this field. This feature depends on whether the underlying cloud-provider supports specifying the loadBalancerIP when a load balancer is created. This field will be ignored if the cloud-provider does not support the feature.")
           (cluster-ip
            :initarg
            :cluster-ip
            :type
            (or string null)
            :documentation
            "clusterIP is the IP address of the service and is usually assigned randomly by the master. If an address is specified manually and is not in use by others, it will be allocated to the service; otherwise, creation of the service will fail. This field can not be changed through updates. Valid values are \"None\", empty string (\"\"), or a valid IP address. \"None\" can be specified for headless services when proxying is not required. Only applies to types ClusterIP, NodePort, and LoadBalancer. Ignored if type is ExternalName. More info: https://kubernetes.io/docs/concepts/services-networking/service/#virtual-ips-and-service-proxies")
           (health-check-node-port
            :initarg
            :health-check-node-port
            :type
            (or integer null)
            :documentation
            "healthCheckNodePort specifies the healthcheck nodePort for the service. If not specified, HealthCheckNodePort is created by the service api backend with the allocated nodePort. Will use user-specified nodePort value if specified by the client. Only effects when Type is set to LoadBalancer and ExternalTrafficPolicy is set to Local.")
           (external-name
            :initarg
            :external-name
            :type
            (or string null)
            :documentation
            "externalName is the external reference that kubedns or equivalent will return as a CNAME record for this service. No proxying will be involved. Must be a valid RFC-1123 hostname (https://tools.ietf.org/html/rfc1123) and requires Type to be ExternalName."))
          (:documentation
           "ServiceSpec describes the attributes that a user creates on a service."))

(defmethod unmarshal
  ((object service-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "sessionAffinityConfig" source)
    (when present-p
      (setf (slot-value object 'session-affinity-config)
            (decode-object "SessionAffinityConfig" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "loadBalancerSourceRanges" source)
    (when present-p
      (setf (slot-value object 'load-balancer-source-ranges)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "selector" source)
    (when present-p
      (setf (slot-value object 'selector)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "ports" source)
    (when present-p
      (setf (slot-value object 'ports)
            (decode-object (cons "array" "ServicePort") value))))
  (multiple-value-bind (value present-p)
      (gethash "publishNotReadyAddresses" source)
    (when present-p
      (setf (slot-value object 'publish-not-ready-addresses)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "externalIPs" source)
    (when present-p
      (setf (slot-value object 'external-i-ps)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "sessionAffinity" source)
    (when present-p
      (setf (slot-value object 'session-affinity)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "externalTrafficPolicy" source)
    (when present-p
      (setf (slot-value object 'external-traffic-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "loadBalancerIP" source)
    (when present-p
      (setf (slot-value object 'load-balancer-ip)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "clusterIP" source)
    (when present-p
      (setf (slot-value object 'cluster-ip)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "healthCheckNodePort" source)
    (when present-p
      (setf (slot-value object 'health-check-node-port)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "externalName" source)
    (when present-p
      (setf (slot-value object 'external-name)
            (decode-object "string" value)))))


(defclass load-balancer-ingress
          (resource)
          ((hostname
            :initarg
            :hostname
            :type
            (or string null)
            :documentation
            "Hostname is set for load-balancer ingress points that are DNS based (typically AWS load-balancers)")
           (ip
            :initarg
            :ip
            :type
            (or string null)
            :documentation
            "IP is set for load-balancer ingress points that are IP based (typically GCE or OpenStack load-balancers)"))
          (:documentation
           "LoadBalancerIngress represents the status of a load-balancer ingress point: traffic intended for the service should be sent to an ingress point."))

(defmethod unmarshal
  ((object load-balancer-ingress) source)
  (multiple-value-bind (value present-p)
      (gethash "hostname" source)
    (when present-p
      (setf (slot-value object 'hostname)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "ip" source)
    (when present-p
      (setf (slot-value object 'ip) (decode-object "string" value)))))


(defclass weighted-pod-affinity-term
          (resource)
          ((pod-affinity-term
            :initarg
            :pod-affinity-term
            :type
            pod-affinity-term
            :documentation
            "Required. A pod affinity term, associated with the corresponding weight.")
           (weight
            :initarg
            :weight
            :type
            integer
            :documentation
            "weight associated with matching the corresponding podAffinityTerm, in the range 1-100."))
          (:documentation
           "The weights of all of the matched WeightedPodAffinityTerm fields are added per-node to find the most preferred node(s)"))

(defmethod unmarshal
  ((object weighted-pod-affinity-term) source)
  (multiple-value-bind (value present-p)
      (gethash "podAffinityTerm" source)
    (when present-p
      (setf (slot-value object 'pod-affinity-term)
            (decode-object "PodAffinityTerm" value))))
  (multiple-value-bind (value present-p)
      (gethash "weight" source)
    (when present-p
      (setf (slot-value object 'weight)
            (decode-object (cons "integer" "int32") value)))))


(defclass secret-key-selector
          (resource)
          ((key
            :initarg
            :key
            :type
            string
            :documentation
            "The key of the secret to select from.  Must be a valid secret key.")
           (optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the Secret or it's key must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names"))
          (:documentation
           "SecretKeySelector selects a key of a Secret."))

(defmethod unmarshal
  ((object secret-key-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass env-var-source
          (resource)
          ((secret-key-ref
            :initarg
            :secret-key-ref
            :type
            (or secret-key-selector null)
            :documentation
            "Selects a key of a secret in the pod's namespace")
           (config-map-key-ref
            :initarg
            :config-map-key-ref
            :type
            (or config-map-key-selector null)
            :documentation
            "Selects a key of a ConfigMap.")
           (field-ref
            :initarg
            :field-ref
            :type
            (or object-field-selector null)
            :documentation
            "Selects a field of the pod: supports metadata.name, metadata.namespace, metadata.labels, metadata.annotations, spec.nodeName, spec.serviceAccountName, status.hostIP, status.podIP.")
           (resource-field-ref
            :initarg
            :resource-field-ref
            :type
            (or resource-field-selector null)
            :documentation
            "Selects a resource of the container: only resources limits and requests (limits.cpu, limits.memory, limits.ephemeral-storage, requests.cpu, requests.memory and requests.ephemeral-storage) are currently supported."))
          (:documentation
           "EnvVarSource represents a source for the value of an EnvVar."))

(defmethod unmarshal
  ((object env-var-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretKeyRef" source)
    (when present-p
      (setf (slot-value object 'secret-key-ref)
            (decode-object "SecretKeySelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "configMapKeyRef" source)
    (when present-p
      (setf (slot-value object 'config-map-key-ref)
            (decode-object "ConfigMapKeySelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "fieldRef" source)
    (when present-p
      (setf (slot-value object 'field-ref)
            (decode-object "ObjectFieldSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "resourceFieldRef" source)
    (when present-p
      (setf (slot-value object 'resource-field-ref)
            (decode-object "ResourceFieldSelector" value)))))


(defclass limit-range-item
          (resource)
          ((default-request
            :initarg
            :default-request
            :type
            (or hash-table null)
            :documentation
            "DefaultRequest is the default resource requirement request value by resource name if resource request is omitted.")
           (min :initarg
                :min
                :type
                (or hash-table null)
                :documentation
                "Min usage constraints on this kind by resource name.")
           (max :initarg
                :max
                :type
                (or hash-table null)
                :documentation
                "Max usage constraints on this kind by resource name.")
           (max-limit-request-ratio
            :initarg
            :max-limit-request-ratio
            :type
            (or hash-table null)
            :documentation
            "MaxLimitRequestRatio if specified, the named resource must have a request and limit that are both non-zero where limit divided by request is less than or equal to the enumerated value; this represents the max burst for the named resource.")
           (type
            :initarg
            :type
            :type
            (or string null)
            :documentation
            "Type of resource that this limit applies to.")
           (default
            :initarg
            :default
            :type
            (or hash-table null)
            :documentation
            "Default resource requirement limit value by resource name if resource limit is omitted."))
          (:documentation
           "LimitRangeItem defines a min/max usage limit for any resource that matches on kind."))

(defmethod unmarshal
  ((object limit-range-item) source)
  (multiple-value-bind (value present-p)
      (gethash "defaultRequest" source)
    (when present-p
      (setf (slot-value object 'default-request)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "min" source)
    (when present-p
      (setf (slot-value object 'min) (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "max" source)
    (when present-p
      (setf (slot-value object 'max) (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "maxLimitRequestRatio" source)
    (when present-p
      (setf (slot-value object 'max-limit-request-ratio)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "default" source)
    (when present-p
      (setf (slot-value object 'default)
            (decode-object "object" value)))))


(defclass node-affinity
          (resource)
          ((preferred-during-scheduling-ignored-during-execution
            :initarg
            :preferred-during-scheduling-ignored-during-execution
            :type
            list
            :documentation
            "The scheduler will prefer to schedule pods to nodes that satisfy the affinity expressions specified by this field, but it may choose a node that violates one or more of the expressions. The node that is most preferred is the one with the greatest sum of weights, i.e. for each node that meets all of the scheduling requirements (resource request, requiredDuringScheduling affinity expressions, etc.), compute a sum by iterating through the elements of this field and adding \"weight\" to the sum if the node matches the corresponding matchExpressions; the node(s) with the highest sum are the most preferred.")
           (required-during-scheduling-ignored-during-execution
            :initarg
            :required-during-scheduling-ignored-during-execution
            :type
            (or node-selector null)
            :documentation
            "If the affinity requirements specified by this field are not met at scheduling time, the pod will not be scheduled onto the node. If the affinity requirements specified by this field cease to be met at some point during pod execution (e.g. due to an update), the system may or may not try to eventually evict the pod from its node."))
          (:documentation
           "Node affinity is a group of node affinity scheduling rules."))

(defmethod unmarshal
  ((object node-affinity) source)
  (multiple-value-bind (value present-p)
      (gethash "preferredDuringSchedulingIgnoredDuringExecution"
               source)
    (when present-p
      (setf (slot-value object
                        'preferred-during-scheduling-ignored-during-execution)
            (decode-object
             (cons "array" "PreferredSchedulingTerm")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "requiredDuringSchedulingIgnoredDuringExecution" source)
    (when present-p
      (setf (slot-value object
                        'required-during-scheduling-ignored-during-execution)
            (decode-object "NodeSelector" value)))))


(defclass resource-quota-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ResourceQuotaList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is a list of ResourceQuota objects. More info: https://kubernetes.io/docs/concepts/policy/resource-quotas/")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "ResourceQuotaList is a list of ResourceQuota items."))

(defmethod unmarshal
  ((object resource-quota-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "ResourceQuota") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass toleration
          (resource)
          ((effect
            :initarg
            :effect
            :type
            (or string null)
            :documentation
            "Effect indicates the taint effect to match. Empty means match all taint effects. When specified, allowed values are NoSchedule, PreferNoSchedule and NoExecute.")
           (key
            :initarg
            :key
            :type
            (or string null)
            :documentation
            "Key is the taint key that the toleration applies to. Empty means match all taint keys. If the key is empty, operator must be Exists; this combination means to match all values and all keys.")
           (toleration-seconds
            :initarg
            :toleration-seconds
            :type
            (or integer null)
            :documentation
            "TolerationSeconds represents the period of time the toleration (which must be of effect NoExecute, otherwise this field is ignored) tolerates the taint. By default, it is not set, which means tolerate the taint forever (do not evict). Zero and negative values will be treated as 0 (evict immediately) by the system.")
           (operator
            :initarg
            :operator
            :type
            (or string null)
            :documentation
            "Operator represents a key's relationship to the value. Valid operators are Exists and Equal. Defaults to Equal. Exists is equivalent to wildcard for value, so that a pod can tolerate all taints of a particular category.")
           (value
            :initarg
            :value
            :type
            (or string null)
            :documentation
            "Value is the taint value the toleration matches to. If the operator is Exists, the value should be empty, otherwise just a regular string."))
          (:documentation
           "The pod this Toleration is attached to tolerates any taint that matches the triple <key,value,effect> using the matching operator <operator>."))

(defmethod unmarshal
  ((object toleration) source)
  (multiple-value-bind (value present-p)
      (gethash "effect" source)
    (when present-p
      (setf (slot-value object 'effect)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "tolerationSeconds" source)
    (when present-p
      (setf (slot-value object 'toleration-seconds)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "operator" source)
    (when present-p
      (setf (slot-value object 'operator)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "value" source)
    (when present-p
      (setf (slot-value object 'value)
            (decode-object "string" value)))))


(defclass label-selector
          (resource)
          ((match-labels
            :initarg
            :match-labels
            :type
            (or hash-table null)
            :documentation
            "matchLabels is a map of {key,value} pairs. A single {key,value} in the matchLabels map is equivalent to an element of matchExpressions, whose key field is \"key\", the operator is \"In\", and the values array contains only \"value\". The requirements are ANDed.")
           (match-expressions
            :initarg
            :match-expressions
            :type
            list
            :documentation
            "matchExpressions is a list of label selector requirements. The requirements are ANDed."))
          (:documentation
           "A label selector is a label query over a set of resources. The result of matchLabels and matchExpressions are ANDed. An empty label selector matches all objects. A null label selector matches no objects."))

(defmethod unmarshal
  ((object label-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "matchLabels" source)
    (when present-p
      (setf (slot-value object 'match-labels)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "matchExpressions" source)
    (when present-p
      (setf (slot-value object 'match-expressions)
            (decode-object
             (cons "array" "LabelSelectorRequirement")
             value)))))


(defclass pod-template-spec
          (resource)
          ((spec
            :initarg
            :spec
            :type
            (or pod-spec null)
            :documentation
            "Specification of the desired behavior of the pod. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "PodTemplateSpec describes the data a pod should have when created from a template"))

(defmethod unmarshal
  ((object pod-template-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "PodSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass replication-controller-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind
            :initform
            "ReplicationControllerList"
            :allocation
            :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of replication controllers. More info: https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "ReplicationControllerList is a collection of replication controllers."))

(defmethod unmarshal
  ((object replication-controller-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object
             (cons "array" "ReplicationController")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass volume-node-affinity
          (resource)
          ((required
            :initarg
            :required
            :type
            (or node-selector null)
            :documentation
            "Required specifies hard node constraints that must be met."))
          (:documentation
           "VolumeNodeAffinity defines constraints that limit what nodes this volume can be accessed from."))

(defmethod unmarshal
  ((object volume-node-affinity) source)
  (multiple-value-bind (value present-p)
      (gethash "required" source)
    (when present-p
      (setf (slot-value object 'required)
            (decode-object "NodeSelector" value)))))


(defclass persistent-volume-status
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human-readable message indicating details about why the volume is in this state.")
           (phase :initarg
                  :phase
                  :type
                  (or string null)
                  :documentation
                  "Phase indicates if a volume is available, bound to a claim, or released by a claim. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#phase")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "Reason is a brief CamelCase string that describes any failure and is meant for machine parsing and tidy display in the CLI."))
          (:documentation
           "PersistentVolumeStatus is the current status of a persistent volume."))

(defmethod unmarshal
  ((object persistent-volume-status) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "phase" source)
    (when present-p
      (setf (slot-value object 'phase)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value)))))


(defclass pod-dns-config-option
          (resource)
          ((name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Required.")
           (value
            :initarg
            :value
            :type
            (or string null)
            :documentation
            nil))
          (:documentation
           "PodDNSConfigOption defines DNS resolver options of a pod."))

(defmethod unmarshal
  ((object pod-dns-config-option) source)
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "value" source)
    (when present-p
      (setf (slot-value object 'value)
            (decode-object "string" value)))))


(defclass replication-controller-status
          (resource)
          ((available-replicas
            :initarg
            :available-replicas
            :type
            (or integer null)
            :documentation
            "The number of available replicas (ready for at least minReadySeconds) for this replication controller.")
           (conditions
            :initarg
            :conditions
            :type
            list
            :documentation
            "Represents the latest available observations of a replication controller's current state.")
           (ready-replicas
            :initarg
            :ready-replicas
            :type
            (or integer null)
            :documentation
            "The number of ready replicas for this replication controller.")
           (observed-generation
            :initarg
            :observed-generation
            :type
            (or integer null)
            :documentation
            "ObservedGeneration reflects the generation of the most recently observed replication controller.")
           (replicas
            :initarg
            :replicas
            :type
            integer
            :documentation
            "Replicas is the most recently oberved number of replicas. More info: https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller#what-is-a-replicationcontroller")
           (fully-labeled-replicas
            :initarg
            :fully-labeled-replicas
            :type
            (or integer null)
            :documentation
            "The number of pods that have labels matching the labels of the pod template of the replication controller."))
          (:documentation
           "ReplicationControllerStatus represents the current status of a replication controller."))

(defmethod unmarshal
  ((object replication-controller-status) source)
  (multiple-value-bind (value present-p)
      (gethash "availableReplicas" source)
    (when present-p
      (setf (slot-value object 'available-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "conditions" source)
    (when present-p
      (setf (slot-value object 'conditions)
            (decode-object
             (cons "array" "ReplicationControllerCondition")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "readyReplicas" source)
    (when present-p
      (setf (slot-value object 'ready-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "observedGeneration" source)
    (when present-p
      (setf (slot-value object 'observed-generation)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "fullyLabeledReplicas" source)
    (when present-p
      (setf (slot-value object 'fully-labeled-replicas)
            (decode-object (cons "integer" "int32") value)))))


(defclass api-resource
          (resource)
          ((kind :initform "APIResource" :allocation :class)
           (namespaced
            :initarg
            :namespaced
            :type
            boolean
            :documentation
            "namespaced indicates if a resource is namespaced or not.")
           (verbs
            :initarg
            :verbs
            :type
            list
            :documentation
            "verbs is a list of supported kube verbs (this includes get, list, watch, create, update, patch, delete, deletecollection, and proxy)")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "name is the plural name of the resource.")
           (categories
            :initarg
            :categories
            :type
            list
            :documentation
            "categories is a list of the grouped resources this resource belongs to (e.g. 'all')")
           (singular-name
            :initarg
            :singular-name
            :type
            string
            :documentation
            "singularName is the singular name of the resource.  This allows clients to handle plural and singular opaquely. The singularName is more correct for reporting status on a single item and both singular and plural are allowed from the kubectl CLI interface.")
           (group
            :initarg
            :group
            :type
            (or string null)
            :documentation
            "group is the preferred group of the resource.  Empty implies the group of the containing resource list. For subresources, this may have a different value, for example: Scale\".")
           (version
            :initarg
            :version
            :type
            (or string null)
            :documentation
            "version is the preferred version of the resource.  Empty implies the version of the containing resource list For subresources, this may have a different value, for example: v1 (while inside a v1beta1 version of the core resource's group)\".")
           (short-names
            :initarg
            :short-names
            :type
            list
            :documentation
            "shortNames is a list of suggested short names of the resource."))
          (:documentation
           "APIResource specifies the name of a resource and whether it is namespaced."))

(defmethod unmarshal
  ((object api-resource) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "namespaced" source)
    (when present-p
      (setf (slot-value object 'namespaced)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "verbs" source)
    (when present-p
      (setf (slot-value object 'verbs)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "categories" source)
    (when present-p
      (setf (slot-value object 'categories)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "singularName" source)
    (when present-p
      (setf (slot-value object 'singular-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "group" source)
    (when present-p
      (setf (slot-value object 'group)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "version" source)
    (when present-p
      (setf (slot-value object 'version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "shortNames" source)
    (when present-p
      (setf (slot-value object 'short-names)
            (decode-object (cons "array" "string") value)))))


(defclass key-to-path
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "The relative path of the file to map the key to. May not be an absolute path. May not contain the path element '..'. May not start with the string '..'.")
           (key
            :initarg
            :key
            :type
            string
            :documentation
            "The key to project.")
           (mode
            :initarg
            :mode
            :type
            (or integer null)
            :documentation
            "Optional: mode bits to use on this file, must be a value between 0 and 0777. If not specified, the volume defaultMode will be used. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set."))
          (:documentation
           "Maps a string key to a path within a volume."))

(defmethod unmarshal
  ((object key-to-path) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "mode" source)
    (when present-p
      (setf (slot-value object 'mode)
            (decode-object (cons "integer" "int32") value)))))


(defclass affinity
          (resource)
          ((pod-anti-affinity
            :initarg
            :pod-anti-affinity
            :type
            (or pod-anti-affinity null)
            :documentation
            "Describes pod anti-affinity scheduling rules (e.g. avoid putting this pod in the same node, zone, etc. as some other pod(s)).")
           (node-affinity
            :initarg
            :node-affinity
            :type
            (or node-affinity null)
            :documentation
            "Describes node affinity scheduling rules for the pod.")
           (pod-affinity
            :initarg
            :pod-affinity
            :type
            (or pod-affinity null)
            :documentation
            "Describes pod affinity scheduling rules (e.g. co-locate this pod in the same node, zone, etc. as some other pod(s))."))
          (:documentation
           "Affinity is a group of affinity scheduling rules."))

(defmethod unmarshal
  ((object affinity) source)
  (multiple-value-bind (value present-p)
      (gethash "podAntiAffinity" source)
    (when present-p
      (setf (slot-value object 'pod-anti-affinity)
            (decode-object "PodAntiAffinity" value))))
  (multiple-value-bind (value present-p)
      (gethash "nodeAffinity" source)
    (when present-p
      (setf (slot-value object 'node-affinity)
            (decode-object "NodeAffinity" value))))
  (multiple-value-bind (value present-p)
      (gethash "podAffinity" source)
    (when present-p
      (setf (slot-value object 'pod-affinity)
            (decode-object "PodAffinity" value)))))


(defclass eviction
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Eviction" :allocation :class)
           (delete-options
            :initarg
            :delete-options
            :type
            (or delete-options null)
            :documentation
            "DeleteOptions may be provided")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "ObjectMeta describes the pod that is being evicted."))
          (:documentation
           "Eviction evicts a pod from its node subject to certain policies and safety constraints. This is a subresource of Pod.  A request to cause such an eviction is created by POSTing to .../pods/<pod name>/evictions."))

(defmethod unmarshal
  ((object eviction) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "deleteOptions" source)
    (when present-p
      (setf (slot-value object 'delete-options)
            (decode-object "DeleteOptions" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass secret
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Secret" :allocation :class)
           (data
            :initarg
            :data
            :type
            (or hash-table null)
            :documentation
            "Data contains the secret data. Each key must consist of alphanumeric characters, '-', '_' or '.'. The serialized form of the secret data is a base64 encoded string, representing the arbitrary (possibly non-string) data value here. Described in https://tools.ietf.org/html/rfc4648#section-4")
           (type
            :initarg
            :type
            :type
            (or string null)
            :documentation
            "Used to facilitate programmatic handling of secret data.")
           (string-data
            :initarg
            :string-data
            :type
            (or hash-table null)
            :documentation
            "stringData allows specifying non-binary secret data in string form. It is provided as a write-only convenience method. All keys and values are merged into the data field on write, overwriting any existing values. It is never output when reading from the API.")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "Secret holds secret data of a certain type. The total bytes of the values in the Data field must be less than MaxSecretSize bytes."))

(defmethod unmarshal
  ((object secret) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "data" source)
    (when present-p
      (setf (slot-value object 'data) (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "stringData" source)
    (when present-p
      (setf (slot-value object 'string-data)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass initializers
          (resource)
          ((pending
            :initarg
            :pending
            :type
            list
            :documentation
            "Pending is a list of initializers that must execute in order before this object is visible. When the last pending initializer is removed, and no failing result is set, the initializers struct will be set to nil and the object is considered as initialized and visible to all clients.")
           (result
            :initarg
            :result
            :type
            (or status null)
            :documentation
            "If result is set with the Failure field, the object will be persisted to storage and then deleted, ensuring that other clients can observe the deletion."))
          (:documentation
           "Initializers tracks the progress of initialization."))

(defmethod unmarshal
  ((object initializers) source)
  (multiple-value-bind (value present-p)
      (gethash "pending" source)
    (when present-p
      (setf (slot-value object 'pending)
            (decode-object (cons "array" "Initializer") value))))
  (multiple-value-bind (value present-p)
      (gethash "result" source)
    (when present-p
      (setf (slot-value object 'result)
            (decode-object "Status" value)))))


(defclass tcp-socket-action
          (resource)
          ((host
            :initarg
            :host
            :type
            (or string null)
            :documentation
            "Optional: Host name to connect to, defaults to the pod IP.")
           (port
            :initarg
            :port
            :type
            string
            :documentation
            "Number or name of the port to access on the container. Number must be in the range 1 to 65535. Name must be an IANA_SVC_NAME."))
          (:documentation
           "TCPSocketAction describes an action based on opening a socket"))

(defmethod unmarshal
  ((object tcp-socket-action) source)
  (multiple-value-bind (value present-p)
      (gethash "host" source)
    (when present-p
      (setf (slot-value object 'host) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "port" source)
    (when present-p
      (setf (slot-value object 'port) (decode-object "string" value)))))


(defclass persistent-volume-claim-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Will force the ReadOnly setting in VolumeMounts. Default false.")
           (claim-name
            :initarg
            :claim-name
            :type
            string
            :documentation
            "ClaimName is the name of a PersistentVolumeClaim in the same namespace as the pod using this volume. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistentvolumeclaims"))
          (:documentation
           "PersistentVolumeClaimVolumeSource references the user's PVC in the same namespace. This volume finds the bound PV and mounts that volume for the pod. A PersistentVolumeClaimVolumeSource is, essentially, a wrapper around another type of volume that is owned by someone else (the system)."))

(defmethod unmarshal
  ((object persistent-volume-claim-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "claimName" source)
    (when present-p
      (setf (slot-value object 'claim-name)
            (decode-object "string" value)))))


(defclass pod-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "PodList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of pods. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation "PodList is a list of Pods."))

(defmethod unmarshal
  ((object pod-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "Pod") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass secret-env-source
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the Secret must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names"))
          (:documentation
           "SecretEnvSource selects a Secret to populate the environment variables with.

The contents of the target Secret's Data field will represent the key-value pairs as environment variables."))

(defmethod unmarshal
  ((object secret-env-source) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass object-field-selector
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (field-path
            :initarg
            :field-path
            :type
            string
            :documentation
            "Path of the field to select in the specified API version."))
          (:documentation
           "ObjectFieldSelector selects an APIVersioned field of an object."))

(defmethod unmarshal
  ((object object-field-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "fieldPath" source)
    (when present-p
      (setf (slot-value object 'field-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value)))))


(defclass container-image
          (resource)
          ((names
            :initarg
            :names
            :type
            list
            :documentation
            "Names by which this image is known. e.g. [\"k8s.gcr.io/hyperkube:v1.0.7\", \"dockerhub.io/google_containers/hyperkube:v1.0.7\"]")
           (size-bytes
            :initarg
            :size-bytes
            :type
            (or integer null)
            :documentation
            "The size of the image in bytes."))
          (:documentation "Describe a container image"))

(defmethod unmarshal
  ((object container-image) source)
  (multiple-value-bind (value present-p)
      (gethash "names" source)
    (when present-p
      (setf (slot-value object 'names)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "sizeBytes" source)
    (when present-p
      (setf (slot-value object 'size-bytes)
            (decode-object (cons "integer" "int64") value)))))


(defclass http-header
          (resource)
          ((name
            :initarg
            :name
            :type
            string
            :documentation
            "The header field name")
           (value
            :initarg
            :value
            :type
            string
            :documentation
            "The header field value"))
          (:documentation
           "HTTPHeader describes a custom header to be used in HTTP probes"))

(defmethod unmarshal
  ((object http-header) source)
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "value" source)
    (when present-p
      (setf (slot-value object 'value)
            (decode-object "string" value)))))


(defclass glusterfs-volume-source
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "Path is the Glusterfs volume path. More info: https://releases.k8s.io/HEAD/examples/volumes/glusterfs/README.md#create-a-pod")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the Glusterfs volume to be mounted with read-only permissions. Defaults to false. More info: https://releases.k8s.io/HEAD/examples/volumes/glusterfs/README.md#create-a-pod")
           (endpoints
            :initarg
            :endpoints
            :type
            string
            :documentation
            "EndpointsName is the endpoint name that details Glusterfs topology. More info: https://releases.k8s.io/HEAD/examples/volumes/glusterfs/README.md#create-a-pod"))
          (:documentation
           "Represents a Glusterfs mount that lasts the lifetime of a pod. Glusterfs volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object glusterfs-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "endpoints" source)
    (when present-p
      (setf (slot-value object 'endpoints)
            (decode-object "string" value)))))


(defclass git-repo-volume-source
          (resource)
          ((repository
            :initarg
            :repository
            :type
            string
            :documentation
            "Repository URL")
           (directory :initarg
                      :directory
                      :type
                      (or string null)
                      :documentation
                      "Target directory name. Must not contain or start with '..'.  If '.' is supplied, the volume directory will be the git repository.  Otherwise, if specified, the volume will contain the git repository in the subdirectory with the given name.")
           (revision
            :initarg
            :revision
            :type
            (or string null)
            :documentation
            "Commit hash for the specified revision."))
          (:documentation
           "Represents a volume that is populated with the contents of a git repository. Git repo volumes do not support ownership management. Git repo volumes support SELinux relabeling."))

(defmethod unmarshal
  ((object git-repo-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "repository" source)
    (when present-p
      (setf (slot-value object 'repository)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "directory" source)
    (when present-p
      (setf (slot-value object 'directory)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "revision" source)
    (when present-p
      (setf (slot-value object 'revision)
            (decode-object "string" value)))))


(defclass patch
          (resource)
          nil
          (:documentation
           "Patch is provided to give a concrete name and type to the Kubernetes PATCH request body."))

(defmethod unmarshal ((object patch) source))


(defclass photon-persistent-disk-volume-source
          (resource)
          ((pd-id
            :initarg
            :pd-id
            :type
            string
            :documentation
            "ID that identifies Photon Controller persistent disk")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified."))
          (:documentation
           "Represents a Photon Controller persistent disk resource."))

(defmethod unmarshal
  ((object photon-persistent-disk-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "pdID" source)
    (when present-p
      (setf (slot-value object 'pd-id)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass node-system-info
          (resource)
          ((os-image
            :initarg
            :os-image
            :type
            string
            :documentation
            "OS Image reported by the node from /etc/os-release (e.g. Debian GNU/Linux 7 (wheezy)).")
           (system-uuid
            :initarg
            :system-uuid
            :type
            string
            :documentation
            "SystemUUID reported by the node. For unique machine identification MachineID is preferred. This field is specific to Red Hat hosts https://access.redhat.com/documentation/en-US/Red_Hat_Subscription_Management/1/html/RHSM/getting-system-uuid.html")
           (machine-id
            :initarg
            :machine-id
            :type
            string
            :documentation
            "MachineID reported by the node. For unique machine identification in the cluster this field is preferred. Learn more from man(5) machine-id: http://man7.org/linux/man-pages/man5/machine-id.5.html")
           (kernel-version
            :initarg
            :kernel-version
            :type
            string
            :documentation
            "Kernel Version reported by the node from 'uname -r' (e.g. 3.16.0-0.bpo.4-amd64).")
           (kube-proxy-version
            :initarg
            :kube-proxy-version
            :type
            string
            :documentation
            "KubeProxy Version reported by the node.")
           (container-runtime-version
            :initarg
            :container-runtime-version
            :type
            string
            :documentation
            "ContainerRuntime Version reported by the node through runtime remote API (e.g. docker://1.5.0).")
           (boot-id
            :initarg
            :boot-id
            :type
            string
            :documentation
            "Boot ID reported by the node.")
           (kubelet-version
            :initarg
            :kubelet-version
            :type
            string
            :documentation
            "Kubelet Version reported by the node.")
           (architecture
            :initarg
            :architecture
            :type
            string
            :documentation
            "The Architecture reported by the node")
           (operating-system
            :initarg
            :operating-system
            :type
            string
            :documentation
            "The Operating System reported by the node"))
          (:documentation
           "NodeSystemInfo is a set of ids/uuids to uniquely identify the node."))

(defmethod unmarshal
  ((object node-system-info) source)
  (multiple-value-bind (value present-p)
      (gethash "osImage" source)
    (when present-p
      (setf (slot-value object 'os-image)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "systemUUID" source)
    (when present-p
      (setf (slot-value object 'system-uuid)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "machineID" source)
    (when present-p
      (setf (slot-value object 'machine-id)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "kernelVersion" source)
    (when present-p
      (setf (slot-value object 'kernel-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "kubeProxyVersion" source)
    (when present-p
      (setf (slot-value object 'kube-proxy-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "containerRuntimeVersion" source)
    (when present-p
      (setf (slot-value object 'container-runtime-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "bootID" source)
    (when present-p
      (setf (slot-value object 'boot-id)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "kubeletVersion" source)
    (when present-p
      (setf (slot-value object 'kubelet-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "architecture" source)
    (when present-p
      (setf (slot-value object 'architecture)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "operatingSystem" source)
    (when present-p
      (setf (slot-value object 'operating-system)
            (decode-object "string" value)))))


(defclass persistent-volume-spec
          (resource)
          ((storageos
            :initarg
            :storageos
            :type
            (or storage-os-persistent-volume-source null)
            :documentation
            "StorageOS represents a StorageOS volume that is attached to the kubelet's host machine and mounted into the pod More info: https://releases.k8s.io/HEAD/examples/volumes/storageos/README.md")
           (cinder
            :initarg
            :cinder
            :type
            (or cinder-volume-source null)
            :documentation
            "Cinder represents a cinder volume attached and mounted on kubelets host machine More info: https://releases.k8s.io/HEAD/examples/mysql-cinder-pd/README.md")
           (aws-elastic-block-store
            :initarg
            :aws-elastic-block-store
            :type
            (or aws-elastic-block-store-volume-source null)
            :documentation
            "AWSElasticBlockStore represents an AWS Disk resource that is attached to a kubelet's host machine and then exposed to the pod. More info: https://kubernetes.io/docs/concepts/storage/volumes#awselasticblockstore")
           (local
            :initarg
            :local
            :type
            (or local-volume-source null)
            :documentation
            "Local represents directly-attached storage with node affinity")
           (storage-class-name
            :initarg
            :storage-class-name
            :type
            (or string null)
            :documentation
            "Name of StorageClass to which this persistent volume belongs. Empty value means that this volume does not belong to any StorageClass.")
           (azure-disk
            :initarg
            :azure-disk
            :type
            (or azure-disk-volume-source null)
            :documentation
            "AzureDisk represents an Azure Data Disk mount on the host and bind mount to the pod.")
           (quobyte
            :initarg
            :quobyte
            :type
            (or quobyte-volume-source null)
            :documentation
            "Quobyte represents a Quobyte mount on the host that shares a pod's lifetime")
           (volume-mode
            :initarg
            :volume-mode
            :type
            (or persistent-volume-mode null)
            :documentation
            "volumeMode defines if a volume is intended to be used with a formatted filesystem or to remain in raw block state. Value of Filesystem is implied when not included in spec. This is an alpha feature and may change in the future.")
           (mount-options
            :initarg
            :mount-options
            :type
            list
            :documentation
            "A list of mount options, e.g. [\"ro\", \"soft\"]. Not validated - mount will simply fail if one is invalid. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes/#mount-options")
           (flex-volume
            :initarg
            :flex-volume
            :type
            (or flex-persistent-volume-source null)
            :documentation
            "FlexVolume represents a generic volume resource that is provisioned/attached using an exec based plugin.")
           (claim-ref
            :initarg
            :claim-ref
            :type
            (or object-reference null)
            :documentation
            "ClaimRef is part of a bi-directional binding between PersistentVolume and PersistentVolumeClaim. Expected to be non-nil when bound. claim.VolumeName is the authoritative bind between PV and PVC. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#binding")
           (rbd
            :initarg
            :rbd
            :type
            (or rbd-persistent-volume-source null)
            :documentation
            "RBD represents a Rados Block Device mount on the host that shares a pod's lifetime. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md")
           (capacity
            :initarg
            :capacity
            :type
            (or hash-table null)
            :documentation
            "A description of the persistent volume's resources and capacity. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#capacity")
           (azure-file
            :initarg
            :azure-file
            :type
            (or azure-file-persistent-volume-source null)
            :documentation
            "AzureFile represents an Azure File Service mount on the host and bind mount to the pod.")
           (glusterfs
            :initarg
            :glusterfs
            :type
            (or glusterfs-volume-source null)
            :documentation
            "Glusterfs represents a Glusterfs volume that is attached to a host and exposed to the pod. Provisioned by an admin. More info: https://releases.k8s.io/HEAD/examples/volumes/glusterfs/README.md")
           (node-affinity
            :initarg
            :node-affinity
            :type
            (or volume-node-affinity null)
            :documentation
            "NodeAffinity defines constraints that limit what nodes this volume can be accessed from. This field influences the scheduling of pods that use this volume.")
           (host-path
            :initarg
            :host-path
            :type
            (or host-path-volume-source null)
            :documentation
            "HostPath represents a directory on the host. Provisioned by a developer or tester. This is useful for single-node development and testing only! On-host storage is not supported in any way and WILL NOT WORK in a multi-node cluster. More info: https://kubernetes.io/docs/concepts/storage/volumes#hostpath")
           (portworx-volume
            :initarg
            :portworx-volume
            :type
            (or portworx-volume-source null)
            :documentation
            "PortworxVolume represents a portworx volume attached and mounted on kubelets host machine")
           (flocker
            :initarg
            :flocker
            :type
            (or flocker-volume-source null)
            :documentation
            "Flocker represents a Flocker volume attached to a kubelet's host machine and exposed to the pod for its usage. This depends on the Flocker control service being running")
           (nfs
            :initarg
            :nfs
            :type
            (or nfs-volume-source null)
            :documentation
            "NFS represents an NFS mount on the host. Provisioned by an admin. More info: https://kubernetes.io/docs/concepts/storage/volumes#nfs")
           (gce-persistent-disk
            :initarg
            :gce-persistent-disk
            :type
            (or gce-persistent-disk-volume-source null)
            :documentation
            "GCEPersistentDisk represents a GCE Disk resource that is attached to a kubelet's host machine and then exposed to the pod. Provisioned by an admin. More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk")
           (fc
            :initarg
            :fc
            :type
            (or fc-volume-source null)
            :documentation
            "FC represents a Fibre Channel resource that is attached to a kubelet's host machine and then exposed to the pod.")
           (scale-io
            :initarg
            :scale-io
            :type
            (or scale-io-persistent-volume-source null)
            :documentation
            "ScaleIO represents a ScaleIO persistent volume attached and mounted on Kubernetes nodes.")
           (cephfs
            :initarg
            :cephfs
            :type
            (or ceph-fs-persistent-volume-source null)
            :documentation
            "CephFS represents a Ceph FS mount on the host that shares a pod's lifetime")
           (access-modes
            :initarg
            :access-modes
            :type
            list
            :documentation
            "AccessModes contains all ways the volume can be mounted. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#access-modes")
           (persistent-volume-reclaim-policy
            :initarg
            :persistent-volume-reclaim-policy
            :type
            (or string null)
            :documentation
            "What happens to a persistent volume when released from its claim. Valid options are Retain (default for manually created PersistentVolumes), Delete (default for dynamically provisioned PersistentVolumes), and Recycle (deprecated). Recycle must be supported by the volume plugin underlying this PersistentVolume. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#reclaiming")
           (csi
            :initarg
            :csi
            :type
            (or csi-persistent-volume-source null)
            :documentation
            "CSI represents storage that handled by an external CSI driver (Beta feature).")
           (iscsi
            :initarg
            :iscsi
            :type
            (or iscsi-persistent-volume-source null)
            :documentation
            "ISCSI represents an ISCSI Disk resource that is attached to a kubelet's host machine and then exposed to the pod. Provisioned by an admin.")
           (photon-persistent-disk
            :initarg
            :photon-persistent-disk
            :type
            (or photon-persistent-disk-volume-source null)
            :documentation
            "PhotonPersistentDisk represents a PhotonController persistent disk attached and mounted on kubelets host machine")
           (vsphere-volume
            :initarg
            :vsphere-volume
            :type
            (or vsphere-virtual-disk-volume-source null)
            :documentation
            "VsphereVolume represents a vSphere volume attached and mounted on kubelets host machine"))
          (:documentation
           "PersistentVolumeSpec is the specification of a persistent volume."))

(defmethod unmarshal
  ((object persistent-volume-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "storageos" source)
    (when present-p
      (setf (slot-value object 'storageos)
            (decode-object "StorageOSPersistentVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "cinder" source)
    (when present-p
      (setf (slot-value object 'cinder)
            (decode-object "CinderVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "awsElasticBlockStore" source)
    (when present-p
      (setf (slot-value object 'aws-elastic-block-store)
            (decode-object "AWSElasticBlockStoreVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "local" source)
    (when present-p
      (setf (slot-value object 'local)
            (decode-object "LocalVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "storageClassName" source)
    (when present-p
      (setf (slot-value object 'storage-class-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "azureDisk" source)
    (when present-p
      (setf (slot-value object 'azure-disk)
            (decode-object "AzureDiskVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "quobyte" source)
    (when present-p
      (setf (slot-value object 'quobyte)
            (decode-object "QuobyteVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeMode" source)
    (when present-p
      (setf (slot-value object 'volume-mode)
            (decode-object "PersistentVolumeMode" value))))
  (multiple-value-bind (value present-p)
      (gethash "mountOptions" source)
    (when present-p
      (setf (slot-value object 'mount-options)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "flexVolume" source)
    (when present-p
      (setf (slot-value object 'flex-volume)
            (decode-object "FlexPersistentVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "claimRef" source)
    (when present-p
      (setf (slot-value object 'claim-ref)
            (decode-object "ObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "rbd" source)
    (when present-p
      (setf (slot-value object 'rbd)
            (decode-object "RBDPersistentVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "capacity" source)
    (when present-p
      (setf (slot-value object 'capacity)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "azureFile" source)
    (when present-p
      (setf (slot-value object 'azure-file)
            (decode-object "AzureFilePersistentVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "glusterfs" source)
    (when present-p
      (setf (slot-value object 'glusterfs)
            (decode-object "GlusterfsVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "nodeAffinity" source)
    (when present-p
      (setf (slot-value object 'node-affinity)
            (decode-object "VolumeNodeAffinity" value))))
  (multiple-value-bind (value present-p)
      (gethash "hostPath" source)
    (when present-p
      (setf (slot-value object 'host-path)
            (decode-object "HostPathVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "portworxVolume" source)
    (when present-p
      (setf (slot-value object 'portworx-volume)
            (decode-object "PortworxVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "flocker" source)
    (when present-p
      (setf (slot-value object 'flocker)
            (decode-object "FlockerVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "nfs" source)
    (when present-p
      (setf (slot-value object 'nfs)
            (decode-object "NFSVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "gcePersistentDisk" source)
    (when present-p
      (setf (slot-value object 'gce-persistent-disk)
            (decode-object "GCEPersistentDiskVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "fc" source)
    (when present-p
      (setf (slot-value object 'fc)
            (decode-object "FCVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "scaleIO" source)
    (when present-p
      (setf (slot-value object 'scale-io)
            (decode-object "ScaleIOPersistentVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "cephfs" source)
    (when present-p
      (setf (slot-value object 'cephfs)
            (decode-object "CephFSPersistentVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "accessModes" source)
    (when present-p
      (setf (slot-value object 'access-modes)
            (decode-object
             (cons "array" "PersistentVolumeAccessMode")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "persistentVolumeReclaimPolicy" source)
    (when present-p
      (setf (slot-value object 'persistent-volume-reclaim-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "csi" source)
    (when present-p
      (setf (slot-value object 'csi)
            (decode-object "CSIPersistentVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "iscsi" source)
    (when present-p
      (setf (slot-value object 'iscsi)
            (decode-object "ISCSIPersistentVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "photonPersistentDisk" source)
    (when present-p
      (setf (slot-value object 'photon-persistent-disk)
            (decode-object "PhotonPersistentDiskVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "vsphereVolume" source)
    (when present-p
      (setf (slot-value object 'vsphere-volume)
            (decode-object "VsphereVirtualDiskVolumeSource" value)))))


(defclass service-account-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ServiceAccountList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of ServiceAccounts. More info: https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "ServiceAccountList is a list of ServiceAccount objects"))

(defmethod unmarshal
  ((object service-account-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "ServiceAccount") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass secret-reference
          (resource)
          ((name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name is unique within a namespace to reference a secret resource.")
           (namespace
            :initarg
            :namespace
            :type
            (or string null)
            :documentation
            "Namespace defines the space within which the secret name must be unique."))
          (:documentation
           "SecretReference represents a Secret Reference. It has enough information to retrieve secret in any namespace"))

(defmethod unmarshal
  ((object secret-reference) source)
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "namespace" source)
    (when present-p
      (setf (slot-value object 'namespace)
            (decode-object "string" value)))))


(defclass scale-spec
          (resource)
          ((replicas
            :initarg
            :replicas
            :type
            (or integer null)
            :documentation
            "desired number of instances for the scaled object."))
          (:documentation
           "ScaleSpec describes the attributes of a scale subresource."))

(defmethod unmarshal
  ((object scale-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value)))))


(defclass binding
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Binding" :allocation :class)
           (target
            :initarg
            :target
            :type
            object-reference
            :documentation
            "The target object that you want to bind to the standard object.")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "Binding ties one object to another; for example, a pod is bound to a node by a scheduler. Deprecated in 1.7, please use the bindings subresource of pods instead."))

(defmethod unmarshal
  ((object binding) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "target" source)
    (when present-p
      (setf (slot-value object 'target)
            (decode-object "ObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass config-map-key-selector
          (resource)
          ((key
            :initarg
            :key
            :type
            string
            :documentation
            "The key to select.")
           (optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the ConfigMap or it's key must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names"))
          (:documentation "Selects a key from a ConfigMap."))

(defmethod unmarshal
  ((object config-map-key-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass capability (resource) nil (:documentation nil))

(defmethod unmarshal ((object capability) source))


(defclass azure-file-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (share-name
            :initarg
            :share-name
            :type
            string
            :documentation
            "Share Name")
           (secret-name
            :initarg
            :secret-name
            :type
            string
            :documentation
            "the name of secret that contains Azure Storage Account Name and Key"))
          (:documentation
           "AzureFile represents an Azure File Service mount on the host and bind mount to the pod."))

(defmethod unmarshal
  ((object azure-file-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "shareName" source)
    (when present-p
      (setf (slot-value object 'share-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "secretName" source)
    (when present-p
      (setf (slot-value object 'secret-name)
            (decode-object "string" value)))))


(defclass resource-quota-spec
          (resource)
          ((scopes
            :initarg
            :scopes
            :type
            list
            :documentation
            "A collection of filters that must match each object tracked by a quota. If not specified, the quota matches all objects.")
           (hard
            :initarg
            :hard
            :type
            (or hash-table null)
            :documentation
            "Hard is the set of desired hard limits for each named resource. More info: https://kubernetes.io/docs/concepts/policy/resource-quotas/"))
          (:documentation
           "ResourceQuotaSpec defines the desired hard limits to enforce for Quota."))

(defmethod unmarshal
  ((object resource-quota-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "scopes" source)
    (when present-p
      (setf (slot-value object 'scopes)
            (decode-object
             (cons "array" "ResourceQuotaScope")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "hard" source)
    (when present-p
      (setf (slot-value object 'hard) (decode-object "object" value)))))


(defclass deletion-propagation (resource) nil (:documentation nil))

(defmethod unmarshal ((object deletion-propagation) source))


(defclass node-address
          (resource)
          ((type
            :initarg
            :type
            :type
            string
            :documentation
            "Node address type, one of Hostname, ExternalIP or InternalIP.")
           (address
            :initarg
            :address
            :type
            string
            :documentation
            "The node address."))
          (:documentation
           "NodeAddress contains information for the node's address."))

(defmethod unmarshal
  ((object node-address) source)
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "address" source)
    (when present-p
      (setf (slot-value object 'address)
            (decode-object "string" value)))))


(defclass azure-disk-volume-source
          (resource)
          ((kind :initform "AzureDiskVolumeSource" :allocation :class)
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (disk-uri
            :initarg
            :disk-uri
            :type
            string
            :documentation
            "The URI the data disk in the blob storage")
           (caching-mode
            :initarg
            :caching-mode
            :type
            (or azure-data-disk-caching-mode null)
            :documentation
            "Host Caching mode: None, Read Only, Read Write.")
           (disk-name
            :initarg
            :disk-name
            :type
            string
            :documentation
            "The Name of the data disk in the blob storage")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified."))
          (:documentation
           "AzureDisk represents an Azure Data Disk mount on the host and bind mount to the pod."))

(defmethod unmarshal
  ((object azure-disk-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind)
            (decode-object "AzureDataDiskKind" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "diskURI" source)
    (when present-p
      (setf (slot-value object 'disk-uri)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "cachingMode" source)
    (when present-p
      (setf (slot-value object 'caching-mode)
            (decode-object "AzureDataDiskCachingMode" value))))
  (multiple-value-bind (value present-p)
      (gethash "diskName" source)
    (when present-p
      (setf (slot-value object 'disk-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass finalizer-name (resource) nil (:documentation nil))

(defmethod unmarshal ((object finalizer-name) source))


(defclass projected-volume-source
          (resource)
          ((default-mode
            :initarg
            :default-mode
            :type
            (or integer null)
            :documentation
            "Mode bits to use on created files by default. Must be a value between 0 and 0777. Directories within the path are not affected by this setting. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set.")
           (sources
            :initarg
            :sources
            :type
            list
            :documentation
            "list of volume projections"))
          (:documentation "Represents a projected volume source"))

(defmethod unmarshal
  ((object projected-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "defaultMode" source)
    (when present-p
      (setf (slot-value object 'default-mode)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "sources" source)
    (when present-p
      (setf (slot-value object 'sources)
            (decode-object (cons "array" "VolumeProjection") value)))))


(defclass pod-condition
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "Human-readable message indicating details about last transition.")
           (last-probe-time
            :initarg
            :last-probe-time
            :type
            (or string null)
            :documentation
            "Last time we probed the condition.")
           (last-transition-time
            :initarg
            :last-transition-time
            :type
            (or string null)
            :documentation
            "Last time the condition transitioned from one status to another.")
           (type
            :initarg
            :type
            :type
            string
            :documentation
            "Type is the type of the condition. Currently only Ready. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#pod-conditions")
           (status
            :initarg
            :status
            :type
            string
            :documentation
            "Status is the status of the condition. Can be True, False, Unknown. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#pod-conditions")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "Unique, one-word, CamelCase reason for the condition's last transition."))
          (:documentation
           "PodCondition contains details for the current condition of this pod."))

(defmethod unmarshal
  ((object pod-condition) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastProbeTime" source)
    (when present-p
      (setf (slot-value object 'last-probe-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastTransitionTime" source)
    (when present-p
      (setf (slot-value object 'last-transition-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value)))))


(defclass persistent-volume-claim-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind
            :initform
            "PersistentVolumeClaimList"
            :allocation
            :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "A list of persistent volume claims. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistentvolumeclaims")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "PersistentVolumeClaimList is a list of PersistentVolumeClaim items."))

(defmethod unmarshal
  ((object persistent-volume-claim-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object
             (cons "array" "PersistentVolumeClaim")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass taint
          (resource)
          ((effect
            :initarg
            :effect
            :type
            string
            :documentation
            "Required. The effect of the taint on pods that do not tolerate the taint. Valid effects are NoSchedule, PreferNoSchedule and NoExecute.")
           (key
            :initarg
            :key
            :type
            string
            :documentation
            "Required. The taint key to be applied to a node.")
           (value
            :initarg
            :value
            :type
            (or string null)
            :documentation
            "Required. The taint value corresponding to the taint key.")
           (time-added
            :initarg
            :time-added
            :type
            (or string null)
            :documentation
            "TimeAdded represents the time at which the taint was added. It is only written for NoExecute taints."))
          (:documentation
           "The node this Taint is attached to has the \"effect\" on any pod that does not tolerate the Taint."))

(defmethod unmarshal
  ((object taint) source)
  (multiple-value-bind (value present-p)
      (gethash "effect" source)
    (when present-p
      (setf (slot-value object 'effect)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "value" source)
    (when present-p
      (setf (slot-value object 'value)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "timeAdded" source)
    (when present-p
      (setf (slot-value object 'time-added)
            (decode-object "string" value)))))


(defclass container-port
          (resource)
          ((host-ip
            :initarg
            :host-ip
            :type
            (or string null)
            :documentation
            "What host IP to bind the external port to.")
           (host-port
            :initarg
            :host-port
            :type
            (or integer null)
            :documentation
            "Number of port to expose on the host. If specified, this must be a valid port number, 0 < x < 65536. If HostNetwork is specified, this must match ContainerPort. Most containers do not need this.")
           (container-port
            :initarg
            :container-port
            :type
            integer
            :documentation
            "Number of port to expose on the pod's IP address. This must be a valid port number, 0 < x < 65536.")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "If specified, this must be an IANA_SVC_NAME and unique within the pod. Each named port in a pod must have a unique name. Name for the port that can be referred to by services.")
           (protocol
            :initarg
            :protocol
            :type
            (or string null)
            :documentation
            "Protocol for port. Must be UDP or TCP. Defaults to \"TCP\"."))
          (:documentation
           "ContainerPort represents a network port in a single container."))

(defmethod unmarshal
  ((object container-port) source)
  (multiple-value-bind (value present-p)
      (gethash "hostIP" source)
    (when present-p
      (setf (slot-value object 'host-ip)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "hostPort" source)
    (when present-p
      (setf (slot-value object 'host-port)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "containerPort" source)
    (when present-p
      (setf (slot-value object 'container-port)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "protocol" source)
    (when present-p
      (setf (slot-value object 'protocol)
            (decode-object "string" value)))))


(defclass pod-anti-affinity
          (resource)
          ((preferred-during-scheduling-ignored-during-execution
            :initarg
            :preferred-during-scheduling-ignored-during-execution
            :type
            list
            :documentation
            "The scheduler will prefer to schedule pods to nodes that satisfy the anti-affinity expressions specified by this field, but it may choose a node that violates one or more of the expressions. The node that is most preferred is the one with the greatest sum of weights, i.e. for each node that meets all of the scheduling requirements (resource request, requiredDuringScheduling anti-affinity expressions, etc.), compute a sum by iterating through the elements of this field and adding \"weight\" to the sum if the node has pods which matches the corresponding podAffinityTerm; the node(s) with the highest sum are the most preferred.")
           (required-during-scheduling-ignored-during-execution
            :initarg
            :required-during-scheduling-ignored-during-execution
            :type
            list
            :documentation
            "If the anti-affinity requirements specified by this field are not met at scheduling time, the pod will not be scheduled onto the node. If the anti-affinity requirements specified by this field cease to be met at some point during pod execution (e.g. due to a pod label update), the system may or may not try to eventually evict the pod from its node. When there are multiple elements, the lists of nodes corresponding to each podAffinityTerm are intersected, i.e. all terms must be satisfied."))
          (:documentation
           "Pod anti affinity is a group of inter pod anti affinity scheduling rules."))

(defmethod unmarshal
  ((object pod-anti-affinity) source)
  (multiple-value-bind (value present-p)
      (gethash "preferredDuringSchedulingIgnoredDuringExecution"
               source)
    (when present-p
      (setf (slot-value object
                        'preferred-during-scheduling-ignored-during-execution)
            (decode-object
             (cons "array" "WeightedPodAffinityTerm")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "requiredDuringSchedulingIgnoredDuringExecution" source)
    (when present-p
      (setf (slot-value object
                        'required-during-scheduling-ignored-during-execution)
            (decode-object (cons "array" "PodAffinityTerm") value)))))


(defclass persistent-volume-claim
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "PersistentVolumeClaim" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or persistent-volume-claim-status null)
            :documentation
            "Status represents the current information/status of a persistent volume claim. Read-only. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistentvolumeclaims")
           (spec
            :initarg
            :spec
            :type
            (or persistent-volume-claim-spec null)
            :documentation
            "Spec defines the desired characteristics of a volume requested by a pod author. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistentvolumeclaims")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "PersistentVolumeClaim is a user's request for and claim to a persistent volume"))

(defmethod unmarshal
  ((object persistent-volume-claim) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "PersistentVolumeClaimStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "PersistentVolumeClaimSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass resource-quota
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ResourceQuota" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or resource-quota-status null)
            :documentation
            "Status defines the actual enforced quota and its current usage. https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (spec
            :initarg
            :spec
            :type
            (or resource-quota-spec null)
            :documentation
            "Spec defines the desired quota. https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "ResourceQuota sets aggregate quota restrictions enforced per namespace"))

(defmethod unmarshal
  ((object resource-quota) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "ResourceQuotaStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "ResourceQuotaSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass config-map-volume-source
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the ConfigMap or it's keys must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names")
           (default-mode
            :initarg
            :default-mode
            :type
            (or integer null)
            :documentation
            "Optional: mode bits to use on created files by default. Must be a value between 0 and 0777. Defaults to 0644. Directories within the path are not affected by this setting. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set.")
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "If unspecified, each key-value pair in the Data field of the referenced ConfigMap will be projected into the volume as a file whose name is the key and content is the value. If specified, the listed keys will be projected into the specified paths, and unlisted keys will not be present. If a key is specified which is not present in the ConfigMap, the volume setup will error unless it is marked optional. Paths must be relative and may not contain the '..' path or start with '..'."))
          (:documentation
           "Adapts a ConfigMap into a volume.

The contents of the target ConfigMap's Data field will be presented in a volume as files using the keys in the Data field as the file names, unless the items element is populated with specific mappings of keys to paths. ConfigMap volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object config-map-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "defaultMode" source)
    (when present-p
      (setf (slot-value object 'default-mode)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "KeyToPath") value)))))


(defclass pod-template-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "PodTemplateList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of pod templates")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation "PodTemplateList is a list of PodTemplates."))

(defmethod unmarshal
  ((object pod-template-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "PodTemplate") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass unique-volume-name (resource) nil (:documentation nil))

(defmethod unmarshal ((object unique-volume-name) source))


(defclass replication-controller-condition
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human readable message indicating details about the transition.")
           (last-transition-time
            :initarg
            :last-transition-time
            :type
            (or string null)
            :documentation
            "The last time the condition transitioned from one status to another.")
           (type
            :initarg
            :type
            :type
            string
            :documentation
            "Type of replication controller condition.")
           (status
            :initarg
            :status
            :type
            string
            :documentation
            "Status of the condition, one of True, False, Unknown.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "The reason for the condition's last transition."))
          (:documentation
           "ReplicationControllerCondition describes the state of a replication controller at a certain point."))

(defmethod unmarshal
  ((object replication-controller-condition) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastTransitionTime" source)
    (when present-p
      (setf (slot-value object 'last-transition-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value)))))


(defclass flocker-volume-source
          (resource)
          ((dataset-uuid
            :initarg
            :dataset-uuid
            :type
            (or string null)
            :documentation
            "UUID of the dataset. This is unique identifier of a Flocker dataset")
           (dataset-name
            :initarg
            :dataset-name
            :type
            (or string null)
            :documentation
            "Name of the dataset stored as metadata -> name on the dataset for Flocker should be considered as deprecated"))
          (:documentation
           "Represents a Flocker volume mounted by the Flocker agent. One and only one of datasetName and datasetUUID should be set. Flocker volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object flocker-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "datasetUUID" source)
    (when present-p
      (setf (slot-value object 'dataset-uuid)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "datasetName" source)
    (when present-p
      (setf (slot-value object 'dataset-name)
            (decode-object "string" value)))))


(defclass label-selector-requirement
          (resource)
          ((key
            :initarg
            :key
            :type
            string
            :documentation
            "key is the label key that the selector applies to.")
           (values :initarg
                   :values
                   :type
                   list
                   :documentation
                   "values is an array of string values. If the operator is In or NotIn, the values array must be non-empty. If the operator is Exists or DoesNotExist, the values array must be empty. This array is replaced during a strategic merge patch.")
           (operator
            :initarg
            :operator
            :type
            string
            :documentation
            "operator represents a key's relationship to a set of values. Valid operators are In, NotIn, Exists and DoesNotExist."))
          (:documentation
           "A label selector requirement is a selector that contains values, a key, and an operator that relates the key and values."))

(defmethod unmarshal
  ((object label-selector-requirement) source)
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "values" source)
    (when present-p
      (setf (slot-value object 'values)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "operator" source)
    (when present-p
      (setf (slot-value object 'operator)
            (decode-object "string" value)))))


(defclass watch-event
          (resource)
          ((type :initarg :type :type string :documentation nil)
           (object :initarg :object :type string :documentation nil))
          (:documentation nil))

(defmethod unmarshal
  ((object watch-event) source)
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "object" source)
    (when present-p
      (setf (slot-value object 'object)
            (decode-object "string" value)))))


(defclass event
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Event" :allocation :class)
           (message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human-readable description of the status of this operation.")
           (involved-object
            :initarg
            :involved-object
            :type
            object-reference
            :documentation
            "The object that this event is about.")
           (first-timestamp
            :initarg
            :first-timestamp
            :type
            (or string null)
            :documentation
            "The time at which the event was first recorded. (Time of server receipt is in TypeMeta.)")
           (reporting-component
            :initarg
            :reporting-component
            :type
            string
            :documentation
            "Name of the controller that emitted this Event, e.g. `kubernetes.io/kubelet`.")
           (related
            :initarg
            :related
            :type
            (or object-reference null)
            :documentation
            "Optional secondary object for more complex actions.")
           (series
            :initarg
            :series
            :type
            (or event-series null)
            :documentation
            "Data about the Event series this event represents or nil if it's a singleton Event.")
           (type
            :initarg
            :type
            :type
            (or string null)
            :documentation
            "Type of this event (Normal, Warning), new types could be added in the future")
           (event-time
            :initarg
            :event-time
            :type
            (or string null)
            :documentation
            "Time when this Event was first observed.")
           (last-timestamp
            :initarg
            :last-timestamp
            :type
            (or string null)
            :documentation
            "The time at which the most recent occurrence of this event was recorded.")
           (count :initarg
                  :count
                  :type
                  (or integer null)
                  :documentation
                  "The number of times this event has occurred.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "This should be a short, machine understandable string that gives the reason for the transition into the object's current status.")
           (action
            :initarg
            :action
            :type
            (or string null)
            :documentation
            "What action was taken/failed regarding to the Regarding object.")
           (source
            :initarg
            :source
            :type
            (or event-source null)
            :documentation
            "The component reporting this event. Should be a short machine understandable string.")
           (reporting-instance
            :initarg
            :reporting-instance
            :type
            string
            :documentation
            "ID of the controller instance, e.g. `kubelet-xyzf`.")
           (metadata
            :initarg
            :metadata
            :type
            object-meta
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "Event is a report of an event somewhere in the cluster."))

(defmethod unmarshal
  ((object event) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "involvedObject" source)
    (when present-p
      (setf (slot-value object 'involved-object)
            (decode-object "ObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "firstTimestamp" source)
    (when present-p
      (setf (slot-value object 'first-timestamp)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reportingComponent" source)
    (when present-p
      (setf (slot-value object 'reporting-component)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "related" source)
    (when present-p
      (setf (slot-value object 'related)
            (decode-object "ObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "series" source)
    (when present-p
      (setf (slot-value object 'series)
            (decode-object "EventSeries" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "eventTime" source)
    (when present-p
      (setf (slot-value object 'event-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastTimestamp" source)
    (when present-p
      (setf (slot-value object 'last-timestamp)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "count" source)
    (when present-p
      (setf (slot-value object 'count)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "action" source)
    (when present-p
      (setf (slot-value object 'action)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "source" source)
    (when present-p
      (setf (slot-value object 'source)
            (decode-object "EventSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reportingInstance" source)
    (when present-p
      (setf (slot-value object 'reporting-instance)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass endpoints-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "EndpointsList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of endpoints.")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation "EndpointsList is a list of endpoints."))

(defmethod unmarshal
  ((object endpoints-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "Endpoints") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass scale-status
          (resource)
          ((selector
            :initarg
            :selector
            :type
            (or string null)
            :documentation
            "label query over pods that should match the replicas count. This is same as the label selector but in the string format to avoid introspection by clients. The string will be in the same format as the query-param syntax. More info about label selectors: http://kubernetes.io/docs/user-guide/labels#label-selectors")
           (replicas
            :initarg
            :replicas
            :type
            integer
            :documentation
            "actual number of observed instances of the scaled object."))
          (:documentation
           "ScaleStatus represents the current status of a scale subresource."))

(defmethod unmarshal
  ((object scale-status) source)
  (multiple-value-bind (value present-p)
      (gethash "selector" source)
    (when present-p
      (setf (slot-value object 'selector)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value)))))


(defclass api-resource-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "APIResourceList" :allocation :class)
           (resources
            :initarg
            :resources
            :type
            list
            :documentation
            "resources contains the name of the resources and if they are namespaced.")
           (group-version
            :initarg
            :group-version
            :type
            string
            :documentation
            "groupVersion is the group and version this APIResourceList is for."))
          (:documentation
           "APIResourceList is a list of APIResource, it is used to expose the name of the resources supported in a specific group and version, and if the resource is namespaced."))

(defmethod unmarshal
  ((object api-resource-list) source)
  (multiple-value-bind (value present-p)
      (gethash "resources" source)
    (when present-p
      (setf (slot-value object 'resources)
            (decode-object (cons "array" "APIResource") value))))
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "groupVersion" source)
    (when present-p
      (setf (slot-value object 'group-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value)))))


(defclass config-map-env-source
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the ConfigMap must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names"))
          (:documentation
           "ConfigMapEnvSource selects a ConfigMap to populate the environment variables with.

The contents of the target ConfigMap's Data field will represent the key-value pairs as environment variables."))

(defmethod unmarshal
  ((object config-map-env-source) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass capabilities
          (resource)
          ((drop
            :initarg
            :drop
            :type
            list
            :documentation
            "Removed capabilities")
           (add
            :initarg
            :add
            :type
            list
            :documentation
            "Added capabilities"))
          (:documentation
           "Adds and removes POSIX capabilities from running containers."))

(defmethod unmarshal
  ((object capabilities) source)
  (multiple-value-bind (value present-p)
      (gethash "drop" source)
    (when present-p
      (setf (slot-value object 'drop)
            (decode-object (cons "array" "Capability") value))))
  (multiple-value-bind (value present-p)
      (gethash "add" source)
    (when present-p
      (setf (slot-value object 'add)
            (decode-object (cons "array" "Capability") value)))))


(defclass local-volume-source
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "The full path to the volume on the node. It can be either a directory or block device (disk, partition, ...). Directories can be represented only by PersistentVolume with VolumeMode=Filesystem. Block devices can be represented only by VolumeMode=Block, which also requires the BlockVolume alpha feature gate to be enabled."))
          (:documentation
           "Local represents directly-attached storage with node affinity (Beta feature)"))

(defmethod unmarshal
  ((object local-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value)))))


(defclass exec-action
          (resource)
          ((command
            :initarg
            :command
            :type
            list
            :documentation
            "Command is the command line to execute inside the container, the working directory for the command  is root ('/') in the container's filesystem. The command is simply exec'd, it is not run inside a shell, so traditional shell instructions ('|', etc) won't work. To use a shell, you need to explicitly call out to that shell. Exit status of 0 is treated as live/healthy and non-zero is unhealthy."))
          (:documentation
           "ExecAction describes a \"run in container\" action."))

(defmethod unmarshal
  ((object exec-action) source)
  (multiple-value-bind (value present-p)
      (gethash "command" source)
    (when present-p
      (setf (slot-value object 'command)
            (decode-object (cons "array" "string") value)))))


(defclass container-state
          (resource)
          ((running
            :initarg
            :running
            :type
            (or container-state-running null)
            :documentation
            "Details about a running container")
           (waiting
            :initarg
            :waiting
            :type
            (or container-state-waiting null)
            :documentation
            "Details about a waiting container")
           (terminated
            :initarg
            :terminated
            :type
            (or container-state-terminated null)
            :documentation
            "Details about a terminated container"))
          (:documentation
           "ContainerState holds a possible state of container. Only one of its members may be specified. If none of them is specified, the default one is ContainerStateWaiting."))

(defmethod unmarshal
  ((object container-state) source)
  (multiple-value-bind (value present-p)
      (gethash "running" source)
    (when present-p
      (setf (slot-value object 'running)
            (decode-object "ContainerStateRunning" value))))
  (multiple-value-bind (value present-p)
      (gethash "waiting" source)
    (when present-p
      (setf (slot-value object 'waiting)
            (decode-object "ContainerStateWaiting" value))))
  (multiple-value-bind (value present-p)
      (gethash "terminated" source)
    (when present-p
      (setf (slot-value object 'terminated)
            (decode-object "ContainerStateTerminated" value)))))


(defclass daemon-endpoint
          (resource)
          ((port
            :initarg
            :port
            :type
            integer
            :documentation
            "Port number of the given endpoint."))
          (:documentation
           "DaemonEndpoint contains information about a single Daemon endpoint."))

(defmethod unmarshal
  ((object daemon-endpoint) source)
  (multiple-value-bind (value present-p)
      (gethash "Port" source)
    (when present-p
      (setf (slot-value object 'port)
            (decode-object (cons "integer" "int32") value)))))


(defclass volume-device
          (resource)
          ((device-path
            :initarg
            :device-path
            :type
            string
            :documentation
            "devicePath is the path inside of the container that the device will be mapped to.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "name must match the name of a persistentVolumeClaim in the pod"))
          (:documentation
           "volumeDevice describes a mapping of a raw block device within a container."))

(defmethod unmarshal
  ((object volume-device) source)
  (multiple-value-bind (value present-p)
      (gethash "devicePath" source)
    (when present-p
      (setf (slot-value object 'device-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass handler
          (resource)
          ((exec
            :initarg
            :exec
            :type
            (or exec-action null)
            :documentation
            "One and only one of the following should be specified. Exec specifies the action to take.")
           (http-get
            :initarg
            :http-get
            :type
            (or http-get-action null)
            :documentation
            "HTTPGet specifies the http request to perform.")
           (tcp-socket
            :initarg
            :tcp-socket
            :type
            (or tcp-socket-action null)
            :documentation
            "TCPSocket specifies an action involving a TCP port. TCP hooks not yet supported"))
          (:documentation
           "Handler defines a specific action that should be taken"))

(defmethod unmarshal
  ((object handler) source)
  (multiple-value-bind (value present-p)
      (gethash "exec" source)
    (when present-p
      (setf (slot-value object 'exec)
            (decode-object "ExecAction" value))))
  (multiple-value-bind (value present-p)
      (gethash "httpGet" source)
    (when present-p
      (setf (slot-value object 'http-get)
            (decode-object "HTTPGetAction" value))))
  (multiple-value-bind (value present-p)
      (gethash "tcpSocket" source)
    (when present-p
      (setf (slot-value object 'tcp-socket)
            (decode-object "TCPSocketAction" value)))))


(defclass endpoints
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "Endpoints" :allocation :class)
           (subsets
            :initarg
            :subsets
            :type
            list
            :documentation
            "The set of all endpoints is the union of all subsets. Addresses are placed into subsets according to the IPs they share. A single address with multiple ports, some of which are ready and some of which are not (because they come from different containers) will result in the address being displayed in different subsets for the different ports. No address will appear in both Addresses and NotReadyAddresses in the same subset. Sets of addresses and ports that comprise a service.")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "Endpoints is a collection of endpoints that implement the actual service. Example:
  Name: \"mysvc\",
  Subsets: [
    {
      Addresses: [{\"ip\": \"10.10.1.1\"}, {\"ip\": \"10.10.2.2\"}],
      Ports: [{\"name\": \"a\", \"port\": 8675}, {\"name\": \"b\", \"port\": 309}]
    },
    {
      Addresses: [{\"ip\": \"10.10.3.3\"}],
      Ports: [{\"name\": \"a\", \"port\": 93}, {\"name\": \"b\", \"port\": 76}]
    },
 ]"))

(defmethod unmarshal
  ((object endpoints) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "subsets" source)
    (when present-p
      (setf (slot-value object 'subsets)
            (decode-object (cons "array" "EndpointSubset") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass pod-status
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human readable message indicating details about why the pod is in this condition.")
           (conditions
            :initarg
            :conditions
            :type
            list
            :documentation
            "Current service state of pod. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#pod-conditions")
           (host-ip
            :initarg
            :host-ip
            :type
            (or string null)
            :documentation
            "IP address of the host to which the pod is assigned. Empty if not yet scheduled.")
           (qos-class
            :initarg
            :qos-class
            :type
            (or string null)
            :documentation
            "The Quality of Service (QOS) classification assigned to the pod based on resource requirements See PodQOSClass type for available QOS classes More info: https://git.k8s.io/community/contributors/design-proposals/node/resource-qos.md")
           (phase :initarg
                  :phase
                  :type
                  (or string null)
                  :documentation
                  "Current condition of the pod. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#pod-phase")
           (container-statuses
            :initarg
            :container-statuses
            :type
            list
            :documentation
            "The list has one entry per container in the manifest. Each entry is currently the output of `docker inspect`. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#pod-and-container-status")
           (nominated-node-name
            :initarg
            :nominated-node-name
            :type
            (or string null)
            :documentation
            "nominatedNodeName is set only when this pod preempts other pods on the node, but it cannot be scheduled right away as preemption victims receive their graceful termination periods. This field does not guarantee that the pod will be scheduled on this node. Scheduler may decide to place the pod elsewhere if other nodes become available sooner. Scheduler may also decide to give the resources on this node to a higher priority pod that is created after preemption. As a result, this field may be different than PodSpec.nodeName when the pod is scheduled.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "A brief CamelCase message indicating details about why the pod is in this state. e.g. 'Evicted'")
           (pod-ip
            :initarg
            :pod-ip
            :type
            (or string null)
            :documentation
            "IP address allocated to the pod. Routable at least within the cluster. Empty if not yet allocated.")
           (start-time
            :initarg
            :start-time
            :type
            (or string null)
            :documentation
            "RFC 3339 date and time at which the object was acknowledged by the Kubelet. This is before the Kubelet pulled the container image(s) for the pod.")
           (init-container-statuses
            :initarg
            :init-container-statuses
            :type
            list
            :documentation
            "The list has one entry per init container in the manifest. The most recent successful init container will have ready = true, the most recently started container will have startTime set. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#pod-and-container-status"))
          (:documentation
           "PodStatus represents information about the status of a pod. Status may trail the actual state of a system."))

(defmethod unmarshal
  ((object pod-status) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "conditions" source)
    (when present-p
      (setf (slot-value object 'conditions)
            (decode-object (cons "array" "PodCondition") value))))
  (multiple-value-bind (value present-p)
      (gethash "hostIP" source)
    (when present-p
      (setf (slot-value object 'host-ip)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "qosClass" source)
    (when present-p
      (setf (slot-value object 'qos-class)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "phase" source)
    (when present-p
      (setf (slot-value object 'phase)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "containerStatuses" source)
    (when present-p
      (setf (slot-value object 'container-statuses)
            (decode-object (cons "array" "ContainerStatus") value))))
  (multiple-value-bind (value present-p)
      (gethash "nominatedNodeName" source)
    (when present-p
      (setf (slot-value object 'nominated-node-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "podIP" source)
    (when present-p
      (setf (slot-value object 'pod-ip)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "startTime" source)
    (when present-p
      (setf (slot-value object 'start-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "initContainerStatuses" source)
    (when present-p
      (setf (slot-value object 'init-container-statuses)
            (decode-object (cons "array" "ContainerStatus") value)))))


(defclass aws-elastic-block-store-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Specify \"true\" to force and set the ReadOnly property in VolumeMounts to \"true\". If omitted, the default is \"false\". More info: https://kubernetes.io/docs/concepts/storage/volumes#awselasticblockstore")
           (partition
            :initarg
            :partition
            :type
            (or integer null)
            :documentation
            "The partition in the volume that you want to mount. If omitted, the default is to mount by volume name. Examples: For volume /dev/sda1, you specify the partition as \"1\". Similarly, the volume partition for /dev/sda is \"0\" (or you can leave the property empty).")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type of the volume that you want to mount. Tip: Ensure that the filesystem type is supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://kubernetes.io/docs/concepts/storage/volumes#awselasticblockstore")
           (volume-id
            :initarg
            :volume-id
            :type
            string
            :documentation
            "Unique ID of the persistent disk resource in AWS (Amazon EBS volume). More info: https://kubernetes.io/docs/concepts/storage/volumes#awselasticblockstore"))
          (:documentation
           "Represents a Persistent Disk resource in AWS.

An AWS EBS disk must exist before mounting to a container. The disk must also be in the same AWS zone as the kubelet. An AWS EBS disk can only be mounted as read/write once. AWS EBS volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object aws-elastic-block-store-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "partition" source)
    (when present-p
      (setf (slot-value object 'partition)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeID" source)
    (when present-p
      (setf (slot-value object 'volume-id)
            (decode-object "string" value)))))


(defclass secret-projection
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the Secret or its key must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names")
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "If unspecified, each key-value pair in the Data field of the referenced Secret will be projected into the volume as a file whose name is the key and content is the value. If specified, the listed keys will be projected into the specified paths, and unlisted keys will not be present. If a key is specified which is not present in the Secret, the volume setup will error unless it is marked optional. Paths must be relative and may not contain the '..' path or start with '..'."))
          (:documentation
           "Adapts a secret into a projected volume.

The contents of the target Secret's Data field will be presented in a projected volume as files using the keys in the Data field as the file names. Note that this is identical to a secret volume source without the default mode."))

(defmethod unmarshal
  ((object secret-projection) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "KeyToPath") value)))))


(defclass status-details
          (resource)
          ((kind :initform "StatusDetails" :allocation :class)
           (causes
            :initarg
            :causes
            :type
            list
            :documentation
            "The Causes array includes more details associated with the StatusReason failure. Not all StatusReasons may provide detailed causes.")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "The name attribute of the resource associated with the status StatusReason (when there is a single name which can be described).")
           (group
            :initarg
            :group
            :type
            (or string null)
            :documentation
            "The group attribute of the resource associated with the status StatusReason.")
           (retry-after-seconds
            :initarg
            :retry-after-seconds
            :type
            (or integer null)
            :documentation
            "If specified, the time in seconds before the operation should be retried. Some errors may indicate the client must take an alternate action - for those errors this field may indicate how long to wait before taking the alternate action.")
           (uid
            :initarg
            :uid
            :type
            (or string null)
            :documentation
            "UID of the resource. (when there is a single resource which can be described). More info: http://kubernetes.io/docs/user-guide/identifiers#uids"))
          (:documentation
           "StatusDetails is a set of additional properties that MAY be set by the server to provide additional information about a response. The Reason field of a Status object defines what attributes will be set. Clients must ignore fields that do not match the defined type of each attribute, and should assume that any attribute may be empty, invalid, or under defined."))

(defmethod unmarshal
  ((object status-details) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "causes" source)
    (when present-p
      (setf (slot-value object 'causes)
            (decode-object (cons "array" "StatusCause") value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "group" source)
    (when present-p
      (setf (slot-value object 'group)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "retryAfterSeconds" source)
    (when present-p
      (setf (slot-value object 'retry-after-seconds)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "uid" source)
    (when present-p
      (setf (slot-value object 'uid) (decode-object "string" value)))))


(defclass node-condition
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "Human readable message indicating details about last transition.")
           (last-transition-time
            :initarg
            :last-transition-time
            :type
            (or string null)
            :documentation
            "Last time the condition transit from one status to another.")
           (type
            :initarg
            :type
            :type
            string
            :documentation
            "Type of node condition.")
           (status
            :initarg
            :status
            :type
            string
            :documentation
            "Status of the condition, one of True, False, Unknown.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "(brief) reason for the condition's last transition.")
           (last-heartbeat-time
            :initarg
            :last-heartbeat-time
            :type
            (or string null)
            :documentation
            "Last time we got an update on a given condition."))
          (:documentation
           "NodeCondition contains condition information for a node."))

(defmethod unmarshal
  ((object node-condition) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastTransitionTime" source)
    (when present-p
      (setf (slot-value object 'last-transition-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastHeartbeatTime" source)
    (when present-p
      (setf (slot-value object 'last-heartbeat-time)
            (decode-object "string" value)))))


(defclass csi-persistent-volume-source
          (resource)
          ((controller-publish-secret-ref
            :initarg
            :controller-publish-secret-ref
            :type
            (or secret-reference null)
            :documentation
            "ControllerPublishSecretRef is a reference to the secret object containing sensitive information to pass to the CSI driver to complete the CSI ControllerPublishVolume and ControllerUnpublishVolume calls. This field is optional, and  may be empty if no secret is required. If the secret object contains more than one secret, all secrets are passed.")
           (node-publish-secret-ref
            :initarg
            :node-publish-secret-ref
            :type
            (or secret-reference null)
            :documentation
            "NodePublishSecretRef is a reference to the secret object containing sensitive information to pass to the CSI driver to complete the CSI NodePublishVolume and NodeUnpublishVolume calls. This field is optional, and  may be empty if no secret is required. If the secret object contains more than one secret, all secrets are passed.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: The value to pass to ControllerPublishVolumeRequest. Defaults to false (read/write).")
           (volume-handle
            :initarg
            :volume-handle
            :type
            string
            :documentation
            "VolumeHandle is the unique volume name returned by the CSI volume plugin’s CreateVolume to refer to the volume on all subsequent calls. Required.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified.")
           (driver
            :initarg
            :driver
            :type
            string
            :documentation
            "Driver is the name of the driver to use for this volume. Required.")
           (node-stage-secret-ref
            :initarg
            :node-stage-secret-ref
            :type
            (or secret-reference null)
            :documentation
            "NodeStageSecretRef is a reference to the secret object containing sensitive information to pass to the CSI driver to complete the CSI NodeStageVolume and NodeStageVolume and NodeUnstageVolume calls. This field is optional, and  may be empty if no secret is required. If the secret object contains more than one secret, all secrets are passed.")
           (volume-attributes
            :initarg
            :volume-attributes
            :type
            (or hash-table null)
            :documentation
            "Attributes of the volume to publish."))
          (:documentation
           "Represents storage that is managed by an external CSI volume driver (Beta feature)"))

(defmethod unmarshal
  ((object csi-persistent-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "controllerPublishSecretRef" source)
    (when present-p
      (setf (slot-value object 'controller-publish-secret-ref)
            (decode-object "SecretReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "nodePublishSecretRef" source)
    (when present-p
      (setf (slot-value object 'node-publish-secret-ref)
            (decode-object "SecretReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeHandle" source)
    (when present-p
      (setf (slot-value object 'volume-handle)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "driver" source)
    (when present-p
      (setf (slot-value object 'driver)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "nodeStageSecretRef" source)
    (when present-p
      (setf (slot-value object 'node-stage-secret-ref)
            (decode-object "SecretReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeAttributes" source)
    (when present-p
      (setf (slot-value object 'volume-attributes)
            (decode-object "object" value)))))


(defclass cinder-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts. More info: https://releases.k8s.io/HEAD/examples/mysql-cinder-pd/README.md")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://releases.k8s.io/HEAD/examples/mysql-cinder-pd/README.md")
           (volume-id
            :initarg
            :volume-id
            :type
            string
            :documentation
            "volume id used to identify the volume in cinder More info: https://releases.k8s.io/HEAD/examples/mysql-cinder-pd/README.md"))
          (:documentation
           "Represents a cinder volume resource in Openstack. A Cinder volume must exist before mounting to a container. The volume must also be in the same region as the kubelet. Cinder volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object cinder-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeID" source)
    (when present-p
      (setf (slot-value object 'volume-id)
            (decode-object "string" value)))))


(defclass component-status
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "ComponentStatus" :allocation :class)
           (conditions
            :initarg
            :conditions
            :type
            list
            :documentation
            "List of component conditions observed")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "ComponentStatus (and ComponentStatusList) holds the cluster validation info."))

(defmethod unmarshal
  ((object component-status) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "conditions" source)
    (when present-p
      (setf (slot-value object 'conditions)
            (decode-object
             (cons "array" "ComponentCondition")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass namespace-spec
          (resource)
          ((finalizers
            :initarg
            :finalizers
            :type
            list
            :documentation
            "Finalizers is an opaque list of values that must be empty to permanently remove object from storage. More info: https://kubernetes.io/docs/tasks/administer-cluster/namespaces/"))
          (:documentation
           "NamespaceSpec describes the attributes on a Namespace."))

(defmethod unmarshal
  ((object namespace-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "finalizers" source)
    (when present-p
      (setf (slot-value object 'finalizers)
            (decode-object (cons "array" "FinalizerName") value)))))


(defclass storage-os-persistent-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or object-reference null)
            :documentation
            "SecretRef specifies the secret to use for obtaining the StorageOS API credentials.  If not specified, default values will be attempted.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (volume-namespace
            :initarg
            :volume-namespace
            :type
            (or string null)
            :documentation
            "VolumeNamespace specifies the scope of the volume within StorageOS.  If no namespace is specified then the Pod's namespace will be used.  This allows the Kubernetes name scoping to be mirrored within StorageOS for tighter integration. Set VolumeName to any name to override the default behaviour. Set to \"default\" if you are not using namespaces within StorageOS. Namespaces that do not pre-exist within StorageOS will be created.")
           (volume-name
            :initarg
            :volume-name
            :type
            (or string null)
            :documentation
            "VolumeName is the human-readable name of the StorageOS volume.  Volume names are only unique within a namespace.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified."))
          (:documentation
           "Represents a StorageOS persistent volume resource."))

(defmethod unmarshal
  ((object storage-os-persistent-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "ObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeNamespace" source)
    (when present-p
      (setf (slot-value object 'volume-namespace)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeName" source)
    (when present-p
      (setf (slot-value object 'volume-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass local-object-reference
          (resource)
          ((name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names"))
          (:documentation
           "LocalObjectReference contains enough information to let you locate the referenced object inside the same namespace."))

(defmethod unmarshal
  ((object local-object-reference) source)
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass node-selector-term
          (resource)
          ((match-fields
            :initarg
            :match-fields
            :type
            list
            :documentation
            "A list of node selector requirements by node's fields.")
           (match-expressions
            :initarg
            :match-expressions
            :type
            list
            :documentation
            "A list of node selector requirements by node's labels."))
          (:documentation
           "A null or empty node selector term matches no objects. The requirements of them are ANDed."))

(defmethod unmarshal
  ((object node-selector-term) source)
  (multiple-value-bind (value present-p)
      (gethash "matchFields" source)
    (when present-p
      (setf (slot-value object 'match-fields)
            (decode-object
             (cons "array" "NodeSelectorRequirement")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "matchExpressions" source)
    (when present-p
      (setf (slot-value object 'match-expressions)
            (decode-object
             (cons "array" "NodeSelectorRequirement")
             value)))))


(defclass volume-mount
          (resource)
          ((mount-path
            :initarg
            :mount-path
            :type
            string
            :documentation
            "Path within the container at which the volume should be mounted.  Must not contain ':'.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Mounted read-only if true, read-write otherwise (false or unspecified). Defaults to false.")
           (sub-path
            :initarg
            :sub-path
            :type
            (or string null)
            :documentation
            "Path within the volume from which the container's volume should be mounted. Defaults to \"\" (volume's root).")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "This must match the Name of a Volume.")
           (mount-propagation
            :initarg
            :mount-propagation
            :type
            (or mount-propagation-mode null)
            :documentation
            "mountPropagation determines how mounts are propagated from the host to container and the other way around. When not set, MountPropagationHostToContainer is used. This field is beta in 1.10."))
          (:documentation
           "VolumeMount describes a mounting of a Volume within a container."))

(defmethod unmarshal
  ((object volume-mount) source)
  (multiple-value-bind (value present-p)
      (gethash "mountPath" source)
    (when present-p
      (setf (slot-value object 'mount-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "subPath" source)
    (when present-p
      (setf (slot-value object 'sub-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "mountPropagation" source)
    (when present-p
      (setf (slot-value object 'mount-propagation)
            (decode-object "MountPropagationMode" value)))))


(defclass persistent-volume-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "PersistentVolumeList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of persistent volumes. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "PersistentVolumeList is a list of PersistentVolume items."))

(defmethod unmarshal
  ((object persistent-volume-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "PersistentVolume") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass pod-dns-config
          (resource)
          ((searches
            :initarg
            :searches
            :type
            list
            :documentation
            "A list of DNS search domains for host-name lookup. This will be appended to the base search paths generated from DNSPolicy. Duplicated search paths will be removed.")
           (options
            :initarg
            :options
            :type
            list
            :documentation
            "A list of DNS resolver options. This will be merged with the base options generated from DNSPolicy. Duplicated entries will be removed. Resolution options given in Options will override those that appear in the base DNSPolicy.")
           (nameservers
            :initarg
            :nameservers
            :type
            list
            :documentation
            "A list of DNS name server IP addresses. This will be appended to the base nameservers generated from DNSPolicy. Duplicated nameservers will be removed."))
          (:documentation
           "PodDNSConfig defines the DNS parameters of a pod in addition to those generated from DNSPolicy."))

(defmethod unmarshal
  ((object pod-dns-config) source)
  (multiple-value-bind (value present-p)
      (gethash "searches" source)
    (when present-p
      (setf (slot-value object 'searches)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "options" source)
    (when present-p
      (setf (slot-value object 'options)
            (decode-object
             (cons "array" "PodDNSConfigOption")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "nameservers" source)
    (when present-p
      (setf (slot-value object 'nameservers)
            (decode-object (cons "array" "string") value)))))


(defclass security-context
          (resource)
          ((capabilities
            :initarg
            :capabilities
            :type
            (or capabilities null)
            :documentation
            "The capabilities to add/drop when running containers. Defaults to the default set of capabilities granted by the container runtime.")
           (run-as-non-root
            :initarg
            :run-as-non-root
            :type
            (or boolean null)
            :documentation
            "Indicates that the container must run as a non-root user. If true, the Kubelet will validate the image at runtime to ensure that it does not run as UID 0 (root) and fail to start the container if it does. If unset or false, no such validation will be performed. May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence.")
           (allow-privilege-escalation
            :initarg
            :allow-privilege-escalation
            :type
            (or boolean null)
            :documentation
            "AllowPrivilegeEscalation controls whether a process can gain more privileges than its parent process. This bool directly controls if the no_new_privs flag will be set on the container process. AllowPrivilegeEscalation is true always when the container is: 1) run as Privileged 2) has CAP_SYS_ADMIN")
           (privileged
            :initarg
            :privileged
            :type
            (or boolean null)
            :documentation
            "Run container in privileged mode. Processes in privileged containers are essentially equivalent to root on the host. Defaults to false.")
           (run-as-group
            :initarg
            :run-as-group
            :type
            (or integer null)
            :documentation
            "The GID to run the entrypoint of the container process. Uses runtime default if unset. May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence.")
           (read-only-root-filesystem
            :initarg
            :read-only-root-filesystem
            :type
            (or boolean null)
            :documentation
            "Whether this container has a read-only root filesystem. Default is false.")
           (se-linux-options
            :initarg
            :se-linux-options
            :type
            (or se-linux-options null)
            :documentation
            "The SELinux context to be applied to the container. If unspecified, the container runtime will allocate a random SELinux context for each container.  May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence.")
           (run-as-user
            :initarg
            :run-as-user
            :type
            (or integer null)
            :documentation
            "The UID to run the entrypoint of the container process. Defaults to user specified in image metadata if unspecified. May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence."))
          (:documentation
           "SecurityContext holds security configuration that will be applied to a container. Some fields are present in both SecurityContext and PodSecurityContext.  When both are set, the values in SecurityContext take precedence."))

(defmethod unmarshal
  ((object security-context) source)
  (multiple-value-bind (value present-p)
      (gethash "capabilities" source)
    (when present-p
      (setf (slot-value object 'capabilities)
            (decode-object "Capabilities" value))))
  (multiple-value-bind (value present-p)
      (gethash "runAsNonRoot" source)
    (when present-p
      (setf (slot-value object 'run-as-non-root)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "allowPrivilegeEscalation" source)
    (when present-p
      (setf (slot-value object 'allow-privilege-escalation)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "privileged" source)
    (when present-p
      (setf (slot-value object 'privileged)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "runAsGroup" source)
    (when present-p
      (setf (slot-value object 'run-as-group)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnlyRootFilesystem" source)
    (when present-p
      (setf (slot-value object 'read-only-root-filesystem)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "seLinuxOptions" source)
    (when present-p
      (setf (slot-value object 'se-linux-options)
            (decode-object "SELinuxOptions" value))))
  (multiple-value-bind (value present-p)
      (gethash "runAsUser" source)
    (when present-p
      (setf (slot-value object 'run-as-user)
            (decode-object (cons "integer" "int64") value)))))


(defclass gce-persistent-disk-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the ReadOnly setting in VolumeMounts. Defaults to false. More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk")
           (pd-name
            :initarg
            :pd-name
            :type
            string
            :documentation
            "Unique name of the PD resource in GCE. Used to identify the disk in GCE. More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk")
           (partition
            :initarg
            :partition
            :type
            (or integer null)
            :documentation
            "The partition in the volume that you want to mount. If omitted, the default is to mount by volume name. Examples: For volume /dev/sda1, you specify the partition as \"1\". Similarly, the volume partition for /dev/sda is \"0\" (or you can leave the property empty). More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type of the volume that you want to mount. Tip: Ensure that the filesystem type is supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk"))
          (:documentation
           "Represents a Persistent Disk resource in Google Compute Engine.

A GCE PD must exist before mounting to a container. The disk must also be in the same GCE project and zone as the kubelet. A GCE PD can only be mounted as read/write once or read-only many times. GCE PDs support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object gce-persistent-disk-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "pdName" source)
    (when present-p
      (setf (slot-value object 'pd-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "partition" source)
    (when present-p
      (setf (slot-value object 'partition)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass secret-list
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "SecretList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is a list of secret objects. More info: https://kubernetes.io/docs/concepts/configuration/secret")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation "SecretList is a list of Secret."))

(defmethod unmarshal
  ((object secret-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "Secret") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass node-spec
          (resource)
          ((unschedulable
            :initarg
            :unschedulable
            :type
            (or boolean null)
            :documentation
            "Unschedulable controls node schedulability of new pods. By default, node is schedulable. More info: https://kubernetes.io/docs/concepts/nodes/node/#manual-node-administration")
           (pod-cidr
            :initarg
            :pod-cidr
            :type
            (or string null)
            :documentation
            "PodCIDR represents the pod IP range assigned to the node.")
           (config-source
            :initarg
            :config-source
            :type
            (or node-config-source null)
            :documentation
            "If specified, the source to get node configuration from The DynamicKubeletConfig feature gate must be enabled for the Kubelet to use this field")
           (provider-id
            :initarg
            :provider-id
            :type
            (or string null)
            :documentation
            "ID of the node assigned by the cloud provider in the format: <ProviderName>://<ProviderSpecificNodeID>")
           (taints
            :initarg
            :taints
            :type
            list
            :documentation
            "If specified, the node's taints.")
           (external-id
            :initarg
            :external-id
            :type
            (or string null)
            :documentation
            "Deprecated. Not all kubelets will set this field. Remove field after 1.13. see: https://issues.k8s.io/61966"))
          (:documentation
           "NodeSpec describes the attributes that a node is created with."))

(defmethod unmarshal
  ((object node-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "unschedulable" source)
    (when present-p
      (setf (slot-value object 'unschedulable)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "podCIDR" source)
    (when present-p
      (setf (slot-value object 'pod-cidr)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "configSource" source)
    (when present-p
      (setf (slot-value object 'config-source)
            (decode-object "NodeConfigSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "providerID" source)
    (when present-p
      (setf (slot-value object 'provider-id)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "taints" source)
    (when present-p
      (setf (slot-value object 'taints)
            (decode-object (cons "array" "Taint") value))))
  (multiple-value-bind (value present-p)
      (gethash "externalID" source)
    (when present-p
      (setf (slot-value object 'external-id)
            (decode-object "string" value)))))


(defclass component-condition
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "Message about the condition for a component. For example, information about a health check.")
           (error :initarg
                  :error
                  :type
                  (or string null)
                  :documentation
                  "Condition error code for a component. For example, a health check error code.")
           (type
            :initarg
            :type
            :type
            string
            :documentation
            "Type of condition for a component. Valid value: \"Healthy\"")
           (status
            :initarg
            :status
            :type
            string
            :documentation
            "Status of the condition for a component. Valid values for \"Healthy\": \"True\", \"False\", or \"Unknown\"."))
          (:documentation
           "Information about the condition of a component."))

(defmethod unmarshal
  ((object component-condition) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "error" source)
    (when present-p
      (setf (slot-value object 'error)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value)))))


(defclass host-alias
          (resource)
          ((hostnames
            :initarg
            :hostnames
            :type
            list
            :documentation
            "Hostnames for the above IP address.")
           (ip
            :initarg
            :ip
            :type
            (or string null)
            :documentation
            "IP address of the host file entry."))
          (:documentation
           "HostAlias holds the mapping between IP and hostnames that will be injected as an entry in the pod's hosts file."))

(defmethod unmarshal
  ((object host-alias) source)
  (multiple-value-bind (value present-p)
      (gethash "hostnames" source)
    (when present-p
      (setf (slot-value object 'hostnames)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "ip" source)
    (when present-p
      (setf (slot-value object 'ip) (decode-object "string" value)))))


(defclass se-linux-options
          (resource)
          ((level
            :initarg
            :level
            :type
            (or string null)
            :documentation
            "Level is SELinux level label that applies to the container.")
           (type
            :initarg
            :type
            :type
            (or string null)
            :documentation
            "Type is a SELinux type label that applies to the container.")
           (role
            :initarg
            :role
            :type
            (or string null)
            :documentation
            "Role is a SELinux role label that applies to the container.")
           (user
            :initarg
            :user
            :type
            (or string null)
            :documentation
            "User is a SELinux user label that applies to the container."))
          (:documentation
           "SELinuxOptions are the labels to be applied to the container"))

(defmethod unmarshal
  ((object se-linux-options) source)
  (multiple-value-bind (value present-p)
      (gethash "level" source)
    (when present-p
      (setf (slot-value object 'level)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "role" source)
    (when present-p
      (setf (slot-value object 'role) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "user" source)
    (when present-p
      (setf (slot-value object 'user) (decode-object "string" value)))))


(defclass load-balancer-status
          (resource)
          ((ingress
            :initarg
            :ingress
            :type
            list
            :documentation
            "Ingress is a list containing ingress points for the load-balancer. Traffic intended for the service should be sent to these ingress points."))
          (:documentation
           "LoadBalancerStatus represents the status of a load-balancer."))

(defmethod unmarshal
  ((object load-balancer-status) source)
  (multiple-value-bind (value present-p)
      (gethash "ingress" source)
    (when present-p
      (setf (slot-value object 'ingress)
            (decode-object
             (cons "array" "LoadBalancerIngress")
             value)))))


(defclass node-status
          (resource)
          ((conditions
            :initarg
            :conditions
            :type
            list
            :documentation
            "Conditions is an array of current observed node conditions. More info: https://kubernetes.io/docs/concepts/nodes/node/#condition")
           (allocatable
            :initarg
            :allocatable
            :type
            (or hash-table null)
            :documentation
            "Allocatable represents the resources of a node that are available for scheduling. Defaults to Capacity.")
           (volumes-in-use
            :initarg
            :volumes-in-use
            :type
            list
            :documentation
            "List of attachable volumes in use (mounted) by the node.")
           (node-info
            :initarg
            :node-info
            :type
            (or node-system-info null)
            :documentation
            "Set of ids/uuids to uniquely identify the node. More info: https://kubernetes.io/docs/concepts/nodes/node/#info")
           (phase :initarg
                  :phase
                  :type
                  (or string null)
                  :documentation
                  "NodePhase is the recently observed lifecycle phase of the node. More info: https://kubernetes.io/docs/concepts/nodes/node/#phase The field is never populated, and now is deprecated.")
           (volumes-attached
            :initarg
            :volumes-attached
            :type
            list
            :documentation
            "List of volumes that are attached to the node.")
           (images
            :initarg
            :images
            :type
            list
            :documentation
            "List of container images on this node")
           (addresses
            :initarg
            :addresses
            :type
            list
            :documentation
            "List of addresses reachable to the node. Queried from cloud provider, if available. More info: https://kubernetes.io/docs/concepts/nodes/node/#addresses")
           (daemon-endpoints
            :initarg
            :daemon-endpoints
            :type
            (or node-daemon-endpoints null)
            :documentation
            "Endpoints of daemons running on the Node.")
           (capacity
            :initarg
            :capacity
            :type
            (or hash-table null)
            :documentation
            "Capacity represents the total resources of a node. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#capacity"))
          (:documentation
           "NodeStatus is information about the current status of a node."))

(defmethod unmarshal
  ((object node-status) source)
  (multiple-value-bind (value present-p)
      (gethash "conditions" source)
    (when present-p
      (setf (slot-value object 'conditions)
            (decode-object (cons "array" "NodeCondition") value))))
  (multiple-value-bind (value present-p)
      (gethash "allocatable" source)
    (when present-p
      (setf (slot-value object 'allocatable)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumesInUse" source)
    (when present-p
      (setf (slot-value object 'volumes-in-use)
            (decode-object (cons "array" "UniqueVolumeName") value))))
  (multiple-value-bind (value present-p)
      (gethash "nodeInfo" source)
    (when present-p
      (setf (slot-value object 'node-info)
            (decode-object "NodeSystemInfo" value))))
  (multiple-value-bind (value present-p)
      (gethash "phase" source)
    (when present-p
      (setf (slot-value object 'phase)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumesAttached" source)
    (when present-p
      (setf (slot-value object 'volumes-attached)
            (decode-object (cons "array" "AttachedVolume") value))))
  (multiple-value-bind (value present-p)
      (gethash "images" source)
    (when present-p
      (setf (slot-value object 'images)
            (decode-object (cons "array" "ContainerImage") value))))
  (multiple-value-bind (value present-p)
      (gethash "addresses" source)
    (when present-p
      (setf (slot-value object 'addresses)
            (decode-object (cons "array" "NodeAddress") value))))
  (multiple-value-bind (value present-p)
      (gethash "daemonEndpoints" source)
    (when present-p
      (setf (slot-value object 'daemon-endpoints)
            (decode-object "NodeDaemonEndpoints" value))))
  (multiple-value-bind (value present-p)
      (gethash "capacity" source)
    (when present-p
      (setf (slot-value object 'capacity)
            (decode-object "object" value)))))


(defclass event-series
          (resource)
          ((last-observed-time
            :initarg
            :last-observed-time
            :type
            (or string null)
            :documentation
            "Time of the last occurrence observed")
           (count :initarg
                  :count
                  :type
                  (or integer null)
                  :documentation
                  "Number of occurrences in this series up to the last heartbeat time")
           (state
            :initarg
            :state
            :type
            (or string null)
            :documentation
            "State of this Series: Ongoing or Finished"))
          (:documentation
           "EventSeries contain information on series of events, i.e. thing that was/is happening continuously for some time."))

(defmethod unmarshal
  ((object event-series) source)
  (multiple-value-bind (value present-p)
      (gethash "lastObservedTime" source)
    (when present-p
      (setf (slot-value object 'last-observed-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "count" source)
    (when present-p
      (setf (slot-value object 'count)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "state" source)
    (when present-p
      (setf (slot-value object 'state)
            (decode-object "string" value)))))


(defclass probe
          (resource)
          ((exec
            :initarg
            :exec
            :type
            (or exec-action null)
            :documentation
            "One and only one of the following should be specified. Exec specifies the action to take.")
           (http-get
            :initarg
            :http-get
            :type
            (or http-get-action null)
            :documentation
            "HTTPGet specifies the http request to perform.")
           (timeout-seconds
            :initarg
            :timeout-seconds
            :type
            (or integer null)
            :documentation
            "Number of seconds after which the probe times out. Defaults to 1 second. Minimum value is 1. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes")
           (failure-threshold
            :initarg
            :failure-threshold
            :type
            (or integer null)
            :documentation
            "Minimum consecutive failures for the probe to be considered failed after having succeeded. Defaults to 3. Minimum value is 1.")
           (success-threshold
            :initarg
            :success-threshold
            :type
            (or integer null)
            :documentation
            "Minimum consecutive successes for the probe to be considered successful after having failed. Defaults to 1. Must be 1 for liveness. Minimum value is 1.")
           (tcp-socket
            :initarg
            :tcp-socket
            :type
            (or tcp-socket-action null)
            :documentation
            "TCPSocket specifies an action involving a TCP port. TCP hooks not yet supported")
           (initial-delay-seconds
            :initarg
            :initial-delay-seconds
            :type
            (or integer null)
            :documentation
            "Number of seconds after the container has started before liveness probes are initiated. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes")
           (period-seconds
            :initarg
            :period-seconds
            :type
            (or integer null)
            :documentation
            "How often (in seconds) to perform the probe. Default to 10 seconds. Minimum value is 1."))
          (:documentation
           "Probe describes a health check to be performed against a container to determine whether it is alive or ready to receive traffic."))

(defmethod unmarshal
  ((object probe) source)
  (multiple-value-bind (value present-p)
      (gethash "exec" source)
    (when present-p
      (setf (slot-value object 'exec)
            (decode-object "ExecAction" value))))
  (multiple-value-bind (value present-p)
      (gethash "httpGet" source)
    (when present-p
      (setf (slot-value object 'http-get)
            (decode-object "HTTPGetAction" value))))
  (multiple-value-bind (value present-p)
      (gethash "timeoutSeconds" source)
    (when present-p
      (setf (slot-value object 'timeout-seconds)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "failureThreshold" source)
    (when present-p
      (setf (slot-value object 'failure-threshold)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "successThreshold" source)
    (when present-p
      (setf (slot-value object 'success-threshold)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "tcpSocket" source)
    (when present-p
      (setf (slot-value object 'tcp-socket)
            (decode-object "TCPSocketAction" value))))
  (multiple-value-bind (value present-p)
      (gethash "initialDelaySeconds" source)
    (when present-p
      (setf (slot-value object 'initial-delay-seconds)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "periodSeconds" source)
    (when present-p
      (setf (slot-value object 'period-seconds)
            (decode-object (cons "integer" "int32") value)))))


(defclass azure-data-disk-caching-mode
          (resource)
          nil
          (:documentation nil))

(defmethod unmarshal ((object azure-data-disk-caching-mode) source))


(defclass persistent-volume
          (resource)
          ((api-version :initform "v1" :allocation :class)
           (kind :initform "PersistentVolume" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or persistent-volume-status null)
            :documentation
            "Status represents the current information/status for the persistent volume. Populated by the system. Read-only. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistent-volumes")
           (spec
            :initarg
            :spec
            :type
            (or persistent-volume-spec null)
            :documentation
            "Spec defines a specification of a persistent volume owned by the cluster. Provisioned by an administrator. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistent-volumes")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "PersistentVolume (PV) is a storage resource provisioned by an administrator. It is analogous to a node. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes"))

(defmethod unmarshal
  ((object persistent-volume) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "PersistentVolumeStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "PersistentVolumeSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass container-state-waiting
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "Message regarding why the container is not yet running.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "(brief) reason the container is not yet running."))
          (:documentation
           "ContainerStateWaiting is a waiting state of a container."))

(defmethod unmarshal
  ((object container-state-waiting) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value)))))


(defclass flex-persistent-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or secret-reference null)
            :documentation
            "Optional: SecretRef is reference to the secret object containing sensitive information to pass to the plugin scripts. This may be empty if no secret object is specified. If the secret object contains more than one secret, all secrets are passed to the plugin scripts.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (options
            :initarg
            :options
            :type
            (or hash-table null)
            :documentation
            "Optional: Extra command options if any.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". The default filesystem depends on FlexVolume script.")
           (driver
            :initarg
            :driver
            :type
            string
            :documentation
            "Driver is the name of the driver to use for this volume."))
          (:documentation
           "FlexPersistentVolumeSource represents a generic persistent volume resource that is provisioned/attached using an exec based plugin."))

(defmethod unmarshal
  ((object flex-persistent-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "SecretReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "options" source)
    (when present-p
      (setf (slot-value object 'options)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "driver" source)
    (when present-p
      (setf (slot-value object 'driver)
            (decode-object "string" value)))))


(defclass persistent-volume-claim
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "PersistentVolumeClaim" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or persistent-volume-claim-status null)
            :documentation
            "Status represents the current information/status of a persistent volume claim. Read-only. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistentvolumeclaims")
           (spec
            :initarg
            :spec
            :type
            (or persistent-volume-claim-spec null)
            :documentation
            "Spec defines the desired characteristics of a volume requested by a pod author. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistentvolumeclaims")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "PersistentVolumeClaim is a user's request for and claim to a persistent volume"))

(defmethod unmarshal
  ((object persistent-volume-claim) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "PersistentVolumeClaimStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "PersistentVolumeClaimSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass env-var
          (resource)
          ((value-from
            :initarg
            :value-from
            :type
            (or env-var-source null)
            :documentation
            "Source for the environment variable's value. Cannot be used if value is not empty.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "Name of the environment variable. Must be a C_IDENTIFIER.")
           (value
            :initarg
            :value
            :type
            (or string null)
            :documentation
            "Variable references $(VAR_NAME) are expanded using the previous defined environment variables in the container and any service environment variables. If a variable cannot be resolved, the reference in the input string will be unchanged. The $(VAR_NAME) syntax can be escaped with a double $$, ie: $$(VAR_NAME). Escaped references will never be expanded, regardless of whether the variable exists or not. Defaults to \"\"."))
          (:documentation
           "EnvVar represents an environment variable present in a Container."))

(defmethod unmarshal
  ((object env-var) source)
  (multiple-value-bind (value present-p)
      (gethash "valueFrom" source)
    (when present-p
      (setf (slot-value object 'value-from)
            (decode-object "EnvVarSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "value" source)
    (when present-p
      (setf (slot-value object 'value)
            (decode-object "string" value)))))


(defclass controller-revision-list
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "ControllerRevisionList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is the list of ControllerRevisions")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "ControllerRevisionList is a resource containing a list of ControllerRevision objects."))

(defmethod unmarshal
  ((object controller-revision-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object
             (cons "array" "ControllerRevision")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass config-map-key-selector
          (resource)
          ((key
            :initarg
            :key
            :type
            string
            :documentation
            "The key to select.")
           (optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the ConfigMap or it's key must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names"))
          (:documentation "Selects a key from a ConfigMap."))

(defmethod unmarshal
  ((object config-map-key-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass daemon-set-list
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "DaemonSetList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "A list of daemon sets.")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "DaemonSetList is a collection of daemon sets."))

(defmethod unmarshal
  ((object daemon-set-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "DaemonSet") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass env-var-source
          (resource)
          ((secret-key-ref
            :initarg
            :secret-key-ref
            :type
            (or secret-key-selector null)
            :documentation
            "Selects a key of a secret in the pod's namespace")
           (config-map-key-ref
            :initarg
            :config-map-key-ref
            :type
            (or config-map-key-selector null)
            :documentation
            "Selects a key of a ConfigMap.")
           (field-ref
            :initarg
            :field-ref
            :type
            (or object-field-selector null)
            :documentation
            "Selects a field of the pod: supports metadata.name, metadata.namespace, metadata.labels, metadata.annotations, spec.nodeName, spec.serviceAccountName, status.hostIP, status.podIP.")
           (resource-field-ref
            :initarg
            :resource-field-ref
            :type
            (or resource-field-selector null)
            :documentation
            "Selects a resource of the container: only resources limits and requests (limits.cpu, limits.memory, limits.ephemeral-storage, requests.cpu, requests.memory and requests.ephemeral-storage) are currently supported."))
          (:documentation
           "EnvVarSource represents a source for the value of an EnvVar."))

(defmethod unmarshal
  ((object env-var-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretKeyRef" source)
    (when present-p
      (setf (slot-value object 'secret-key-ref)
            (decode-object "SecretKeySelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "configMapKeyRef" source)
    (when present-p
      (setf (slot-value object 'config-map-key-ref)
            (decode-object "ConfigMapKeySelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "fieldRef" source)
    (when present-p
      (setf (slot-value object 'field-ref)
            (decode-object "ObjectFieldSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "resourceFieldRef" source)
    (when present-p
      (setf (slot-value object 'resource-field-ref)
            (decode-object "ResourceFieldSelector" value)))))


(defclass aws-elastic-block-store-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Specify \"true\" to force and set the ReadOnly property in VolumeMounts to \"true\". If omitted, the default is \"false\". More info: https://kubernetes.io/docs/concepts/storage/volumes#awselasticblockstore")
           (partition
            :initarg
            :partition
            :type
            (or integer null)
            :documentation
            "The partition in the volume that you want to mount. If omitted, the default is to mount by volume name. Examples: For volume /dev/sda1, you specify the partition as \"1\". Similarly, the volume partition for /dev/sda is \"0\" (or you can leave the property empty).")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type of the volume that you want to mount. Tip: Ensure that the filesystem type is supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://kubernetes.io/docs/concepts/storage/volumes#awselasticblockstore")
           (volume-id
            :initarg
            :volume-id
            :type
            string
            :documentation
            "Unique ID of the persistent disk resource in AWS (Amazon EBS volume). More info: https://kubernetes.io/docs/concepts/storage/volumes#awselasticblockstore"))
          (:documentation
           "Represents a Persistent Disk resource in AWS.

An AWS EBS disk must exist before mounting to a container. The disk must also be in the same AWS zone as the kubelet. An AWS EBS disk can only be mounted as read/write once. AWS EBS volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object aws-elastic-block-store-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "partition" source)
    (when present-p
      (setf (slot-value object 'partition)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeID" source)
    (when present-p
      (setf (slot-value object 'volume-id)
            (decode-object "string" value)))))


(defclass rolling-update-daemon-set
          (resource)
          ((max-unavailable
            :initarg
            :max-unavailable
            :type
            (or string null)
            :documentation
            "The maximum number of DaemonSet pods that can be unavailable during the update. Value can be an absolute number (ex: 5) or a percentage of total number of DaemonSet pods at the start of the update (ex: 10%). Absolute number is calculated from percentage by rounding up. This cannot be 0. Default value is 1. Example: when this is set to 30%, at most 30% of the total number of nodes that should be running the daemon pod (i.e. status.desiredNumberScheduled) can have their pods stopped for an update at any given time. The update starts by stopping at most 30% of those DaemonSet pods and then brings up new DaemonSet pods in their place. Once the new pods are available, it then proceeds onto other DaemonSet pods, thus ensuring that at least 70% of original number of DaemonSet pods are available at all times during the update."))
          (:documentation
           "Spec to control the desired behavior of daemon set rolling update."))

(defmethod unmarshal
  ((object rolling-update-daemon-set) source)
  (multiple-value-bind (value present-p)
      (gethash "maxUnavailable" source)
    (when present-p
      (setf (slot-value object 'max-unavailable)
            (decode-object "string" value)))))


(defclass http-header
          (resource)
          ((name
            :initarg
            :name
            :type
            string
            :documentation
            "The header field name")
           (value
            :initarg
            :value
            :type
            string
            :documentation
            "The header field value"))
          (:documentation
           "HTTPHeader describes a custom header to be used in HTTP probes"))

(defmethod unmarshal
  ((object http-header) source)
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "value" source)
    (when present-p
      (setf (slot-value object 'value)
            (decode-object "string" value)))))


(defclass list-meta
          (resource)
          ((self-link
            :initarg
            :self-link
            :type
            (or string null)
            :documentation
            "selfLink is a URL representing this object. Populated by the system. Read-only.")
           (continue :initarg
                     :continue
                     :type
                     (or string null)
                     :documentation
                     "continue may be set if the user set a limit on the number of items returned, and indicates that the server has more data available. The value is opaque and may be used to issue another request to the endpoint that served this list to retrieve the next set of available objects. Continuing a list may not be possible if the server configuration has changed or more than a few minutes have passed. The resourceVersion field returned when using this continue value will be identical to the value in the first response.")
           (resource-version
            :initarg
            :resource-version
            :type
            (or string null)
            :documentation
            "String that identifies the server's internal version of this object that can be used by clients to determine when objects have changed. Value must be treated as opaque by clients and passed unmodified back to the server. Populated by the system. Read-only. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#concurrency-control-and-consistency"))
          (:documentation
           "ListMeta describes metadata that synthetic resources must have, including lists and various status objects. A resource may have only one of {ObjectMeta, ListMeta}."))

(defmethod unmarshal
  ((object list-meta) source)
  (multiple-value-bind (value present-p)
      (gethash "selfLink" source)
    (when present-p
      (setf (slot-value object 'self-link)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "continue" source)
    (when present-p
      (setf (slot-value object 'continue)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "resourceVersion" source)
    (when present-p
      (setf (slot-value object 'resource-version)
            (decode-object "string" value)))))


(defclass vsphere-virtual-disk-volume-source
          (resource)
          ((storage-policy-name
            :initarg
            :storage-policy-name
            :type
            (or string null)
            :documentation
            "Storage Policy Based Management (SPBM) profile name.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified.")
           (storage-policy-id
            :initarg
            :storage-policy-id
            :type
            (or string null)
            :documentation
            "Storage Policy Based Management (SPBM) profile ID associated with the StoragePolicyName.")
           (volume-path
            :initarg
            :volume-path
            :type
            string
            :documentation
            "Path that identifies vSphere volume vmdk"))
          (:documentation "Represents a vSphere volume resource."))

(defmethod unmarshal
  ((object vsphere-virtual-disk-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "storagePolicyName" source)
    (when present-p
      (setf (slot-value object 'storage-policy-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "storagePolicyID" source)
    (when present-p
      (setf (slot-value object 'storage-policy-id)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumePath" source)
    (when present-p
      (setf (slot-value object 'volume-path)
            (decode-object "string" value)))))


(defclass deployment-spec
          (resource)
          ((selector
            :initarg
            :selector
            :type
            label-selector
            :documentation
            "Label selector for pods. Existing ReplicaSets whose pods are selected by this will be the ones affected by this deployment. It must match the pod template's labels.")
           (progress-deadline-seconds
            :initarg
            :progress-deadline-seconds
            :type
            (or integer null)
            :documentation
            "The maximum time in seconds for a deployment to make progress before it is considered to be failed. The deployment controller will continue to process failed deployments and a condition with a ProgressDeadlineExceeded reason will be surfaced in the deployment status. Note that progress will not be estimated during the time a deployment is paused. Defaults to 600s.")
           (strategy
            :initarg
            :strategy
            :type
            (or deployment-strategy null)
            :documentation
            "The deployment strategy to use to replace existing pods with new ones.")
           (replicas
            :initarg
            :replicas
            :type
            (or integer null)
            :documentation
            "Number of desired pods. This is a pointer to distinguish between explicit zero and not specified. Defaults to 1.")
           (revision-history-limit
            :initarg
            :revision-history-limit
            :type
            (or integer null)
            :documentation
            "The number of old ReplicaSets to retain to allow rollback. This is a pointer to distinguish between explicit zero and not specified. Defaults to 10.")
           (template
            :initarg
            :template
            :type
            pod-template-spec
            :documentation
            "Template describes the pods that will be created.")
           (min-ready-seconds
            :initarg
            :min-ready-seconds
            :type
            (or integer null)
            :documentation
            "Minimum number of seconds for which a newly created pod should be ready without any of its container crashing, for it to be considered available. Defaults to 0 (pod will be considered available as soon as it is ready)")
           (paused
            :initarg
            :paused
            :type
            (or boolean null)
            :documentation
            "Indicates that the deployment is paused."))
          (:documentation
           "DeploymentSpec is the specification of the desired behavior of the Deployment."))

(defmethod unmarshal
  ((object deployment-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "selector" source)
    (when present-p
      (setf (slot-value object 'selector)
            (decode-object "LabelSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "progressDeadlineSeconds" source)
    (when present-p
      (setf (slot-value object 'progress-deadline-seconds)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "strategy" source)
    (when present-p
      (setf (slot-value object 'strategy)
            (decode-object "DeploymentStrategy" value))))
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "revisionHistoryLimit" source)
    (when present-p
      (setf (slot-value object 'revision-history-limit)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "template" source)
    (when present-p
      (setf (slot-value object 'template)
            (decode-object "PodTemplateSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "minReadySeconds" source)
    (when present-p
      (setf (slot-value object 'min-ready-seconds)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "paused" source)
    (when present-p
      (setf (slot-value object 'paused)
            (decode-object "boolean" value)))))


(defclass pod-anti-affinity
          (resource)
          ((preferred-during-scheduling-ignored-during-execution
            :initarg
            :preferred-during-scheduling-ignored-during-execution
            :type
            list
            :documentation
            "The scheduler will prefer to schedule pods to nodes that satisfy the anti-affinity expressions specified by this field, but it may choose a node that violates one or more of the expressions. The node that is most preferred is the one with the greatest sum of weights, i.e. for each node that meets all of the scheduling requirements (resource request, requiredDuringScheduling anti-affinity expressions, etc.), compute a sum by iterating through the elements of this field and adding \"weight\" to the sum if the node has pods which matches the corresponding podAffinityTerm; the node(s) with the highest sum are the most preferred.")
           (required-during-scheduling-ignored-during-execution
            :initarg
            :required-during-scheduling-ignored-during-execution
            :type
            list
            :documentation
            "If the anti-affinity requirements specified by this field are not met at scheduling time, the pod will not be scheduled onto the node. If the anti-affinity requirements specified by this field cease to be met at some point during pod execution (e.g. due to a pod label update), the system may or may not try to eventually evict the pod from its node. When there are multiple elements, the lists of nodes corresponding to each podAffinityTerm are intersected, i.e. all terms must be satisfied."))
          (:documentation
           "Pod anti affinity is a group of inter pod anti affinity scheduling rules."))

(defmethod unmarshal
  ((object pod-anti-affinity) source)
  (multiple-value-bind (value present-p)
      (gethash "preferredDuringSchedulingIgnoredDuringExecution"
               source)
    (when present-p
      (setf (slot-value object
                        'preferred-during-scheduling-ignored-during-execution)
            (decode-object
             (cons "array" "WeightedPodAffinityTerm")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "requiredDuringSchedulingIgnoredDuringExecution" source)
    (when present-p
      (setf (slot-value object
                        'required-during-scheduling-ignored-during-execution)
            (decode-object (cons "array" "PodAffinityTerm") value)))))


(defclass security-context
          (resource)
          ((capabilities
            :initarg
            :capabilities
            :type
            (or capabilities null)
            :documentation
            "The capabilities to add/drop when running containers. Defaults to the default set of capabilities granted by the container runtime.")
           (run-as-non-root
            :initarg
            :run-as-non-root
            :type
            (or boolean null)
            :documentation
            "Indicates that the container must run as a non-root user. If true, the Kubelet will validate the image at runtime to ensure that it does not run as UID 0 (root) and fail to start the container if it does. If unset or false, no such validation will be performed. May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence.")
           (allow-privilege-escalation
            :initarg
            :allow-privilege-escalation
            :type
            (or boolean null)
            :documentation
            "AllowPrivilegeEscalation controls whether a process can gain more privileges than its parent process. This bool directly controls if the no_new_privs flag will be set on the container process. AllowPrivilegeEscalation is true always when the container is: 1) run as Privileged 2) has CAP_SYS_ADMIN")
           (privileged
            :initarg
            :privileged
            :type
            (or boolean null)
            :documentation
            "Run container in privileged mode. Processes in privileged containers are essentially equivalent to root on the host. Defaults to false.")
           (run-as-group
            :initarg
            :run-as-group
            :type
            (or integer null)
            :documentation
            "The GID to run the entrypoint of the container process. Uses runtime default if unset. May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence.")
           (read-only-root-filesystem
            :initarg
            :read-only-root-filesystem
            :type
            (or boolean null)
            :documentation
            "Whether this container has a read-only root filesystem. Default is false.")
           (se-linux-options
            :initarg
            :se-linux-options
            :type
            (or se-linux-options null)
            :documentation
            "The SELinux context to be applied to the container. If unspecified, the container runtime will allocate a random SELinux context for each container.  May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence.")
           (run-as-user
            :initarg
            :run-as-user
            :type
            (or integer null)
            :documentation
            "The UID to run the entrypoint of the container process. Defaults to user specified in image metadata if unspecified. May also be set in PodSecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence."))
          (:documentation
           "SecurityContext holds security configuration that will be applied to a container. Some fields are present in both SecurityContext and PodSecurityContext.  When both are set, the values in SecurityContext take precedence."))

(defmethod unmarshal
  ((object security-context) source)
  (multiple-value-bind (value present-p)
      (gethash "capabilities" source)
    (when present-p
      (setf (slot-value object 'capabilities)
            (decode-object "Capabilities" value))))
  (multiple-value-bind (value present-p)
      (gethash "runAsNonRoot" source)
    (when present-p
      (setf (slot-value object 'run-as-non-root)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "allowPrivilegeEscalation" source)
    (when present-p
      (setf (slot-value object 'allow-privilege-escalation)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "privileged" source)
    (when present-p
      (setf (slot-value object 'privileged)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "runAsGroup" source)
    (when present-p
      (setf (slot-value object 'run-as-group)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnlyRootFilesystem" source)
    (when present-p
      (setf (slot-value object 'read-only-root-filesystem)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "seLinuxOptions" source)
    (when present-p
      (setf (slot-value object 'se-linux-options)
            (decode-object "SELinuxOptions" value))))
  (multiple-value-bind (value present-p)
      (gethash "runAsUser" source)
    (when present-p
      (setf (slot-value object 'run-as-user)
            (decode-object (cons "integer" "int64") value)))))


(defclass fc-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (target-ww-ns
            :initarg
            :target-ww-ns
            :type
            list
            :documentation
            "Optional: FC target worldwide names (WWNs)")
           (lun
            :initarg
            :lun
            :type
            (or integer null)
            :documentation
            "Optional: FC target lun number")
           (wwids
            :initarg
            :wwids
            :type
            list
            :documentation
            "Optional: FC volume world wide identifiers (wwids) Either wwids or combination of targetWWNs and lun must be set, but not both simultaneously.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified."))
          (:documentation
           "Represents a Fibre Channel volume. Fibre Channel volumes can only be mounted as read/write once. Fibre Channel volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object fc-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "targetWWNs" source)
    (when present-p
      (setf (slot-value object 'target-ww-ns)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "lun" source)
    (when present-p
      (setf (slot-value object 'lun)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "wwids" source)
    (when present-p
      (setf (slot-value object 'wwids)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass secret-volume-source
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the Secret or it's keys must be defined")
           (secret-name
            :initarg
            :secret-name
            :type
            (or string null)
            :documentation
            "Name of the secret in the pod's namespace to use. More info: https://kubernetes.io/docs/concepts/storage/volumes#secret")
           (default-mode
            :initarg
            :default-mode
            :type
            (or integer null)
            :documentation
            "Optional: mode bits to use on created files by default. Must be a value between 0 and 0777. Defaults to 0644. Directories within the path are not affected by this setting. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set.")
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "If unspecified, each key-value pair in the Data field of the referenced Secret will be projected into the volume as a file whose name is the key and content is the value. If specified, the listed keys will be projected into the specified paths, and unlisted keys will not be present. If a key is specified which is not present in the Secret, the volume setup will error unless it is marked optional. Paths must be relative and may not contain the '..' path or start with '..'."))
          (:documentation
           "Adapts a Secret into a volume.

The contents of the target Secret's Data field will be presented in a volume as files using the keys in the Data field as the file names. Secret volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object secret-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "secretName" source)
    (when present-p
      (setf (slot-value object 'secret-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "defaultMode" source)
    (when present-p
      (setf (slot-value object 'default-mode)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "KeyToPath") value)))))


(defclass rolling-update-deployment
          (resource)
          ((max-surge
            :initarg
            :max-surge
            :type
            (or string null)
            :documentation
            "The maximum number of pods that can be scheduled above the desired number of pods. Value can be an absolute number (ex: 5) or a percentage of desired pods (ex: 10%). This can not be 0 if MaxUnavailable is 0. Absolute number is calculated from percentage by rounding up. Defaults to 25%. Example: when this is set to 30%, the new ReplicaSet can be scaled up immediately when the rolling update starts, such that the total number of old and new pods do not exceed 130% of desired pods. Once old pods have been killed, new ReplicaSet can be scaled up further, ensuring that total number of pods running at any time during the update is at most 130% of desired pods.")
           (max-unavailable
            :initarg
            :max-unavailable
            :type
            (or string null)
            :documentation
            "The maximum number of pods that can be unavailable during the update. Value can be an absolute number (ex: 5) or a percentage of desired pods (ex: 10%). Absolute number is calculated from percentage by rounding down. This can not be 0 if MaxSurge is 0. Defaults to 25%. Example: when this is set to 30%, the old ReplicaSet can be scaled down to 70% of desired pods immediately when the rolling update starts. Once new pods are ready, old ReplicaSet can be scaled down further, followed by scaling up the new ReplicaSet, ensuring that the total number of pods available at all times during the update is at least 70% of desired pods."))
          (:documentation
           "Spec to control the desired behavior of rolling update."))

(defmethod unmarshal
  ((object rolling-update-deployment) source)
  (multiple-value-bind (value present-p)
      (gethash "maxSurge" source)
    (when present-p
      (setf (slot-value object 'max-surge)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "maxUnavailable" source)
    (when present-p
      (setf (slot-value object 'max-unavailable)
            (decode-object "string" value)))))


(defclass deployment-condition
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human readable message indicating details about the transition.")
           (last-transition-time
            :initarg
            :last-transition-time
            :type
            (or string null)
            :documentation
            "Last time the condition transitioned from one status to another.")
           (type
            :initarg
            :type
            :type
            string
            :documentation
            "Type of deployment condition.")
           (status
            :initarg
            :status
            :type
            string
            :documentation
            "Status of the condition, one of True, False, Unknown.")
           (last-update-time
            :initarg
            :last-update-time
            :type
            (or string null)
            :documentation
            "The last time this condition was updated.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "The reason for the condition's last transition."))
          (:documentation
           "DeploymentCondition describes the state of a deployment at a certain point."))

(defmethod unmarshal
  ((object deployment-condition) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastTransitionTime" source)
    (when present-p
      (setf (slot-value object 'last-transition-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastUpdateTime" source)
    (when present-p
      (setf (slot-value object 'last-update-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value)))))


(defclass capabilities
          (resource)
          ((drop
            :initarg
            :drop
            :type
            list
            :documentation
            "Removed capabilities")
           (add
            :initarg
            :add
            :type
            list
            :documentation
            "Added capabilities"))
          (:documentation
           "Adds and removes POSIX capabilities from running containers."))

(defmethod unmarshal
  ((object capabilities) source)
  (multiple-value-bind (value present-p)
      (gethash "drop" source)
    (when present-p
      (setf (slot-value object 'drop)
            (decode-object (cons "array" "Capability") value))))
  (multiple-value-bind (value present-p)
      (gethash "add" source)
    (when present-p
      (setf (slot-value object 'add)
            (decode-object (cons "array" "Capability") value)))))


(defclass object-field-selector
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (field-path
            :initarg
            :field-path
            :type
            string
            :documentation
            "Path of the field to select in the specified API version."))
          (:documentation
           "ObjectFieldSelector selects an APIVersioned field of an object."))

(defmethod unmarshal
  ((object object-field-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "fieldPath" source)
    (when present-p
      (setf (slot-value object 'field-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value)))))


(defclass label-selector-requirement
          (resource)
          ((key
            :initarg
            :key
            :type
            string
            :documentation
            "key is the label key that the selector applies to.")
           (values :initarg
                   :values
                   :type
                   list
                   :documentation
                   "values is an array of string values. If the operator is In or NotIn, the values array must be non-empty. If the operator is Exists or DoesNotExist, the values array must be empty. This array is replaced during a strategic merge patch.")
           (operator
            :initarg
            :operator
            :type
            string
            :documentation
            "operator represents a key's relationship to a set of values. Valid operators are In, NotIn, Exists and DoesNotExist."))
          (:documentation
           "A label selector requirement is a selector that contains values, a key, and an operator that relates the key and values."))

(defmethod unmarshal
  ((object label-selector-requirement) source)
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "values" source)
    (when present-p
      (setf (slot-value object 'values)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "operator" source)
    (when present-p
      (setf (slot-value object 'operator)
            (decode-object "string" value)))))


(defclass azure-file-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (share-name
            :initarg
            :share-name
            :type
            string
            :documentation
            "Share Name")
           (secret-name
            :initarg
            :secret-name
            :type
            string
            :documentation
            "the name of secret that contains Azure Storage Account Name and Key"))
          (:documentation
           "AzureFile represents an Azure File Service mount on the host and bind mount to the pod."))

(defmethod unmarshal
  ((object azure-file-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "shareName" source)
    (when present-p
      (setf (slot-value object 'share-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "secretName" source)
    (when present-p
      (setf (slot-value object 'secret-name)
            (decode-object "string" value)))))


(defclass patch
          (resource)
          nil
          (:documentation
           "Patch is provided to give a concrete name and type to the Kubernetes PATCH request body."))

(defmethod unmarshal ((object patch) source))


(defclass scale
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "Scale" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or scale-status null)
            :documentation
            "current status of the scale. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status. Read-only.")
           (spec
            :initarg
            :spec
            :type
            (or scale-spec null)
            :documentation
            "defines the behavior of the scale. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status.")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object metadata; More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata."))
          (:documentation
           "Scale represents a scaling request for a resource."))

(defmethod unmarshal
  ((object scale) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "ScaleStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "ScaleSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass daemon-set-condition
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human readable message indicating details about the transition.")
           (last-transition-time
            :initarg
            :last-transition-time
            :type
            (or string null)
            :documentation
            "Last time the condition transitioned from one status to another.")
           (type
            :initarg
            :type
            :type
            string
            :documentation
            "Type of DaemonSet condition.")
           (status
            :initarg
            :status
            :type
            string
            :documentation
            "Status of the condition, one of True, False, Unknown.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "The reason for the condition's last transition."))
          (:documentation
           "DaemonSetCondition describes the state of a DaemonSet at a certain point."))

(defmethod unmarshal
  ((object daemon-set-condition) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastTransitionTime" source)
    (when present-p
      (setf (slot-value object 'last-transition-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value)))))


(defclass status-cause
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human-readable description of the cause of the error.  This field may be presented as-is to a reader.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "A machine-readable description of the cause of the error. If this value is empty there is no information available.")
           (field
            :initarg
            :field
            :type
            (or string null)
            :documentation
            "The field of the resource that has caused this error, as named by its JSON serialization. May include dot and postfix notation for nested attributes. Arrays are zero-indexed.  Fields may appear more than once in an array of causes due to fields having multiple errors. Optional.

Examples:
  \"name\" - the field \"name\" on the current resource
  \"items[0].name\" - the field \"name\" on the first array entry in \"items\""))
          (:documentation
           "StatusCause provides more information about an api.Status failure, including cases when multiple errors are encountered."))

(defmethod unmarshal
  ((object status-cause) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "field" source)
    (when present-p
      (setf (slot-value object 'field)
            (decode-object "string" value)))))


(defclass label-selector
          (resource)
          ((match-labels
            :initarg
            :match-labels
            :type
            (or hash-table null)
            :documentation
            "matchLabels is a map of {key,value} pairs. A single {key,value} in the matchLabels map is equivalent to an element of matchExpressions, whose key field is \"key\", the operator is \"In\", and the values array contains only \"value\". The requirements are ANDed.")
           (match-expressions
            :initarg
            :match-expressions
            :type
            list
            :documentation
            "matchExpressions is a list of label selector requirements. The requirements are ANDed."))
          (:documentation
           "A label selector is a label query over a set of resources. The result of matchLabels and matchExpressions are ANDed. An empty label selector matches all objects. A null label selector matches no objects."))

(defmethod unmarshal
  ((object label-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "matchLabels" source)
    (when present-p
      (setf (slot-value object 'match-labels)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "matchExpressions" source)
    (when present-p
      (setf (slot-value object 'match-expressions)
            (decode-object
             (cons "array" "LabelSelectorRequirement")
             value)))))


(defclass portworx-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "FSType represents the filesystem type to mount Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\". Implicitly inferred to be \"ext4\" if unspecified.")
           (volume-id
            :initarg
            :volume-id
            :type
            string
            :documentation
            "VolumeID uniquely identifies a Portworx volume"))
          (:documentation
           "PortworxVolumeSource represents a Portworx volume resource."))

(defmethod unmarshal
  ((object portworx-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeID" source)
    (when present-p
      (setf (slot-value object 'volume-id)
            (decode-object "string" value)))))


(defclass scale-io-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            local-object-reference
            :documentation
            "SecretRef references to the secret for ScaleIO user and other sensitive information. If this is not provided, Login operation will fail.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (protection-domain
            :initarg
            :protection-domain
            :type
            (or string null)
            :documentation
            "The name of the ScaleIO Protection Domain for the configured storage.")
           (storage-pool
            :initarg
            :storage-pool
            :type
            (or string null)
            :documentation
            "The ScaleIO Storage Pool associated with the protection domain.")
           (ssl-enabled
            :initarg
            :ssl-enabled
            :type
            (or boolean null)
            :documentation
            "Flag to enable/disable SSL communication with Gateway, default false")
           (volume-name
            :initarg
            :volume-name
            :type
            (or string null)
            :documentation
            "The name of a volume already created in the ScaleIO system that is associated with this volume source.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified.")
           (storage-mode
            :initarg
            :storage-mode
            :type
            (or string null)
            :documentation
            "Indicates whether the storage for a volume should be ThickProvisioned or ThinProvisioned.")
           (system
            :initarg
            :system
            :type
            string
            :documentation
            "The name of the storage system as configured in ScaleIO.")
           (gateway
            :initarg
            :gateway
            :type
            string
            :documentation
            "The host address of the ScaleIO API Gateway."))
          (:documentation
           "ScaleIOVolumeSource represents a persistent ScaleIO volume"))

(defmethod unmarshal
  ((object scale-io-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "protectionDomain" source)
    (when present-p
      (setf (slot-value object 'protection-domain)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "storagePool" source)
    (when present-p
      (setf (slot-value object 'storage-pool)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "sslEnabled" source)
    (when present-p
      (setf (slot-value object 'ssl-enabled)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeName" source)
    (when present-p
      (setf (slot-value object 'volume-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "storageMode" source)
    (when present-p
      (setf (slot-value object 'storage-mode)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "system" source)
    (when present-p
      (setf (slot-value object 'system)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "gateway" source)
    (when present-p
      (setf (slot-value object 'gateway)
            (decode-object "string" value)))))


(defclass owner-reference
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "OwnerReference" :allocation :class)
           (block-owner-deletion
            :initarg
            :block-owner-deletion
            :type
            (or boolean null)
            :documentation
            "If true, AND if the owner has the \"foregroundDeletion\" finalizer, then the owner cannot be deleted from the key-value store until this reference is removed. Defaults to false. To set this field, a user needs \"delete\" permission of the owner, otherwise 422 (Unprocessable Entity) will be returned.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "Name of the referent. More info: http://kubernetes.io/docs/user-guide/identifiers#names")
           (controller
            :initarg
            :controller
            :type
            (or boolean null)
            :documentation
            "If true, this reference points to the managing controller.")
           (uid
            :initarg
            :uid
            :type
            string
            :documentation
            "UID of the referent. More info: http://kubernetes.io/docs/user-guide/identifiers#uids"))
          (:documentation
           "OwnerReference contains enough information to let you identify an owning object. Currently, an owning object must be in the same namespace, so there is no namespace field."))

(defmethod unmarshal
  ((object owner-reference) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "blockOwnerDeletion" source)
    (when present-p
      (setf (slot-value object 'block-owner-deletion)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "controller" source)
    (when present-p
      (setf (slot-value object 'controller)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "uid" source)
    (when present-p
      (setf (slot-value object 'uid) (decode-object "string" value)))))


(defclass azure-disk-volume-source
          (resource)
          ((kind :initform "AzureDiskVolumeSource" :allocation :class)
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (disk-uri
            :initarg
            :disk-uri
            :type
            string
            :documentation
            "The URI the data disk in the blob storage")
           (caching-mode
            :initarg
            :caching-mode
            :type
            (or azure-data-disk-caching-mode null)
            :documentation
            "Host Caching mode: None, Read Only, Read Write.")
           (disk-name
            :initarg
            :disk-name
            :type
            string
            :documentation
            "The Name of the data disk in the blob storage")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified."))
          (:documentation
           "AzureDisk represents an Azure Data Disk mount on the host and bind mount to the pod."))

(defmethod unmarshal
  ((object azure-disk-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind)
            (decode-object "AzureDataDiskKind" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "diskURI" source)
    (when present-p
      (setf (slot-value object 'disk-uri)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "cachingMode" source)
    (when present-p
      (setf (slot-value object 'caching-mode)
            (decode-object "AzureDataDiskCachingMode" value))))
  (multiple-value-bind (value present-p)
      (gethash "diskName" source)
    (when present-p
      (setf (slot-value object 'disk-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass http-get-action
          (resource)
          ((host
            :initarg
            :host
            :type
            (or string null)
            :documentation
            "Host name to connect to, defaults to the pod IP. You probably want to set \"Host\" in httpHeaders instead.")
           (path
            :initarg
            :path
            :type
            (or string null)
            :documentation
            "Path to access on the HTTP server.")
           (scheme
            :initarg
            :scheme
            :type
            (or string null)
            :documentation
            "Scheme to use for connecting to the host. Defaults to HTTP.")
           (port
            :initarg
            :port
            :type
            string
            :documentation
            "Name or number of the port to access on the container. Number must be in the range 1 to 65535. Name must be an IANA_SVC_NAME.")
           (http-headers
            :initarg
            :http-headers
            :type
            list
            :documentation
            "Custom headers to set in the request. HTTP allows repeated headers."))
          (:documentation
           "HTTPGetAction describes an action based on HTTP Get requests."))

(defmethod unmarshal
  ((object http-get-action) source)
  (multiple-value-bind (value present-p)
      (gethash "host" source)
    (when present-p
      (setf (slot-value object 'host) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "scheme" source)
    (when present-p
      (setf (slot-value object 'scheme)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "port" source)
    (when present-p
      (setf (slot-value object 'port) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "httpHeaders" source)
    (when present-p
      (setf (slot-value object 'http-headers)
            (decode-object (cons "array" "HTTPHeader") value)))))


(defclass resource-requirements
          (resource)
          ((requests
            :initarg
            :requests
            :type
            (or hash-table null)
            :documentation
            "Requests describes the minimum amount of compute resources required. If Requests is omitted for a container, it defaults to Limits if that is explicitly specified, otherwise to an implementation-defined value. More info: https://kubernetes.io/docs/concepts/configuration/manage-compute-resources-container/")
           (limits
            :initarg
            :limits
            :type
            (or hash-table null)
            :documentation
            "Limits describes the maximum amount of compute resources allowed. More info: https://kubernetes.io/docs/concepts/configuration/manage-compute-resources-container/"))
          (:documentation
           "ResourceRequirements describes the compute resource requirements."))

(defmethod unmarshal
  ((object resource-requirements) source)
  (multiple-value-bind (value present-p)
      (gethash "requests" source)
    (when present-p
      (setf (slot-value object 'requests)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "limits" source)
    (when present-p
      (setf (slot-value object 'limits)
            (decode-object "object" value)))))


(defclass ceph-fs-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or local-object-reference null)
            :documentation
            "Optional: SecretRef is reference to the authentication secret for User, default is empty. More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (monitors
            :initarg
            :monitors
            :type
            list
            :documentation
            "Required: Monitors is a collection of Ceph monitors More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (path
            :initarg
            :path
            :type
            (or string null)
            :documentation
            "Optional: Used as the mounted root, rather than the full Ceph tree, default is /")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts. More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (user
            :initarg
            :user
            :type
            (or string null)
            :documentation
            "Optional: User is the rados user name, default is admin More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it")
           (secret-file
            :initarg
            :secret-file
            :type
            (or string null)
            :documentation
            "Optional: SecretFile is the path to key ring for User, default is /etc/ceph/user.secret More info: https://releases.k8s.io/HEAD/examples/volumes/cephfs/README.md#how-to-use-it"))
          (:documentation
           "Represents a Ceph Filesystem mount that lasts the lifetime of a pod Cephfs volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object ceph-fs-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "monitors" source)
    (when present-p
      (setf (slot-value object 'monitors)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "user" source)
    (when present-p
      (setf (slot-value object 'user) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "secretFile" source)
    (when present-p
      (setf (slot-value object 'secret-file)
            (decode-object "string" value)))))


(defclass delete-options
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "DeleteOptions" :allocation :class)
           (preconditions
            :initarg
            :preconditions
            :type
            (or preconditions null)
            :documentation
            "Must be fulfilled before a deletion is carried out. If not possible, a 409 Conflict status will be returned.")
           (grace-period-seconds
            :initarg
            :grace-period-seconds
            :type
            (or integer null)
            :documentation
            "The duration in seconds before the object should be deleted. Value must be non-negative integer. The value zero indicates delete immediately. If this value is nil, the default grace period for the specified type will be used. Defaults to a per object value if not specified. zero means delete immediately.")
           (propagation-policy
            :initarg
            :propagation-policy
            :type
            (or deletion-propagation null)
            :documentation
            "Whether and how garbage collection will be performed. Either this field or OrphanDependents may be set, but not both. The default policy is decided by the existing finalizer set in the metadata.finalizers and the resource-specific default policy. Acceptable values are: 'Orphan' - orphan the dependents; 'Background' - allow the garbage collector to delete the dependents in the background; 'Foreground' - a cascading policy that deletes all dependents in the foreground.")
           (orphan-dependents
            :initarg
            :orphan-dependents
            :type
            (or boolean null)
            :documentation
            "Deprecated: please use the PropagationPolicy, this field will be deprecated in 1.7. Should the dependent objects be orphaned. If true/false, the \"orphan\" finalizer will be added to/removed from the object's finalizers list. Either this field or PropagationPolicy may be set, but not both."))
          (:documentation
           "DeleteOptions may be provided when deleting an API object."))

(defmethod unmarshal
  ((object delete-options) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "preconditions" source)
    (when present-p
      (setf (slot-value object 'preconditions)
            (decode-object "Preconditions" value))))
  (multiple-value-bind (value present-p)
      (gethash "gracePeriodSeconds" source)
    (when present-p
      (setf (slot-value object 'grace-period-seconds)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "propagationPolicy" source)
    (when present-p
      (setf (slot-value object 'propagation-policy)
            (decode-object "DeletionPropagation" value))))
  (multiple-value-bind (value present-p)
      (gethash "orphanDependents" source)
    (when present-p
      (setf (slot-value object 'orphan-dependents)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value)))))


(defclass api-resource-list
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "APIResourceList" :allocation :class)
           (resources
            :initarg
            :resources
            :type
            list
            :documentation
            "resources contains the name of the resources and if they are namespaced.")
           (group-version
            :initarg
            :group-version
            :type
            string
            :documentation
            "groupVersion is the group and version this APIResourceList is for."))
          (:documentation
           "APIResourceList is a list of APIResource, it is used to expose the name of the resources supported in a specific group and version, and if the resource is namespaced."))

(defmethod unmarshal
  ((object api-resource-list) source)
  (multiple-value-bind (value present-p)
      (gethash "resources" source)
    (when present-p
      (setf (slot-value object 'resources)
            (decode-object (cons "array" "APIResource") value))))
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "groupVersion" source)
    (when present-p
      (setf (slot-value object 'group-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value)))))


(defclass pod-dns-config-option
          (resource)
          ((name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Required.")
           (value
            :initarg
            :value
            :type
            (or string null)
            :documentation
            nil))
          (:documentation
           "PodDNSConfigOption defines DNS resolver options of a pod."))

(defmethod unmarshal
  ((object pod-dns-config-option) source)
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "value" source)
    (when present-p
      (setf (slot-value object 'value)
            (decode-object "string" value)))))


(defclass object-meta
          (resource)
          ((deletion-timestamp
            :initarg
            :deletion-timestamp
            :type
            (or string null)
            :documentation
            "DeletionTimestamp is RFC 3339 date and time at which this resource will be deleted. This field is set by the server when a graceful deletion is requested by the user, and is not directly settable by a client. The resource is expected to be deleted (no longer visible from resource lists, and not reachable by name) after the time in this field, once the finalizers list is empty. As long as the finalizers list contains items, deletion is blocked. Once the deletionTimestamp is set, this value may not be unset or be set further into the future, although it may be shortened or the resource may be deleted prior to this time. For example, a user may request that a pod is deleted in 30 seconds. The Kubelet will react by sending a graceful termination signal to the containers in the pod. After that 30 seconds, the Kubelet will send a hard termination signal (SIGKILL) to the container and after cleanup, remove the pod from the API. In the presence of network partitions, this object may still exist after this timestamp, until an administrator or automated process can determine the resource is fully terminated. If not set, graceful deletion of the object has not been requested.

Populated by the system when a graceful deletion is requested. Read-only. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata")
           (self-link
            :initarg
            :self-link
            :type
            (or string null)
            :documentation
            "SelfLink is a URL representing this object. Populated by the system. Read-only.")
           (creation-timestamp
            :initarg
            :creation-timestamp
            :type
            (or string null)
            :documentation
            "CreationTimestamp is a timestamp representing the server time when this object was created. It is not guaranteed to be set in happens-before order across separate operations. Clients may not set this value. It is represented in RFC3339 form and is in UTC.

Populated by the system. Read-only. Null for lists. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata")
           (annotations
            :initarg
            :annotations
            :type
            (or hash-table null)
            :documentation
            "Annotations is an unstructured key value map stored with a resource that may be set by external tools to store and retrieve arbitrary metadata. They are not queryable and should be preserved when modifying objects. More info: http://kubernetes.io/docs/user-guide/annotations")
           (initializers
            :initarg
            :initializers
            :type
            (or initializers null)
            :documentation
            "An initializer is a controller which enforces some system invariant at object creation time. This field is a list of initializers that have not yet acted on this object. If nil or empty, this object has been completely initialized. Otherwise, the object is considered uninitialized and is hidden (in list/watch and get calls) from clients that haven't explicitly asked to observe uninitialized objects.

When an object is created, the system will populate this list with the current set of initializers. Only privileged users may set or modify this list. Once it is empty, it may not be modified further by any user.")
           (labels :initarg
             :labels
             :type
             (or hash-table null)
             :documentation
             "Map of string keys and values that can be used to organize and categorize (scope and select) objects. May match selectors of replication controllers and services. More info: http://kubernetes.io/docs/user-guide/labels")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name must be unique within a namespace. Is required when creating resources, although some resources may allow a client to request the generation of an appropriate name automatically. Name is primarily intended for creation idempotence and configuration definition. Cannot be updated. More info: http://kubernetes.io/docs/user-guide/identifiers#names")
           (cluster-name
            :initarg
            :cluster-name
            :type
            (or string null)
            :documentation
            "The name of the cluster which the object belongs to. This is used to distinguish resources with same name and namespace in different clusters. This field is not set anywhere right now and apiserver is going to ignore it if set in create or update request.")
           (owner-references
            :initarg
            :owner-references
            :type
            list
            :documentation
            "List of objects depended by this object. If ALL objects in the list have been deleted, this object will be garbage collected. If this object is managed by a controller, then an entry in this list will point to this controller, with the controller field set to true. There cannot be more than one managing controller.")
           (deletion-grace-period-seconds
            :initarg
            :deletion-grace-period-seconds
            :type
            (or integer null)
            :documentation
            "Number of seconds allowed for this object to gracefully terminate before it will be removed from the system. Only set when deletionTimestamp is also set. May only be shortened. Read-only.")
           (namespace
            :initarg
            :namespace
            :type
            (or string null)
            :documentation
            "Namespace defines the space within each name must be unique. An empty namespace is equivalent to the \"default\" namespace, but \"default\" is the canonical representation. Not all objects are required to be scoped to a namespace - the value of this field for those objects will be empty.

Must be a DNS_LABEL. Cannot be updated. More info: http://kubernetes.io/docs/user-guide/namespaces")
           (finalizers
            :initarg
            :finalizers
            :type
            list
            :documentation
            "Must be empty before the object is deleted from the registry. Each entry is an identifier for the responsible component that will remove the entry from the list. If the deletionTimestamp of the object is non-nil, entries in this list can only be removed.")
           (resource-version
            :initarg
            :resource-version
            :type
            (or string null)
            :documentation
            "An opaque value that represents the internal version of this object that can be used by clients to determine when objects have changed. May be used for optimistic concurrency, change detection, and the watch operation on a resource or set of resources. Clients must treat these values as opaque and passed unmodified back to the server. They may only be valid for a particular resource or set of resources.

Populated by the system. Read-only. Value must be treated as opaque by clients and . More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#concurrency-control-and-consistency")
           (generate-name
            :initarg
            :generate-name
            :type
            (or string null)
            :documentation
            "GenerateName is an optional prefix, used by the server, to generate a unique name ONLY IF the Name field has not been provided. If this field is used, the name returned to the client will be different than the name passed. This value will also be combined with a unique suffix. The provided value has the same validation rules as the Name field, and may be truncated by the length of the suffix required to make the value unique on the server.

If this field is specified and the generated name exists, the server will NOT return a 409 - instead, it will either return 201 Created or 500 with Reason ServerTimeout indicating a unique name could not be found in the time allotted, and the client should retry (optionally after the time indicated in the Retry-After header).

Applied only if Name is not specified. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#idempotency")
           (generation
            :initarg
            :generation
            :type
            (or integer null)
            :documentation
            "A sequence number representing a specific generation of the desired state. Populated by the system. Read-only.")
           (uid
            :initarg
            :uid
            :type
            (or string null)
            :documentation
            "UID is the unique in time and space value for this object. It is typically generated by the server on successful creation of a resource and is not allowed to change on PUT operations.

Populated by the system. Read-only. More info: http://kubernetes.io/docs/user-guide/identifiers#uids"))
          (:documentation
           "ObjectMeta is metadata that all persisted resources must have, which includes all objects users must create."))

(defmethod unmarshal
  ((object object-meta) source)
  (multiple-value-bind (value present-p)
      (gethash "deletionTimestamp" source)
    (when present-p
      (setf (slot-value object 'deletion-timestamp)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "selfLink" source)
    (when present-p
      (setf (slot-value object 'self-link)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "creationTimestamp" source)
    (when present-p
      (setf (slot-value object 'creation-timestamp)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "annotations" source)
    (when present-p
      (setf (slot-value object 'annotations)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "initializers" source)
    (when present-p
      (setf (slot-value object 'initializers)
            (decode-object "Initializers" value))))
  (multiple-value-bind (value present-p)
      (gethash "labels" source)
    (when present-p
      (setf (slot-value object 'labels)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "clusterName" source)
    (when present-p
      (setf (slot-value object 'cluster-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "ownerReferences" source)
    (when present-p
      (setf (slot-value object 'owner-references)
            (decode-object (cons "array" "OwnerReference") value))))
  (multiple-value-bind (value present-p)
      (gethash "deletionGracePeriodSeconds" source)
    (when present-p
      (setf (slot-value object 'deletion-grace-period-seconds)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "namespace" source)
    (when present-p
      (setf (slot-value object 'namespace)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "finalizers" source)
    (when present-p
      (setf (slot-value object 'finalizers)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "resourceVersion" source)
    (when present-p
      (setf (slot-value object 'resource-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "generateName" source)
    (when present-p
      (setf (slot-value object 'generate-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "generation" source)
    (when present-p
      (setf (slot-value object 'generation)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "uid" source)
    (when present-p
      (setf (slot-value object 'uid) (decode-object "string" value)))))


(defclass controller-revision
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "ControllerRevision" :allocation :class)
           (data
            :initarg
            :data
            :type
            (or string null)
            :documentation
            "Data is the serialized representation of the state.")
           (revision
            :initarg
            :revision
            :type
            integer
            :documentation
            "Revision indicates the revision of the state represented by Data.")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "ControllerRevision implements an immutable snapshot of state data. Clients are responsible for serializing and deserializing the objects that contain their internal state. Once a ControllerRevision has been successfully created, it can not be updated. The API Server will fail validation of all requests that attempt to mutate the Data field. ControllerRevisions may, however, be deleted. Note that, due to its use by both the DaemonSet and StatefulSet controllers for update and rollback, this object is beta. However, it may be subject to name and representation changes in future releases, and clients should not depend on its stability. It is primarily for internal use by controllers."))

(defmethod unmarshal
  ((object controller-revision) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "data" source)
    (when present-p
      (setf (slot-value object 'data) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "revision" source)
    (when present-p
      (setf (slot-value object 'revision)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass replica-set-spec
          (resource)
          ((selector
            :initarg
            :selector
            :type
            label-selector
            :documentation
            "Selector is a label query over pods that should match the replica count. Label keys and values that must match in order to be controlled by this replica set. It must match the pod template's labels. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/labels/#label-selectors")
           (replicas
            :initarg
            :replicas
            :type
            (or integer null)
            :documentation
            "Replicas is the number of desired replicas. This is a pointer to distinguish between explicit zero and unspecified. Defaults to 1. More info: https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller/#what-is-a-replicationcontroller")
           (template
            :initarg
            :template
            :type
            (or pod-template-spec null)
            :documentation
            "Template is the object that describes the pod that will be created if insufficient replicas are detected. More info: https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller#pod-template")
           (min-ready-seconds
            :initarg
            :min-ready-seconds
            :type
            (or integer null)
            :documentation
            "Minimum number of seconds for which a newly created pod should be ready without any of its container crashing, for it to be considered available. Defaults to 0 (pod will be considered available as soon as it is ready)"))
          (:documentation
           "ReplicaSetSpec is the specification of a ReplicaSet."))

(defmethod unmarshal
  ((object replica-set-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "selector" source)
    (when present-p
      (setf (slot-value object 'selector)
            (decode-object "LabelSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "template" source)
    (when present-p
      (setf (slot-value object 'template)
            (decode-object "PodTemplateSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "minReadySeconds" source)
    (when present-p
      (setf (slot-value object 'min-ready-seconds)
            (decode-object (cons "integer" "int32") value)))))


(defclass persistent-volume-access-mode
          (resource)
          nil
          (:documentation nil))

(defmethod unmarshal ((object persistent-volume-access-mode) source))


(defclass downward-api-projection
          (resource)
          ((items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is a list of DownwardAPIVolume file"))
          (:documentation
           "Represents downward API info for projecting into a projected volume. Note that this is identical to a downwardAPI volume source without the default mode."))

(defmethod unmarshal
  ((object downward-api-projection) source)
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object
             (cons "array" "DownwardAPIVolumeFile")
             value)))))


(defclass config-map-volume-source
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the ConfigMap or it's keys must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names")
           (default-mode
            :initarg
            :default-mode
            :type
            (or integer null)
            :documentation
            "Optional: mode bits to use on created files by default. Must be a value between 0 and 0777. Defaults to 0644. Directories within the path are not affected by this setting. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set.")
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "If unspecified, each key-value pair in the Data field of the referenced ConfigMap will be projected into the volume as a file whose name is the key and content is the value. If specified, the listed keys will be projected into the specified paths, and unlisted keys will not be present. If a key is specified which is not present in the ConfigMap, the volume setup will error unless it is marked optional. Paths must be relative and may not contain the '..' path or start with '..'."))
          (:documentation
           "Adapts a ConfigMap into a volume.

The contents of the target ConfigMap's Data field will be presented in a volume as files using the keys in the Data field as the file names, unless the items element is populated with specific mappings of keys to paths. ConfigMap volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object config-map-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "defaultMode" source)
    (when present-p
      (setf (slot-value object 'default-mode)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "KeyToPath") value)))))


(defclass uid (resource) nil (:documentation nil))

(defmethod unmarshal ((object uid) source))


(defclass deployment-list
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "DeploymentList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is the list of Deployments.")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata."))
          (:documentation "DeploymentList is a list of Deployments."))

(defmethod unmarshal
  ((object deployment-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "Deployment") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass key-to-path
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "The relative path of the file to map the key to. May not be an absolute path. May not contain the path element '..'. May not start with the string '..'.")
           (key
            :initarg
            :key
            :type
            string
            :documentation
            "The key to project.")
           (mode
            :initarg
            :mode
            :type
            (or integer null)
            :documentation
            "Optional: mode bits to use on this file, must be a value between 0 and 0777. If not specified, the volume defaultMode will be used. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set."))
          (:documentation
           "Maps a string key to a path within a volume."))

(defmethod unmarshal
  ((object key-to-path) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "mode" source)
    (when present-p
      (setf (slot-value object 'mode)
            (decode-object (cons "integer" "int32") value)))))


(defclass host-path-type (resource) nil (:documentation nil))

(defmethod unmarshal ((object host-path-type) source))


(defclass config-map-env-source
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the ConfigMap must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names"))
          (:documentation
           "ConfigMapEnvSource selects a ConfigMap to populate the environment variables with.

The contents of the target ConfigMap's Data field will represent the key-value pairs as environment variables."))

(defmethod unmarshal
  ((object config-map-env-source) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass handler
          (resource)
          ((exec
            :initarg
            :exec
            :type
            (or exec-action null)
            :documentation
            "One and only one of the following should be specified. Exec specifies the action to take.")
           (http-get
            :initarg
            :http-get
            :type
            (or http-get-action null)
            :documentation
            "HTTPGet specifies the http request to perform.")
           (tcp-socket
            :initarg
            :tcp-socket
            :type
            (or tcp-socket-action null)
            :documentation
            "TCPSocket specifies an action involving a TCP port. TCP hooks not yet supported"))
          (:documentation
           "Handler defines a specific action that should be taken"))

(defmethod unmarshal
  ((object handler) source)
  (multiple-value-bind (value present-p)
      (gethash "exec" source)
    (when present-p
      (setf (slot-value object 'exec)
            (decode-object "ExecAction" value))))
  (multiple-value-bind (value present-p)
      (gethash "httpGet" source)
    (when present-p
      (setf (slot-value object 'http-get)
            (decode-object "HTTPGetAction" value))))
  (multiple-value-bind (value present-p)
      (gethash "tcpSocket" source)
    (when present-p
      (setf (slot-value object 'tcp-socket)
            (decode-object "TCPSocketAction" value)))))


(defclass replica-set-status
          (resource)
          ((available-replicas
            :initarg
            :available-replicas
            :type
            (or integer null)
            :documentation
            "The number of available replicas (ready for at least minReadySeconds) for this replica set.")
           (conditions
            :initarg
            :conditions
            :type
            list
            :documentation
            "Represents the latest available observations of a replica set's current state.")
           (ready-replicas
            :initarg
            :ready-replicas
            :type
            (or integer null)
            :documentation
            "The number of ready replicas for this replica set.")
           (observed-generation
            :initarg
            :observed-generation
            :type
            (or integer null)
            :documentation
            "ObservedGeneration reflects the generation of the most recently observed ReplicaSet.")
           (replicas
            :initarg
            :replicas
            :type
            integer
            :documentation
            "Replicas is the most recently oberved number of replicas. More info: https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller/#what-is-a-replicationcontroller")
           (fully-labeled-replicas
            :initarg
            :fully-labeled-replicas
            :type
            (or integer null)
            :documentation
            "The number of pods that have labels matching the labels of the pod template of the replicaset."))
          (:documentation
           "ReplicaSetStatus represents the current status of a ReplicaSet."))

(defmethod unmarshal
  ((object replica-set-status) source)
  (multiple-value-bind (value present-p)
      (gethash "availableReplicas" source)
    (when present-p
      (setf (slot-value object 'available-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "conditions" source)
    (when present-p
      (setf (slot-value object 'conditions)
            (decode-object
             (cons "array" "ReplicaSetCondition")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "readyReplicas" source)
    (when present-p
      (setf (slot-value object 'ready-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "observedGeneration" source)
    (when present-p
      (setf (slot-value object 'observed-generation)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "fullyLabeledReplicas" source)
    (when present-p
      (setf (slot-value object 'fully-labeled-replicas)
            (decode-object (cons "integer" "int32") value)))))


(defclass preconditions
          (resource)
          ((uid
            :initarg
            :uid
            :type
            (or uid null)
            :documentation
            "Specifies the target UID."))
          (:documentation
           "Preconditions must be fulfilled before an operation (update, delete, etc.) is carried out."))

(defmethod unmarshal
  ((object preconditions) source)
  (multiple-value-bind (value present-p)
      (gethash "uid" source)
    (when present-p
      (setf (slot-value object 'uid) (decode-object "UID" value)))))


(defclass flex-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or local-object-reference null)
            :documentation
            "Optional: SecretRef is reference to the secret object containing sensitive information to pass to the plugin scripts. This may be empty if no secret object is specified. If the secret object contains more than one secret, all secrets are passed to the plugin scripts.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (options
            :initarg
            :options
            :type
            (or hash-table null)
            :documentation
            "Optional: Extra command options if any.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". The default filesystem depends on FlexVolume script.")
           (driver
            :initarg
            :driver
            :type
            string
            :documentation
            "Driver is the name of the driver to use for this volume."))
          (:documentation
           "FlexVolume represents a generic volume resource that is provisioned/attached using an exec based plugin."))

(defmethod unmarshal
  ((object flex-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "options" source)
    (when present-p
      (setf (slot-value object 'options)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "driver" source)
    (when present-p
      (setf (slot-value object 'driver)
            (decode-object "string" value)))))


(defclass probe
          (resource)
          ((exec
            :initarg
            :exec
            :type
            (or exec-action null)
            :documentation
            "One and only one of the following should be specified. Exec specifies the action to take.")
           (http-get
            :initarg
            :http-get
            :type
            (or http-get-action null)
            :documentation
            "HTTPGet specifies the http request to perform.")
           (timeout-seconds
            :initarg
            :timeout-seconds
            :type
            (or integer null)
            :documentation
            "Number of seconds after which the probe times out. Defaults to 1 second. Minimum value is 1. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes")
           (failure-threshold
            :initarg
            :failure-threshold
            :type
            (or integer null)
            :documentation
            "Minimum consecutive failures for the probe to be considered failed after having succeeded. Defaults to 3. Minimum value is 1.")
           (success-threshold
            :initarg
            :success-threshold
            :type
            (or integer null)
            :documentation
            "Minimum consecutive successes for the probe to be considered successful after having failed. Defaults to 1. Must be 1 for liveness. Minimum value is 1.")
           (tcp-socket
            :initarg
            :tcp-socket
            :type
            (or tcp-socket-action null)
            :documentation
            "TCPSocket specifies an action involving a TCP port. TCP hooks not yet supported")
           (initial-delay-seconds
            :initarg
            :initial-delay-seconds
            :type
            (or integer null)
            :documentation
            "Number of seconds after the container has started before liveness probes are initiated. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes")
           (period-seconds
            :initarg
            :period-seconds
            :type
            (or integer null)
            :documentation
            "How often (in seconds) to perform the probe. Default to 10 seconds. Minimum value is 1."))
          (:documentation
           "Probe describes a health check to be performed against a container to determine whether it is alive or ready to receive traffic."))

(defmethod unmarshal
  ((object probe) source)
  (multiple-value-bind (value present-p)
      (gethash "exec" source)
    (when present-p
      (setf (slot-value object 'exec)
            (decode-object "ExecAction" value))))
  (multiple-value-bind (value present-p)
      (gethash "httpGet" source)
    (when present-p
      (setf (slot-value object 'http-get)
            (decode-object "HTTPGetAction" value))))
  (multiple-value-bind (value present-p)
      (gethash "timeoutSeconds" source)
    (when present-p
      (setf (slot-value object 'timeout-seconds)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "failureThreshold" source)
    (when present-p
      (setf (slot-value object 'failure-threshold)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "successThreshold" source)
    (when present-p
      (setf (slot-value object 'success-threshold)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "tcpSocket" source)
    (when present-p
      (setf (slot-value object 'tcp-socket)
            (decode-object "TCPSocketAction" value))))
  (multiple-value-bind (value present-p)
      (gethash "initialDelaySeconds" source)
    (when present-p
      (setf (slot-value object 'initial-delay-seconds)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "periodSeconds" source)
    (when present-p
      (setf (slot-value object 'period-seconds)
            (decode-object (cons "integer" "int32") value)))))


(defclass gce-persistent-disk-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the ReadOnly setting in VolumeMounts. Defaults to false. More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk")
           (pd-name
            :initarg
            :pd-name
            :type
            string
            :documentation
            "Unique name of the PD resource in GCE. Used to identify the disk in GCE. More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk")
           (partition
            :initarg
            :partition
            :type
            (or integer null)
            :documentation
            "The partition in the volume that you want to mount. If omitted, the default is to mount by volume name. Examples: For volume /dev/sda1, you specify the partition as \"1\". Similarly, the volume partition for /dev/sda is \"0\" (or you can leave the property empty). More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type of the volume that you want to mount. Tip: Ensure that the filesystem type is supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk"))
          (:documentation
           "Represents a Persistent Disk resource in Google Compute Engine.

A GCE PD must exist before mounting to a container. The disk must also be in the same GCE project and zone as the kubelet. A GCE PD can only be mounted as read/write once or read-only many times. GCE PDs support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object gce-persistent-disk-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "pdName" source)
    (when present-p
      (setf (slot-value object 'pd-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "partition" source)
    (when present-p
      (setf (slot-value object 'partition)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass replica-set-list
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "ReplicaSetList" :allocation :class)
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "List of ReplicaSets. More info: https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "ReplicaSetList is a collection of ReplicaSets."))

(defmethod unmarshal
  ((object replica-set-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "ReplicaSet") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass watch-event
          (resource)
          ((type :initarg :type :type string :documentation nil)
           (object :initarg :object :type string :documentation nil))
          (:documentation nil))

(defmethod unmarshal
  ((object watch-event) source)
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "object" source)
    (when present-p
      (setf (slot-value object 'object)
            (decode-object "string" value)))))


(defclass iscsi-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or local-object-reference null)
            :documentation
            "CHAP Secret for iSCSI target and initiator authentication")
           (portals
            :initarg
            :portals
            :type
            list
            :documentation
            "iSCSI Target Portal List. The portal is either an IP or ip_addr:port if the port is other than default (typically TCP ports 860 and 3260).")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the ReadOnly setting in VolumeMounts. Defaults to false.")
           (lun
            :initarg
            :lun
            :type
            integer
            :documentation
            "iSCSI Target Lun number.")
           (target-portal
            :initarg
            :target-portal
            :type
            string
            :documentation
            "iSCSI Target Portal. The Portal is either an IP or ip_addr:port if the port is other than default (typically TCP ports 860 and 3260).")
           (iscsi-interface
            :initarg
            :iscsi-interface
            :type
            (or string null)
            :documentation
            "iSCSI Interface Name that uses an iSCSI transport. Defaults to 'default' (tcp).")
           (iqn
            :initarg
            :iqn
            :type
            string
            :documentation
            "Target iSCSI Qualified Name.")
           (chap-auth-discovery
            :initarg
            :chap-auth-discovery
            :type
            (or boolean null)
            :documentation
            "whether support iSCSI Discovery CHAP authentication")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type of the volume that you want to mount. Tip: Ensure that the filesystem type is supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://kubernetes.io/docs/concepts/storage/volumes#iscsi")
           (initiator-name
            :initarg
            :initiator-name
            :type
            (or string null)
            :documentation
            "Custom iSCSI Initiator Name. If initiatorName is specified with iscsiInterface simultaneously, new iSCSI interface <target portal>:<volume name> will be created for the connection.")
           (chap-auth-session
            :initarg
            :chap-auth-session
            :type
            (or boolean null)
            :documentation
            "whether support iSCSI Session CHAP authentication"))
          (:documentation
           "Represents an ISCSI disk. ISCSI volumes can only be mounted as read/write once. ISCSI volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object iscsi-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "portals" source)
    (when present-p
      (setf (slot-value object 'portals)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "lun" source)
    (when present-p
      (setf (slot-value object 'lun)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "targetPortal" source)
    (when present-p
      (setf (slot-value object 'target-portal)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "iscsiInterface" source)
    (when present-p
      (setf (slot-value object 'iscsi-interface)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "iqn" source)
    (when present-p
      (setf (slot-value object 'iqn) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "chapAuthDiscovery" source)
    (when present-p
      (setf (slot-value object 'chap-auth-discovery)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "initiatorName" source)
    (when present-p
      (setf (slot-value object 'initiator-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "chapAuthSession" source)
    (when present-p
      (setf (slot-value object 'chap-auth-session)
            (decode-object "boolean" value)))))


(defclass downward-api-volume-file
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "Required: Path is  the relative path name of the file to be created. Must not be absolute or contain the '..' path. Must be utf-8 encoded. The first item of the relative path must not start with '..'")
           (mode
            :initarg
            :mode
            :type
            (or integer null)
            :documentation
            "Optional: mode bits to use on this file, must be a value between 0 and 0777. If not specified, the volume defaultMode will be used. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set.")
           (field-ref
            :initarg
            :field-ref
            :type
            (or object-field-selector null)
            :documentation
            "Required: Selects a field of the pod: only annotations, labels, name and namespace are supported.")
           (resource-field-ref
            :initarg
            :resource-field-ref
            :type
            (or resource-field-selector null)
            :documentation
            "Selects a resource of the container: only resources limits and requests (limits.cpu, limits.memory, requests.cpu and requests.memory) are currently supported."))
          (:documentation
           "DownwardAPIVolumeFile represents information to create the file containing the pod field"))

(defmethod unmarshal
  ((object downward-api-volume-file) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "mode" source)
    (when present-p
      (setf (slot-value object 'mode)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "fieldRef" source)
    (when present-p
      (setf (slot-value object 'field-ref)
            (decode-object "ObjectFieldSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "resourceFieldRef" source)
    (when present-p
      (setf (slot-value object 'resource-field-ref)
            (decode-object "ResourceFieldSelector" value)))))


(defclass replica-set-condition
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human readable message indicating details about the transition.")
           (last-transition-time
            :initarg
            :last-transition-time
            :type
            (or string null)
            :documentation
            "The last time the condition transitioned from one status to another.")
           (type
            :initarg
            :type
            :type
            string
            :documentation
            "Type of replica set condition.")
           (status
            :initarg
            :status
            :type
            string
            :documentation
            "Status of the condition, one of True, False, Unknown.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "The reason for the condition's last transition."))
          (:documentation
           "ReplicaSetCondition describes the state of a replica set at a certain point."))

(defmethod unmarshal
  ((object replica-set-condition) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastTransitionTime" source)
    (when present-p
      (setf (slot-value object 'last-transition-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value)))))


(defclass container-port
          (resource)
          ((host-ip
            :initarg
            :host-ip
            :type
            (or string null)
            :documentation
            "What host IP to bind the external port to.")
           (host-port
            :initarg
            :host-port
            :type
            (or integer null)
            :documentation
            "Number of port to expose on the host. If specified, this must be a valid port number, 0 < x < 65536. If HostNetwork is specified, this must match ContainerPort. Most containers do not need this.")
           (container-port
            :initarg
            :container-port
            :type
            integer
            :documentation
            "Number of port to expose on the pod's IP address. This must be a valid port number, 0 < x < 65536.")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "If specified, this must be an IANA_SVC_NAME and unique within the pod. Each named port in a pod must have a unique name. Name for the port that can be referred to by services.")
           (protocol
            :initarg
            :protocol
            :type
            (or string null)
            :documentation
            "Protocol for port. Must be UDP or TCP. Defaults to \"TCP\"."))
          (:documentation
           "ContainerPort represents a network port in a single container."))

(defmethod unmarshal
  ((object container-port) source)
  (multiple-value-bind (value present-p)
      (gethash "hostIP" source)
    (when present-p
      (setf (slot-value object 'host-ip)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "hostPort" source)
    (when present-p
      (setf (slot-value object 'host-port)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "containerPort" source)
    (when present-p
      (setf (slot-value object 'container-port)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "protocol" source)
    (when present-p
      (setf (slot-value object 'protocol)
            (decode-object "string" value)))))


(defclass toleration
          (resource)
          ((effect
            :initarg
            :effect
            :type
            (or string null)
            :documentation
            "Effect indicates the taint effect to match. Empty means match all taint effects. When specified, allowed values are NoSchedule, PreferNoSchedule and NoExecute.")
           (key
            :initarg
            :key
            :type
            (or string null)
            :documentation
            "Key is the taint key that the toleration applies to. Empty means match all taint keys. If the key is empty, operator must be Exists; this combination means to match all values and all keys.")
           (toleration-seconds
            :initarg
            :toleration-seconds
            :type
            (or integer null)
            :documentation
            "TolerationSeconds represents the period of time the toleration (which must be of effect NoExecute, otherwise this field is ignored) tolerates the taint. By default, it is not set, which means tolerate the taint forever (do not evict). Zero and negative values will be treated as 0 (evict immediately) by the system.")
           (operator
            :initarg
            :operator
            :type
            (or string null)
            :documentation
            "Operator represents a key's relationship to the value. Valid operators are Exists and Equal. Defaults to Equal. Exists is equivalent to wildcard for value, so that a pod can tolerate all taints of a particular category.")
           (value
            :initarg
            :value
            :type
            (or string null)
            :documentation
            "Value is the taint value the toleration matches to. If the operator is Exists, the value should be empty, otherwise just a regular string."))
          (:documentation
           "The pod this Toleration is attached to tolerates any taint that matches the triple <key,value,effect> using the matching operator <operator>."))

(defmethod unmarshal
  ((object toleration) source)
  (multiple-value-bind (value present-p)
      (gethash "effect" source)
    (when present-p
      (setf (slot-value object 'effect)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "tolerationSeconds" source)
    (when present-p
      (setf (slot-value object 'toleration-seconds)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "operator" source)
    (when present-p
      (setf (slot-value object 'operator)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "value" source)
    (when present-p
      (setf (slot-value object 'value)
            (decode-object "string" value)))))


(defclass affinity
          (resource)
          ((pod-anti-affinity
            :initarg
            :pod-anti-affinity
            :type
            (or pod-anti-affinity null)
            :documentation
            "Describes pod anti-affinity scheduling rules (e.g. avoid putting this pod in the same node, zone, etc. as some other pod(s)).")
           (node-affinity
            :initarg
            :node-affinity
            :type
            (or node-affinity null)
            :documentation
            "Describes node affinity scheduling rules for the pod.")
           (pod-affinity
            :initarg
            :pod-affinity
            :type
            (or pod-affinity null)
            :documentation
            "Describes pod affinity scheduling rules (e.g. co-locate this pod in the same node, zone, etc. as some other pod(s))."))
          (:documentation
           "Affinity is a group of affinity scheduling rules."))

(defmethod unmarshal
  ((object affinity) source)
  (multiple-value-bind (value present-p)
      (gethash "podAntiAffinity" source)
    (when present-p
      (setf (slot-value object 'pod-anti-affinity)
            (decode-object "PodAntiAffinity" value))))
  (multiple-value-bind (value present-p)
      (gethash "nodeAffinity" source)
    (when present-p
      (setf (slot-value object 'node-affinity)
            (decode-object "NodeAffinity" value))))
  (multiple-value-bind (value present-p)
      (gethash "podAffinity" source)
    (when present-p
      (setf (slot-value object 'pod-affinity)
            (decode-object "PodAffinity" value)))))


(defclass daemon-set-update-strategy
          (resource)
          ((rolling-update
            :initarg
            :rolling-update
            :type
            (or rolling-update-daemon-set null)
            :documentation
            "Rolling update config params. Present only if type = \"RollingUpdate\".")
           (type
            :initarg
            :type
            :type
            (or string null)
            :documentation
            "Type of daemon set update. Can be \"RollingUpdate\" or \"OnDelete\". Default is RollingUpdate."))
          (:documentation
           "DaemonSetUpdateStrategy is a struct used to control the update strategy for a DaemonSet."))

(defmethod unmarshal
  ((object daemon-set-update-strategy) source)
  (multiple-value-bind (value present-p)
      (gethash "rollingUpdate" source)
    (when present-p
      (setf (slot-value object 'rolling-update)
            (decode-object "RollingUpdateDaemonSet" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value)))))


(defclass quobyte-volume-source
          (resource)
          ((volume
            :initarg
            :volume
            :type
            string
            :documentation
            "Volume is a string that references an already created Quobyte volume by name.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the Quobyte volume to be mounted with read-only permissions. Defaults to false.")
           (group
            :initarg
            :group
            :type
            (or string null)
            :documentation
            "Group to map volume access to Default is no group")
           (user
            :initarg
            :user
            :type
            (or string null)
            :documentation
            "User to map volume access to Defaults to serivceaccount user")
           (registry
            :initarg
            :registry
            :type
            string
            :documentation
            "Registry represents a single or multiple Quobyte Registry services specified as a string as host:port pair (multiple entries are separated with commas) which acts as the central registry for volumes"))
          (:documentation
           "Represents a Quobyte mount that lasts the lifetime of a pod. Quobyte volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object quobyte-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "volume" source)
    (when present-p
      (setf (slot-value object 'volume)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "group" source)
    (when present-p
      (setf (slot-value object 'group)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "user" source)
    (when present-p
      (setf (slot-value object 'user) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "registry" source)
    (when present-p
      (setf (slot-value object 'registry)
            (decode-object "string" value)))))


(defclass daemon-set
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "DaemonSet" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or daemon-set-status null)
            :documentation
            "The current status of this daemon set. This data may be out of date by some window of time. Populated by the system. Read-only. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (spec
            :initarg
            :spec
            :type
            (or daemon-set-spec null)
            :documentation
            "The desired behavior of this daemon set. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "DaemonSet represents the configuration of a daemon set."))

(defmethod unmarshal
  ((object daemon-set) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "DaemonSetStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "DaemonSetSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass preferred-scheduling-term
          (resource)
          ((preference
            :initarg
            :preference
            :type
            node-selector-term
            :documentation
            "A node selector term, associated with the corresponding weight.")
           (weight
            :initarg
            :weight
            :type
            integer
            :documentation
            "Weight associated with matching the corresponding nodeSelectorTerm, in the range 1-100."))
          (:documentation
           "An empty preferred scheduling term matches all objects with implicit weight 0 (i.e. it's a no-op). A null preferred scheduling term matches no objects (i.e. is also a no-op)."))

(defmethod unmarshal
  ((object preferred-scheduling-term) source)
  (multiple-value-bind (value present-p)
      (gethash "preference" source)
    (when present-p
      (setf (slot-value object 'preference)
            (decode-object "NodeSelectorTerm" value))))
  (multiple-value-bind (value present-p)
      (gethash "weight" source)
    (when present-p
      (setf (slot-value object 'weight)
            (decode-object (cons "integer" "int32") value)))))


(defclass stateful-set-condition
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human readable message indicating details about the transition.")
           (last-transition-time
            :initarg
            :last-transition-time
            :type
            (or string null)
            :documentation
            "Last time the condition transitioned from one status to another.")
           (type
            :initarg
            :type
            :type
            string
            :documentation
            "Type of statefulset condition.")
           (status
            :initarg
            :status
            :type
            string
            :documentation
            "Status of the condition, one of True, False, Unknown.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "The reason for the condition's last transition."))
          (:documentation
           "StatefulSetCondition describes the state of a statefulset at a certain point."))

(defmethod unmarshal
  ((object stateful-set-condition) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastTransitionTime" source)
    (when present-p
      (setf (slot-value object 'last-transition-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value)))))


(defclass host-alias
          (resource)
          ((hostnames
            :initarg
            :hostnames
            :type
            list
            :documentation
            "Hostnames for the above IP address.")
           (ip
            :initarg
            :ip
            :type
            (or string null)
            :documentation
            "IP address of the host file entry."))
          (:documentation
           "HostAlias holds the mapping between IP and hostnames that will be injected as an entry in the pod's hosts file."))

(defmethod unmarshal
  ((object host-alias) source)
  (multiple-value-bind (value present-p)
      (gethash "hostnames" source)
    (when present-p
      (setf (slot-value object 'hostnames)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "ip" source)
    (when present-p
      (setf (slot-value object 'ip) (decode-object "string" value)))))


(defclass volume-projection
          (resource)
          ((config-map
            :initarg
            :config-map
            :type
            (or config-map-projection null)
            :documentation
            "information about the configMap data to project")
           (downward-api
            :initarg
            :downward-api
            :type
            (or downward-api-projection null)
            :documentation
            "information about the downwardAPI data to project")
           (secret
            :initarg
            :secret
            :type
            (or secret-projection null)
            :documentation
            "information about the secret data to project"))
          (:documentation
           "Projection that may be projected along with other supported volume types"))

(defmethod unmarshal
  ((object volume-projection) source)
  (multiple-value-bind (value present-p)
      (gethash "configMap" source)
    (when present-p
      (setf (slot-value object 'config-map)
            (decode-object "ConfigMapProjection" value))))
  (multiple-value-bind (value present-p)
      (gethash "downwardAPI" source)
    (when present-p
      (setf (slot-value object 'downward-api)
            (decode-object "DownwardAPIProjection" value))))
  (multiple-value-bind (value present-p)
      (gethash "secret" source)
    (when present-p
      (setf (slot-value object 'secret)
            (decode-object "SecretProjection" value)))))


(defclass flocker-volume-source
          (resource)
          ((dataset-uuid
            :initarg
            :dataset-uuid
            :type
            (or string null)
            :documentation
            "UUID of the dataset. This is unique identifier of a Flocker dataset")
           (dataset-name
            :initarg
            :dataset-name
            :type
            (or string null)
            :documentation
            "Name of the dataset stored as metadata -> name on the dataset for Flocker should be considered as deprecated"))
          (:documentation
           "Represents a Flocker volume mounted by the Flocker agent. One and only one of datasetName and datasetUUID should be set. Flocker volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object flocker-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "datasetUUID" source)
    (when present-p
      (setf (slot-value object 'dataset-uuid)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "datasetName" source)
    (when present-p
      (setf (slot-value object 'dataset-name)
            (decode-object "string" value)))))


(defclass secret-env-source
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the Secret must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names"))
          (:documentation
           "SecretEnvSource selects a Secret to populate the environment variables with.

The contents of the target Secret's Data field will represent the key-value pairs as environment variables."))

(defmethod unmarshal
  ((object secret-env-source) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass daemon-set-spec
          (resource)
          ((selector
            :initarg
            :selector
            :type
            label-selector
            :documentation
            "A label query over pods that are managed by the daemon set. Must match in order to be controlled. It must match the pod template's labels. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/labels/#label-selectors")
           (revision-history-limit
            :initarg
            :revision-history-limit
            :type
            (or integer null)
            :documentation
            "The number of old history to retain to allow rollback. This is a pointer to distinguish between explicit zero and not specified. Defaults to 10.")
           (template
            :initarg
            :template
            :type
            pod-template-spec
            :documentation
            "An object that describes the pod that will be created. The DaemonSet will create exactly one copy of this pod on every node that matches the template's node selector (or on every node if no node selector is specified). More info: https://kubernetes.io/docs/concepts/workloads/controllers/replicationcontroller#pod-template")
           (min-ready-seconds
            :initarg
            :min-ready-seconds
            :type
            (or integer null)
            :documentation
            "The minimum number of seconds for which a newly created DaemonSet pod should be ready without any of its container crashing, for it to be considered available. Defaults to 0 (pod will be considered available as soon as it is ready).")
           (update-strategy
            :initarg
            :update-strategy
            :type
            (or daemon-set-update-strategy null)
            :documentation
            "An update strategy to replace existing DaemonSet pods with new pods."))
          (:documentation
           "DaemonSetSpec is the specification of a daemon set."))

(defmethod unmarshal
  ((object daemon-set-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "selector" source)
    (when present-p
      (setf (slot-value object 'selector)
            (decode-object "LabelSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "revisionHistoryLimit" source)
    (when present-p
      (setf (slot-value object 'revision-history-limit)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "template" source)
    (when present-p
      (setf (slot-value object 'template)
            (decode-object "PodTemplateSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "minReadySeconds" source)
    (when present-p
      (setf (slot-value object 'min-ready-seconds)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "updateStrategy" source)
    (when present-p
      (setf (slot-value object 'update-strategy)
            (decode-object "DaemonSetUpdateStrategy" value)))))


(defclass replica-set
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "ReplicaSet" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or replica-set-status null)
            :documentation
            "Status is the most recently observed status of the ReplicaSet. This data may be out of date by some window of time. Populated by the system. Read-only. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (spec
            :initarg
            :spec
            :type
            (or replica-set-spec null)
            :documentation
            "Spec defines the specification of the desired behavior of the ReplicaSet. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "If the Labels of a ReplicaSet are empty, they are defaulted to be the same as the Pod(s) that the ReplicaSet manages. Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "ReplicaSet ensures that a specified number of pod replicas are running at any given time."))

(defmethod unmarshal
  ((object replica-set) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "ReplicaSetStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "ReplicaSetSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass secret-key-selector
          (resource)
          ((key
            :initarg
            :key
            :type
            string
            :documentation
            "The key of the secret to select from.  Must be a valid secret key.")
           (optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the Secret or it's key must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names"))
          (:documentation
           "SecretKeySelector selects a key of a Secret."))

(defmethod unmarshal
  ((object secret-key-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass exec-action
          (resource)
          ((command
            :initarg
            :command
            :type
            list
            :documentation
            "Command is the command line to execute inside the container, the working directory for the command  is root ('/') in the container's filesystem. The command is simply exec'd, it is not run inside a shell, so traditional shell instructions ('|', etc) won't work. To use a shell, you need to explicitly call out to that shell. Exit status of 0 is treated as live/healthy and non-zero is unhealthy."))
          (:documentation
           "ExecAction describes a \"run in container\" action."))

(defmethod unmarshal
  ((object exec-action) source)
  (multiple-value-bind (value present-p)
      (gethash "command" source)
    (when present-p
      (setf (slot-value object 'command)
            (decode-object (cons "array" "string") value)))))


(defclass rolling-update-stateful-set-strategy
          (resource)
          ((partition
            :initarg
            :partition
            :type
            (or integer null)
            :documentation
            "Partition indicates the ordinal at which the StatefulSet should be partitioned. Default value is 0."))
          (:documentation
           "RollingUpdateStatefulSetStrategy is used to communicate parameter for RollingUpdateStatefulSetStrategyType."))

(defmethod unmarshal
  ((object rolling-update-stateful-set-strategy) source)
  (multiple-value-bind (value present-p)
      (gethash "partition" source)
    (when present-p
      (setf (slot-value object 'partition)
            (decode-object (cons "integer" "int32") value)))))


(defclass deployment-strategy
          (resource)
          ((rolling-update
            :initarg
            :rolling-update
            :type
            (or rolling-update-deployment null)
            :documentation
            "Rolling update config params. Present only if DeploymentStrategyType = RollingUpdate.")
           (type
            :initarg
            :type
            :type
            (or string null)
            :documentation
            "Type of deployment. Can be \"Recreate\" or \"RollingUpdate\". Default is RollingUpdate."))
          (:documentation
           "DeploymentStrategy describes how to replace existing pods with new ones."))

(defmethod unmarshal
  ((object deployment-strategy) source)
  (multiple-value-bind (value present-p)
      (gethash "rollingUpdate" source)
    (when present-p
      (setf (slot-value object 'rolling-update)
            (decode-object "RollingUpdateDeployment" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value)))))


(defclass env-from-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or secret-env-source null)
            :documentation
            "The Secret to select from")
           (config-map-ref
            :initarg
            :config-map-ref
            :type
            (or config-map-env-source null)
            :documentation
            "The ConfigMap to select from")
           (prefix
            :initarg
            :prefix
            :type
            (or string null)
            :documentation
            "An optional identifier to prepend to each key in the ConfigMap. Must be a C_IDENTIFIER."))
          (:documentation
           "EnvFromSource represents the source of a set of ConfigMaps"))

(defmethod unmarshal
  ((object env-from-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "SecretEnvSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "configMapRef" source)
    (when present-p
      (setf (slot-value object 'config-map-ref)
            (decode-object "ConfigMapEnvSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "prefix" source)
    (when present-p
      (setf (slot-value object 'prefix)
            (decode-object "string" value)))))


(defclass initializers
          (resource)
          ((pending
            :initarg
            :pending
            :type
            list
            :documentation
            "Pending is a list of initializers that must execute in order before this object is visible. When the last pending initializer is removed, and no failing result is set, the initializers struct will be set to nil and the object is considered as initialized and visible to all clients.")
           (result
            :initarg
            :result
            :type
            (or status null)
            :documentation
            "If result is set with the Failure field, the object will be persisted to storage and then deleted, ensuring that other clients can observe the deletion."))
          (:documentation
           "Initializers tracks the progress of initialization."))

(defmethod unmarshal
  ((object initializers) source)
  (multiple-value-bind (value present-p)
      (gethash "pending" source)
    (when present-p
      (setf (slot-value object 'pending)
            (decode-object (cons "array" "Initializer") value))))
  (multiple-value-bind (value present-p)
      (gethash "result" source)
    (when present-p
      (setf (slot-value object 'result)
            (decode-object "Status" value)))))


(defclass empty-dir-volume-source
          (resource)
          ((size-limit
            :initarg
            :size-limit
            :type
            (or string null)
            :documentation
            "Total amount of local storage required for this EmptyDir volume. The size limit is also applicable for memory medium. The maximum usage on memory medium EmptyDir would be the minimum value between the SizeLimit specified here and the sum of memory limits of all containers in a pod. The default is nil which means that the limit is undefined. More info: http://kubernetes.io/docs/user-guide/volumes#emptydir")
           (medium
            :initarg
            :medium
            :type
            (or string null)
            :documentation
            "What type of storage medium should back this directory. The default is \"\" which means to use the node's default medium. Must be an empty string (default) or Memory. More info: https://kubernetes.io/docs/concepts/storage/volumes#emptydir"))
          (:documentation
           "Represents an empty directory for a pod. Empty directory volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object empty-dir-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "sizeLimit" source)
    (when present-p
      (setf (slot-value object 'size-limit)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "medium" source)
    (when present-p
      (setf (slot-value object 'medium)
            (decode-object "string" value)))))


(defclass secret-projection
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the Secret or its key must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names")
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "If unspecified, each key-value pair in the Data field of the referenced Secret will be projected into the volume as a file whose name is the key and content is the value. If specified, the listed keys will be projected into the specified paths, and unlisted keys will not be present. If a key is specified which is not present in the Secret, the volume setup will error unless it is marked optional. Paths must be relative and may not contain the '..' path or start with '..'."))
          (:documentation
           "Adapts a secret into a projected volume.

The contents of the target Secret's Data field will be presented in a projected volume as files using the keys in the Data field as the file names. Note that this is identical to a secret volume source without the default mode."))

(defmethod unmarshal
  ((object secret-projection) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "KeyToPath") value)))))


(defclass initializer
          (resource)
          ((name
            :initarg
            :name
            :type
            string
            :documentation
            "name of the process that is responsible for initializing this object."))
          (:documentation
           "Initializer is information about an initializer that has not yet completed."))

(defmethod unmarshal
  ((object initializer) source)
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass scale-status
          (resource)
          ((selector
            :initarg
            :selector
            :type
            (or string null)
            :documentation
            "label query over pods that should match the replicas count. This is same as the label selector but in the string format to avoid introspection by clients. The string will be in the same format as the query-param syntax. More info about label selectors: http://kubernetes.io/docs/user-guide/labels#label-selectors")
           (replicas
            :initarg
            :replicas
            :type
            integer
            :documentation
            "actual number of observed instances of the scaled object."))
          (:documentation
           "ScaleStatus represents the current status of a scale subresource."))

(defmethod unmarshal
  ((object scale-status) source)
  (multiple-value-bind (value present-p)
      (gethash "selector" source)
    (when present-p
      (setf (slot-value object 'selector)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value)))))


(defclass deployment
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "Deployment" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or deployment-status null)
            :documentation
            "Most recently observed status of the Deployment.")
           (spec
            :initarg
            :spec
            :type
            (or deployment-spec null)
            :documentation
            "Specification of the desired behavior of the Deployment.")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object metadata."))
          (:documentation
           "Deployment enables declarative updates for Pods and ReplicaSets."))

(defmethod unmarshal
  ((object deployment) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "DeploymentStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "DeploymentSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass pod-dns-config
          (resource)
          ((searches
            :initarg
            :searches
            :type
            list
            :documentation
            "A list of DNS search domains for host-name lookup. This will be appended to the base search paths generated from DNSPolicy. Duplicated search paths will be removed.")
           (options
            :initarg
            :options
            :type
            list
            :documentation
            "A list of DNS resolver options. This will be merged with the base options generated from DNSPolicy. Duplicated entries will be removed. Resolution options given in Options will override those that appear in the base DNSPolicy.")
           (nameservers
            :initarg
            :nameservers
            :type
            list
            :documentation
            "A list of DNS name server IP addresses. This will be appended to the base nameservers generated from DNSPolicy. Duplicated nameservers will be removed."))
          (:documentation
           "PodDNSConfig defines the DNS parameters of a pod in addition to those generated from DNSPolicy."))

(defmethod unmarshal
  ((object pod-dns-config) source)
  (multiple-value-bind (value present-p)
      (gethash "searches" source)
    (when present-p
      (setf (slot-value object 'searches)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "options" source)
    (when present-p
      (setf (slot-value object 'options)
            (decode-object
             (cons "array" "PodDNSConfigOption")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "nameservers" source)
    (when present-p
      (setf (slot-value object 'nameservers)
            (decode-object (cons "array" "string") value)))))


(defclass pod-security-context
          (resource)
          ((run-as-non-root
            :initarg
            :run-as-non-root
            :type
            (or boolean null)
            :documentation
            "Indicates that the container must run as a non-root user. If true, the Kubelet will validate the image at runtime to ensure that it does not run as UID 0 (root) and fail to start the container if it does. If unset or false, no such validation will be performed. May also be set in SecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence.")
           (fs-group
            :initarg
            :fs-group
            :type
            (or integer null)
            :documentation
            "A special supplemental group that applies to all containers in a pod. Some volume types allow the Kubelet to change the ownership of that volume to be owned by the pod:

1. The owning GID will be the FSGroup 2. The setgid bit is set (new files created in the volume will be owned by FSGroup) 3. The permission bits are OR'd with rw-rw ")
           (run-as-group
            :initarg
            :run-as-group
            :type
            (or integer null)
            :documentation
            "The GID to run the entrypoint of the container process. Uses runtime default if unset. May also be set in SecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence for that container.")
           (se-linux-options
            :initarg
            :se-linux-options
            :type
            (or se-linux-options null)
            :documentation
            "The SELinux context to be applied to all containers. If unspecified, the container runtime will allocate a random SELinux context for each container.  May also be set in SecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence for that container.")
           (supplemental-groups
            :initarg
            :supplemental-groups
            :type
            list
            :documentation
            "A list of groups applied to the first process run in each container, in addition to the container's primary GID.  If unspecified, no groups will be added to any container.")
           (run-as-user
            :initarg
            :run-as-user
            :type
            (or integer null)
            :documentation
            "The UID to run the entrypoint of the container process. Defaults to user specified in image metadata if unspecified. May also be set in SecurityContext.  If set in both SecurityContext and PodSecurityContext, the value specified in SecurityContext takes precedence for that container."))
          (:documentation
           "PodSecurityContext holds pod-level security attributes and common container settings. Some fields are also present in container.securityContext.  Field values of container.securityContext take precedence over field values of PodSecurityContext."))

(defmethod unmarshal
  ((object pod-security-context) source)
  (multiple-value-bind (value present-p)
      (gethash "runAsNonRoot" source)
    (when present-p
      (setf (slot-value object 'run-as-non-root)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsGroup" source)
    (when present-p
      (setf (slot-value object 'fs-group)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "runAsGroup" source)
    (when present-p
      (setf (slot-value object 'run-as-group)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "seLinuxOptions" source)
    (when present-p
      (setf (slot-value object 'se-linux-options)
            (decode-object "SELinuxOptions" value))))
  (multiple-value-bind (value present-p)
      (gethash "supplementalGroups" source)
    (when present-p
      (setf (slot-value object 'supplemental-groups)
            (decode-object (cons "array" "integer") value))))
  (multiple-value-bind (value present-p)
      (gethash "runAsUser" source)
    (when present-p
      (setf (slot-value object 'run-as-user)
            (decode-object (cons "integer" "int64") value)))))


(defclass downward-api-volume-source
          (resource)
          ((default-mode
            :initarg
            :default-mode
            :type
            (or integer null)
            :documentation
            "Optional: mode bits to use on created files by default. Must be a value between 0 and 0777. Defaults to 0644. Directories within the path are not affected by this setting. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set.")
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "Items is a list of downward API volume file"))
          (:documentation
           "DownwardAPIVolumeSource represents a volume containing downward API info. Downward API volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object downward-api-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "defaultMode" source)
    (when present-p
      (setf (slot-value object 'default-mode)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object
             (cons "array" "DownwardAPIVolumeFile")
             value)))))


(defclass host-path-volume-source
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "Path of the directory on the host. If the path is a symlink, it will follow the link to the real path. More info: https://kubernetes.io/docs/concepts/storage/volumes#hostpath")
           (type
            :initarg
            :type
            :type
            (or host-path-type null)
            :documentation
            "Type for HostPath Volume Defaults to \"\" More info: https://kubernetes.io/docs/concepts/storage/volumes#hostpath"))
          (:documentation
           "Represents a host path mapped into a pod. Host path volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object host-path-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type)
            (decode-object "HostPathType" value)))))


(defclass stateful-set-status
          (resource)
          ((conditions
            :initarg
            :conditions
            :type
            list
            :documentation
            "Represents the latest available observations of a statefulset's current state.")
           (update-revision
            :initarg
            :update-revision
            :type
            (or string null)
            :documentation
            "updateRevision, if not empty, indicates the version of the StatefulSet used to generate Pods in the sequence [replicas-updatedReplicas,replicas)")
           (ready-replicas
            :initarg
            :ready-replicas
            :type
            (or integer null)
            :documentation
            "readyReplicas is the number of Pods created by the StatefulSet controller that have a Ready Condition.")
           (current-revision
            :initarg
            :current-revision
            :type
            (or string null)
            :documentation
            "currentRevision, if not empty, indicates the version of the StatefulSet used to generate Pods in the sequence [0,currentReplicas).")
           (current-replicas
            :initarg
            :current-replicas
            :type
            (or integer null)
            :documentation
            "currentReplicas is the number of Pods created by the StatefulSet controller from the StatefulSet version indicated by currentRevision.")
           (observed-generation
            :initarg
            :observed-generation
            :type
            (or integer null)
            :documentation
            "observedGeneration is the most recent generation observed for this StatefulSet. It corresponds to the StatefulSet's generation, which is updated on mutation by the API Server.")
           (replicas
            :initarg
            :replicas
            :type
            integer
            :documentation
            "replicas is the number of Pods created by the StatefulSet controller.")
           (updated-replicas
            :initarg
            :updated-replicas
            :type
            (or integer null)
            :documentation
            "updatedReplicas is the number of Pods created by the StatefulSet controller from the StatefulSet version indicated by updateRevision.")
           (collision-count
            :initarg
            :collision-count
            :type
            (or integer null)
            :documentation
            "collisionCount is the count of hash collisions for the StatefulSet. The StatefulSet controller uses this field as a collision avoidance mechanism when it needs to create the name for the newest ControllerRevision."))
          (:documentation
           "StatefulSetStatus represents the current state of a StatefulSet."))

(defmethod unmarshal
  ((object stateful-set-status) source)
  (multiple-value-bind (value present-p)
      (gethash "conditions" source)
    (when present-p
      (setf (slot-value object 'conditions)
            (decode-object
             (cons "array" "StatefulSetCondition")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "updateRevision" source)
    (when present-p
      (setf (slot-value object 'update-revision)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readyReplicas" source)
    (when present-p
      (setf (slot-value object 'ready-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "currentRevision" source)
    (when present-p
      (setf (slot-value object 'current-revision)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "currentReplicas" source)
    (when present-p
      (setf (slot-value object 'current-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "observedGeneration" source)
    (when present-p
      (setf (slot-value object 'observed-generation)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "updatedReplicas" source)
    (when present-p
      (setf (slot-value object 'updated-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "collisionCount" source)
    (when present-p
      (setf (slot-value object 'collision-count)
            (decode-object (cons "integer" "int32") value)))))


(defclass node-selector-term
          (resource)
          ((match-fields
            :initarg
            :match-fields
            :type
            list
            :documentation
            "A list of node selector requirements by node's fields.")
           (match-expressions
            :initarg
            :match-expressions
            :type
            list
            :documentation
            "A list of node selector requirements by node's labels."))
          (:documentation
           "A null or empty node selector term matches no objects. The requirements of them are ANDed."))

(defmethod unmarshal
  ((object node-selector-term) source)
  (multiple-value-bind (value present-p)
      (gethash "matchFields" source)
    (when present-p
      (setf (slot-value object 'match-fields)
            (decode-object
             (cons "array" "NodeSelectorRequirement")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "matchExpressions" source)
    (when present-p
      (setf (slot-value object 'match-expressions)
            (decode-object
             (cons "array" "NodeSelectorRequirement")
             value)))))


(defclass volume-device
          (resource)
          ((device-path
            :initarg
            :device-path
            :type
            string
            :documentation
            "devicePath is the path inside of the container that the device will be mapped to.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "name must match the name of a persistentVolumeClaim in the pod"))
          (:documentation
           "volumeDevice describes a mapping of a raw block device within a container."))

(defmethod unmarshal
  ((object volume-device) source)
  (multiple-value-bind (value present-p)
      (gethash "devicePath" source)
    (when present-p
      (setf (slot-value object 'device-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass azure-data-disk-kind (resource) nil (:documentation nil))

(defmethod unmarshal ((object azure-data-disk-kind) source))


(defclass stateful-set-update-strategy
          (resource)
          ((rolling-update
            :initarg
            :rolling-update
            :type
            (or rolling-update-stateful-set-strategy null)
            :documentation
            "RollingUpdate is used to communicate parameters when Type is RollingUpdateStatefulSetStrategyType.")
           (type
            :initarg
            :type
            :type
            (or string null)
            :documentation
            "Type indicates the type of the StatefulSetUpdateStrategy. Default is RollingUpdate."))
          (:documentation
           "StatefulSetUpdateStrategy indicates the strategy that the StatefulSet controller will use to perform updates. It includes any additional parameters necessary to perform the update for the indicated strategy."))

(defmethod unmarshal
  ((object stateful-set-update-strategy) source)
  (multiple-value-bind (value present-p)
      (gethash "rollingUpdate" source)
    (when present-p
      (setf (slot-value object 'rolling-update)
            (decode-object "RollingUpdateStatefulSetStrategy" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value)))))


(defclass volume-mount
          (resource)
          ((mount-path
            :initarg
            :mount-path
            :type
            string
            :documentation
            "Path within the container at which the volume should be mounted.  Must not contain ':'.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Mounted read-only if true, read-write otherwise (false or unspecified). Defaults to false.")
           (sub-path
            :initarg
            :sub-path
            :type
            (or string null)
            :documentation
            "Path within the volume from which the container's volume should be mounted. Defaults to \"\" (volume's root).")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "This must match the Name of a Volume.")
           (mount-propagation
            :initarg
            :mount-propagation
            :type
            (or mount-propagation-mode null)
            :documentation
            "mountPropagation determines how mounts are propagated from the host to container and the other way around. When not set, MountPropagationHostToContainer is used. This field is beta in 1.10."))
          (:documentation
           "VolumeMount describes a mounting of a Volume within a container."))

(defmethod unmarshal
  ((object volume-mount) source)
  (multiple-value-bind (value present-p)
      (gethash "mountPath" source)
    (when present-p
      (setf (slot-value object 'mount-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "subPath" source)
    (when present-p
      (setf (slot-value object 'sub-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "mountPropagation" source)
    (when present-p
      (setf (slot-value object 'mount-propagation)
            (decode-object "MountPropagationMode" value)))))


(defclass glusterfs-volume-source
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "Path is the Glusterfs volume path. More info: https://releases.k8s.io/HEAD/examples/volumes/glusterfs/README.md#create-a-pod")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the Glusterfs volume to be mounted with read-only permissions. Defaults to false. More info: https://releases.k8s.io/HEAD/examples/volumes/glusterfs/README.md#create-a-pod")
           (endpoints
            :initarg
            :endpoints
            :type
            string
            :documentation
            "EndpointsName is the endpoint name that details Glusterfs topology. More info: https://releases.k8s.io/HEAD/examples/volumes/glusterfs/README.md#create-a-pod"))
          (:documentation
           "Represents a Glusterfs mount that lasts the lifetime of a pod. Glusterfs volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object glusterfs-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "endpoints" source)
    (when present-p
      (setf (slot-value object 'endpoints)
            (decode-object "string" value)))))


(defclass scale-spec
          (resource)
          ((replicas
            :initarg
            :replicas
            :type
            (or integer null)
            :documentation
            "desired number of instances for the scaled object."))
          (:documentation
           "ScaleSpec describes the attributes of a scale subresource."))

(defmethod unmarshal
  ((object scale-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value)))))


(defclass pod-affinity
          (resource)
          ((preferred-during-scheduling-ignored-during-execution
            :initarg
            :preferred-during-scheduling-ignored-during-execution
            :type
            list
            :documentation
            "The scheduler will prefer to schedule pods to nodes that satisfy the affinity expressions specified by this field, but it may choose a node that violates one or more of the expressions. The node that is most preferred is the one with the greatest sum of weights, i.e. for each node that meets all of the scheduling requirements (resource request, requiredDuringScheduling affinity expressions, etc.), compute a sum by iterating through the elements of this field and adding \"weight\" to the sum if the node has pods which matches the corresponding podAffinityTerm; the node(s) with the highest sum are the most preferred.")
           (required-during-scheduling-ignored-during-execution
            :initarg
            :required-during-scheduling-ignored-during-execution
            :type
            list
            :documentation
            "If the affinity requirements specified by this field are not met at scheduling time, the pod will not be scheduled onto the node. If the affinity requirements specified by this field cease to be met at some point during pod execution (e.g. due to a pod label update), the system may or may not try to eventually evict the pod from its node. When there are multiple elements, the lists of nodes corresponding to each podAffinityTerm are intersected, i.e. all terms must be satisfied."))
          (:documentation
           "Pod affinity is a group of inter pod affinity scheduling rules."))

(defmethod unmarshal
  ((object pod-affinity) source)
  (multiple-value-bind (value present-p)
      (gethash "preferredDuringSchedulingIgnoredDuringExecution"
               source)
    (when present-p
      (setf (slot-value object
                        'preferred-during-scheduling-ignored-during-execution)
            (decode-object
             (cons "array" "WeightedPodAffinityTerm")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "requiredDuringSchedulingIgnoredDuringExecution" source)
    (when present-p
      (setf (slot-value object
                        'required-during-scheduling-ignored-during-execution)
            (decode-object (cons "array" "PodAffinityTerm") value)))))


(defclass photon-persistent-disk-volume-source
          (resource)
          ((pd-id
            :initarg
            :pd-id
            :type
            string
            :documentation
            "ID that identifies Photon Controller persistent disk")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified."))
          (:documentation
           "Represents a Photon Controller persistent disk resource."))

(defmethod unmarshal
  ((object photon-persistent-disk-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "pdID" source)
    (when present-p
      (setf (slot-value object 'pd-id)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass daemon-set-status
          (resource)
          ((number-misscheduled
            :initarg
            :number-misscheduled
            :type
            integer
            :documentation
            "The number of nodes that are running the daemon pod, but are not supposed to run the daemon pod. More info: https://kubernetes.io/docs/concepts/workloads/controllers/daemonset/")
           (conditions
            :initarg
            :conditions
            :type
            list
            :documentation
            "Represents the latest available observations of a DaemonSet's current state.")
           (number-available
            :initarg
            :number-available
            :type
            (or integer null)
            :documentation
            "The number of nodes that should be running the daemon pod and have one or more of the daemon pod running and available (ready for at least spec.minReadySeconds)")
           (updated-number-scheduled
            :initarg
            :updated-number-scheduled
            :type
            (or integer null)
            :documentation
            "The total number of nodes that are running updated daemon pod")
           (current-number-scheduled
            :initarg
            :current-number-scheduled
            :type
            integer
            :documentation
            "The number of nodes that are running at least 1 daemon pod and are supposed to run the daemon pod. More info: https://kubernetes.io/docs/concepts/workloads/controllers/daemonset/")
           (desired-number-scheduled
            :initarg
            :desired-number-scheduled
            :type
            integer
            :documentation
            "The total number of nodes that should be running the daemon pod (including nodes correctly running the daemon pod). More info: https://kubernetes.io/docs/concepts/workloads/controllers/daemonset/")
           (observed-generation
            :initarg
            :observed-generation
            :type
            (or integer null)
            :documentation
            "The most recent generation observed by the daemon set controller.")
           (number-ready
            :initarg
            :number-ready
            :type
            integer
            :documentation
            "The number of nodes that should be running the daemon pod and have one or more of the daemon pod running and ready.")
           (number-unavailable
            :initarg
            :number-unavailable
            :type
            (or integer null)
            :documentation
            "The number of nodes that should be running the daemon pod and have none of the daemon pod running and available (ready for at least spec.minReadySeconds)")
           (collision-count
            :initarg
            :collision-count
            :type
            (or integer null)
            :documentation
            "Count of hash collisions for the DaemonSet. The DaemonSet controller uses this field as a collision avoidance mechanism when it needs to create the name for the newest ControllerRevision."))
          (:documentation
           "DaemonSetStatus represents the current status of a daemon set."))

(defmethod unmarshal
  ((object daemon-set-status) source)
  (multiple-value-bind (value present-p)
      (gethash "numberMisscheduled" source)
    (when present-p
      (setf (slot-value object 'number-misscheduled)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "conditions" source)
    (when present-p
      (setf (slot-value object 'conditions)
            (decode-object
             (cons "array" "DaemonSetCondition")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "numberAvailable" source)
    (when present-p
      (setf (slot-value object 'number-available)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "updatedNumberScheduled" source)
    (when present-p
      (setf (slot-value object 'updated-number-scheduled)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "currentNumberScheduled" source)
    (when present-p
      (setf (slot-value object 'current-number-scheduled)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "desiredNumberScheduled" source)
    (when present-p
      (setf (slot-value object 'desired-number-scheduled)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "observedGeneration" source)
    (when present-p
      (setf (slot-value object 'observed-generation)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "numberReady" source)
    (when present-p
      (setf (slot-value object 'number-ready)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "numberUnavailable" source)
    (when present-p
      (setf (slot-value object 'number-unavailable)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "collisionCount" source)
    (when present-p
      (setf (slot-value object 'collision-count)
            (decode-object (cons "integer" "int32") value)))))


(defclass capability (resource) nil (:documentation nil))

(defmethod unmarshal ((object capability) source))


(defclass resource-field-selector
          (resource)
          ((container-name
            :initarg
            :container-name
            :type
            (or string null)
            :documentation
            "Container name: required for volumes, optional for env vars")
           (resource
            :initarg
            :resource
            :type
            string
            :documentation
            "Required: resource to select")
           (divisor
            :initarg
            :divisor
            :type
            (or string null)
            :documentation
            "Specifies the output format of the exposed resources, defaults to \"1\""))
          (:documentation
           "ResourceFieldSelector represents container resources (cpu, memory) and their output format"))

(defmethod unmarshal
  ((object resource-field-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "containerName" source)
    (when present-p
      (setf (slot-value object 'container-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "resource" source)
    (when present-p
      (setf (slot-value object 'resource)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "divisor" source)
    (when present-p
      (setf (slot-value object 'divisor)
            (decode-object "string" value)))))


(defclass cinder-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Optional: Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts. More info: https://releases.k8s.io/HEAD/examples/mysql-cinder-pd/README.md")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://releases.k8s.io/HEAD/examples/mysql-cinder-pd/README.md")
           (volume-id
            :initarg
            :volume-id
            :type
            string
            :documentation
            "volume id used to identify the volume in cinder More info: https://releases.k8s.io/HEAD/examples/mysql-cinder-pd/README.md"))
          (:documentation
           "Represents a cinder volume resource in Openstack. A Cinder volume must exist before mounting to a container. The volume must also be in the same region as the kubelet. Cinder volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object cinder-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeID" source)
    (when present-p
      (setf (slot-value object 'volume-id)
            (decode-object "string" value)))))


(defclass node-affinity
          (resource)
          ((preferred-during-scheduling-ignored-during-execution
            :initarg
            :preferred-during-scheduling-ignored-during-execution
            :type
            list
            :documentation
            "The scheduler will prefer to schedule pods to nodes that satisfy the affinity expressions specified by this field, but it may choose a node that violates one or more of the expressions. The node that is most preferred is the one with the greatest sum of weights, i.e. for each node that meets all of the scheduling requirements (resource request, requiredDuringScheduling affinity expressions, etc.), compute a sum by iterating through the elements of this field and adding \"weight\" to the sum if the node matches the corresponding matchExpressions; the node(s) with the highest sum are the most preferred.")
           (required-during-scheduling-ignored-during-execution
            :initarg
            :required-during-scheduling-ignored-during-execution
            :type
            (or node-selector null)
            :documentation
            "If the affinity requirements specified by this field are not met at scheduling time, the pod will not be scheduled onto the node. If the affinity requirements specified by this field cease to be met at some point during pod execution (e.g. due to an update), the system may or may not try to eventually evict the pod from its node."))
          (:documentation
           "Node affinity is a group of node affinity scheduling rules."))

(defmethod unmarshal
  ((object node-affinity) source)
  (multiple-value-bind (value present-p)
      (gethash "preferredDuringSchedulingIgnoredDuringExecution"
               source)
    (when present-p
      (setf (slot-value object
                        'preferred-during-scheduling-ignored-during-execution)
            (decode-object
             (cons "array" "PreferredSchedulingTerm")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "requiredDuringSchedulingIgnoredDuringExecution" source)
    (when present-p
      (setf (slot-value object
                        'required-during-scheduling-ignored-during-execution)
            (decode-object "NodeSelector" value)))))


(defclass deletion-propagation (resource) nil (:documentation nil))

(defmethod unmarshal ((object deletion-propagation) source))


(defclass stateful-set-spec
          (resource)
          ((volume-claim-templates
            :initarg
            :volume-claim-templates
            :type
            list
            :documentation
            "volumeClaimTemplates is a list of claims that pods are allowed to reference. The StatefulSet controller is responsible for mapping network identities to claims in a way that maintains the identity of a pod. Every claim in this list must have at least one matching (by name) volumeMount in one container in the template. A claim in this list takes precedence over any volumes in the template, with the same name.")
           (selector
            :initarg
            :selector
            :type
            label-selector
            :documentation
            "selector is a label query over pods that should match the replica count. It must match the pod template's labels. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/labels/#label-selectors")
           (pod-management-policy
            :initarg
            :pod-management-policy
            :type
            (or string null)
            :documentation
            "podManagementPolicy controls how pods are created during initial scale up, when replacing pods on nodes, or when scaling down. The default policy is `OrderedReady`, where pods are created in increasing order (pod-0, then pod-1, etc) and the controller will wait until each pod is ready before continuing. When scaling down, the pods are removed in the opposite order. The alternative policy is `Parallel` which will create pods in parallel to match the desired scale without waiting, and on scale down will delete all pods at once.")
           (replicas
            :initarg
            :replicas
            :type
            (or integer null)
            :documentation
            "replicas is the desired number of replicas of the given Template. These are replicas in the sense that they are instantiations of the same Template, but individual replicas also have a consistent identity. If unspecified, defaults to 1.")
           (revision-history-limit
            :initarg
            :revision-history-limit
            :type
            (or integer null)
            :documentation
            "revisionHistoryLimit is the maximum number of revisions that will be maintained in the StatefulSet's revision history. The revision history consists of all revisions not represented by a currently applied StatefulSetSpec version. The default value is 10.")
           (template
            :initarg
            :template
            :type
            pod-template-spec
            :documentation
            "template is the object that describes the pod that will be created if insufficient replicas are detected. Each pod stamped out by the StatefulSet will fulfill this Template, but have a unique identity from the rest of the StatefulSet.")
           (service-name
            :initarg
            :service-name
            :type
            string
            :documentation
            "serviceName is the name of the service that governs this StatefulSet. This service must exist before the StatefulSet, and is responsible for the network identity of the set. Pods get DNS/hostnames that follow the pattern: pod-specific-string.serviceName.default.svc.cluster.local where \"pod-specific-string\" is managed by the StatefulSet controller.")
           (update-strategy
            :initarg
            :update-strategy
            :type
            (or stateful-set-update-strategy null)
            :documentation
            "updateStrategy indicates the StatefulSetUpdateStrategy that will be employed to update Pods in the StatefulSet when a revision is made to Template."))
          (:documentation
           "A StatefulSetSpec is the specification of a StatefulSet."))

(defmethod unmarshal
  ((object stateful-set-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "volumeClaimTemplates" source)
    (when present-p
      (setf (slot-value object 'volume-claim-templates)
            (decode-object
             (cons "array" "PersistentVolumeClaim")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "selector" source)
    (when present-p
      (setf (slot-value object 'selector)
            (decode-object "LabelSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "podManagementPolicy" source)
    (when present-p
      (setf (slot-value object 'pod-management-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "revisionHistoryLimit" source)
    (when present-p
      (setf (slot-value object 'revision-history-limit)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "template" source)
    (when present-p
      (setf (slot-value object 'template)
            (decode-object "PodTemplateSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "serviceName" source)
    (when present-p
      (setf (slot-value object 'service-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "updateStrategy" source)
    (when present-p
      (setf (slot-value object 'update-strategy)
            (decode-object "StatefulSetUpdateStrategy" value)))))


(defclass node-selector
          (resource)
          ((node-selector-terms
            :initarg
            :node-selector-terms
            :type
            list
            :documentation
            "Required. A list of node selector terms. The terms are ORed."))
          (:documentation
           "A node selector represents the union of the results of one or more label queries over a set of nodes; that is, it represents the OR of the selectors represented by the node selector terms."))

(defmethod unmarshal
  ((object node-selector) source)
  (multiple-value-bind (value present-p)
      (gethash "nodeSelectorTerms" source)
    (when present-p
      (setf (slot-value object 'node-selector-terms)
            (decode-object (cons "array" "NodeSelectorTerm") value)))))


(defclass api-resource
          (resource)
          ((kind :initform "APIResource" :allocation :class)
           (namespaced
            :initarg
            :namespaced
            :type
            boolean
            :documentation
            "namespaced indicates if a resource is namespaced or not.")
           (verbs
            :initarg
            :verbs
            :type
            list
            :documentation
            "verbs is a list of supported kube verbs (this includes get, list, watch, create, update, patch, delete, deletecollection, and proxy)")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "name is the plural name of the resource.")
           (categories
            :initarg
            :categories
            :type
            list
            :documentation
            "categories is a list of the grouped resources this resource belongs to (e.g. 'all')")
           (singular-name
            :initarg
            :singular-name
            :type
            string
            :documentation
            "singularName is the singular name of the resource.  This allows clients to handle plural and singular opaquely. The singularName is more correct for reporting status on a single item and both singular and plural are allowed from the kubectl CLI interface.")
           (group
            :initarg
            :group
            :type
            (or string null)
            :documentation
            "group is the preferred group of the resource.  Empty implies the group of the containing resource list. For subresources, this may have a different value, for example: Scale\".")
           (version
            :initarg
            :version
            :type
            (or string null)
            :documentation
            "version is the preferred version of the resource.  Empty implies the version of the containing resource list For subresources, this may have a different value, for example: v1 (while inside a v1beta1 version of the core resource's group)\".")
           (short-names
            :initarg
            :short-names
            :type
            list
            :documentation
            "shortNames is a list of suggested short names of the resource."))
          (:documentation
           "APIResource specifies the name of a resource and whether it is namespaced."))

(defmethod unmarshal
  ((object api-resource) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "namespaced" source)
    (when present-p
      (setf (slot-value object 'namespaced)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "verbs" source)
    (when present-p
      (setf (slot-value object 'verbs)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "categories" source)
    (when present-p
      (setf (slot-value object 'categories)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "singularName" source)
    (when present-p
      (setf (slot-value object 'singular-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "group" source)
    (when present-p
      (setf (slot-value object 'group)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "version" source)
    (when present-p
      (setf (slot-value object 'version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "shortNames" source)
    (when present-p
      (setf (slot-value object 'short-names)
            (decode-object (cons "array" "string") value)))))


(defclass pod-affinity-term
          (resource)
          ((topology-key
            :initarg
            :topology-key
            :type
            string
            :documentation
            "This pod should be co-located (affinity) or not co-located (anti-affinity) with the pods matching the labelSelector in the specified namespaces, where co-located is defined as running on a node whose value of the label with key topologyKey matches that of any node on which any of the selected pods is running. Empty topologyKey is not allowed.")
           (label-selector
            :initarg
            :label-selector
            :type
            (or label-selector null)
            :documentation
            "A label query over a set of resources, in this case pods.")
           (namespaces
            :initarg
            :namespaces
            :type
            list
            :documentation
            "namespaces specifies which namespaces the labelSelector applies to (matches against); null or empty list means \"this pod's namespace\""))
          (:documentation
           "Defines a set of pods (namely those matching the labelSelector relative to the given namespace(s)) that this pod should be co-located (affinity) or not co-located (anti-affinity) with, where co-located is defined as running on a node whose value of the label with key <topologyKey> matches that of any node on which a pod of the set of pods is running"))

(defmethod unmarshal
  ((object pod-affinity-term) source)
  (multiple-value-bind (value present-p)
      (gethash "topologyKey" source)
    (when present-p
      (setf (slot-value object 'topology-key)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "labelSelector" source)
    (when present-p
      (setf (slot-value object 'label-selector)
            (decode-object "LabelSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "namespaces" source)
    (when present-p
      (setf (slot-value object 'namespaces)
            (decode-object (cons "array" "string") value)))))


(defclass mount-propagation-mode (resource) nil (:documentation nil))

(defmethod unmarshal ((object mount-propagation-mode) source))


(defclass persistent-volume-mode (resource) nil (:documentation nil))

(defmethod unmarshal ((object persistent-volume-mode) source))


(defclass local-object-reference
          (resource)
          ((name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names"))
          (:documentation
           "LocalObjectReference contains enough information to let you locate the referenced object inside the same namespace."))

(defmethod unmarshal
  ((object local-object-reference) source)
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value)))))


(defclass status
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "Status" :allocation :class)
           (message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "A human-readable description of the status of this operation.")
           (status
            :initarg
            :status
            :type
            (or string null)
            :documentation
            "Status of the operation. One of: \"Success\" or \"Failure\". More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (code
            :initarg
            :code
            :type
            (or integer null)
            :documentation
            "Suggested HTTP return code for this status, 0 if not set.")
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "A machine-readable description of why this operation is in the \"Failure\" status. If this value is empty there is no information available. A Reason clarifies an HTTP status code but does not override it.")
           (details
            :initarg
            :details
            :type
            (or status-details null)
            :documentation
            "Extended data associated with the reason.  Each reason may define its own extended details. This field is optional and the data returned is not guaranteed to conform to any schema except that defined by the reason type.")
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            "Standard list metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#types-kinds"))
          (:documentation
           "Status is a return value for calls that don't return other objects."))

(defmethod unmarshal
  ((object status) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "code" source)
    (when present-p
      (setf (slot-value object 'code)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "details" source)
    (when present-p
      (setf (slot-value object 'details)
            (decode-object "StatusDetails" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass azure-data-disk-caching-mode
          (resource)
          nil
          (:documentation nil))

(defmethod unmarshal ((object azure-data-disk-caching-mode) source))


(defclass node-selector-requirement
          (resource)
          ((key
            :initarg
            :key
            :type
            string
            :documentation
            "The label key that the selector applies to.")
           (values :initarg
                   :values
                   :type
                   list
                   :documentation
                   "An array of string values. If the operator is In or NotIn, the values array must be non-empty. If the operator is Exists or DoesNotExist, the values array must be empty. If the operator is Gt or Lt, the values array must have a single element, which will be interpreted as an integer. This array is replaced during a strategic merge patch.")
           (operator
            :initarg
            :operator
            :type
            string
            :documentation
            "Represents a key's relationship to a set of values. Valid operators are In, NotIn, Exists, DoesNotExist. Gt, and Lt."))
          (:documentation
           "A node selector requirement is a selector that contains values, a key, and an operator that relates the key and values."))

(defmethod unmarshal
  ((object node-selector-requirement) source)
  (multiple-value-bind (value present-p)
      (gethash "key" source)
    (when present-p
      (setf (slot-value object 'key) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "values" source)
    (when present-p
      (setf (slot-value object 'values)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "operator" source)
    (when present-p
      (setf (slot-value object 'operator)
            (decode-object "string" value)))))


(defclass persistent-volume-claim-status
          (resource)
          ((conditions
            :initarg
            :conditions
            :type
            list
            :documentation
            "Current Condition of persistent volume claim. If underlying persistent volume is being resized then the Condition will be set to 'ResizeStarted'.")
           (phase :initarg
                  :phase
                  :type
                  (or string null)
                  :documentation
                  "Phase represents the current phase of PersistentVolumeClaim.")
           (capacity
            :initarg
            :capacity
            :type
            (or hash-table null)
            :documentation
            "Represents the actual resources of the underlying volume.")
           (access-modes
            :initarg
            :access-modes
            :type
            list
            :documentation
            "AccessModes contains the actual access modes the volume backing the PVC has. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#access-modes-1"))
          (:documentation
           "PersistentVolumeClaimStatus is the current status of a persistent volume claim."))

(defmethod unmarshal
  ((object persistent-volume-claim-status) source)
  (multiple-value-bind (value present-p)
      (gethash "conditions" source)
    (when present-p
      (setf (slot-value object 'conditions)
            (decode-object
             (cons "array" "PersistentVolumeClaimCondition")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "phase" source)
    (when present-p
      (setf (slot-value object 'phase)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "capacity" source)
    (when present-p
      (setf (slot-value object 'capacity)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "accessModes" source)
    (when present-p
      (setf (slot-value object 'access-modes)
            (decode-object
             (cons "array" "PersistentVolumeAccessMode")
             value)))))


(defclass projected-volume-source
          (resource)
          ((default-mode
            :initarg
            :default-mode
            :type
            (or integer null)
            :documentation
            "Mode bits to use on created files by default. Must be a value between 0 and 0777. Directories within the path are not affected by this setting. This might be in conflict with other options that affect the file mode, like fsGroup, and the result can be other mode bits set.")
           (sources
            :initarg
            :sources
            :type
            list
            :documentation
            "list of volume projections"))
          (:documentation "Represents a projected volume source"))

(defmethod unmarshal
  ((object projected-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "defaultMode" source)
    (when present-p
      (setf (slot-value object 'default-mode)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "sources" source)
    (when present-p
      (setf (slot-value object 'sources)
            (decode-object (cons "array" "VolumeProjection") value)))))


(defclass storage-os-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or local-object-reference null)
            :documentation
            "SecretRef specifies the secret to use for obtaining the StorageOS API credentials.  If not specified, default values will be attempted.")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Defaults to false (read/write). ReadOnly here will force the ReadOnly setting in VolumeMounts.")
           (volume-namespace
            :initarg
            :volume-namespace
            :type
            (or string null)
            :documentation
            "VolumeNamespace specifies the scope of the volume within StorageOS.  If no namespace is specified then the Pod's namespace will be used.  This allows the Kubernetes name scoping to be mirrored within StorageOS for tighter integration. Set VolumeName to any name to override the default behaviour. Set to \"default\" if you are not using namespaces within StorageOS. Namespaces that do not pre-exist within StorageOS will be created.")
           (volume-name
            :initarg
            :volume-name
            :type
            (or string null)
            :documentation
            "VolumeName is the human-readable name of the StorageOS volume.  Volume names are only unique within a namespace.")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type to mount. Must be a filesystem type supported by the host operating system. Ex. \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified."))
          (:documentation
           "Represents a StorageOS persistent volume resource."))

(defmethod unmarshal
  ((object storage-os-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeNamespace" source)
    (when present-p
      (setf (slot-value object 'volume-namespace)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeName" source)
    (when present-p
      (setf (slot-value object 'volume-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))


(defclass status-details
          (resource)
          ((kind :initform "StatusDetails" :allocation :class)
           (causes
            :initarg
            :causes
            :type
            list
            :documentation
            "The Causes array includes more details associated with the StatusReason failure. Not all StatusReasons may provide detailed causes.")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "The name attribute of the resource associated with the status StatusReason (when there is a single name which can be described).")
           (group
            :initarg
            :group
            :type
            (or string null)
            :documentation
            "The group attribute of the resource associated with the status StatusReason.")
           (retry-after-seconds
            :initarg
            :retry-after-seconds
            :type
            (or integer null)
            :documentation
            "If specified, the time in seconds before the operation should be retried. Some errors may indicate the client must take an alternate action - for those errors this field may indicate how long to wait before taking the alternate action.")
           (uid
            :initarg
            :uid
            :type
            (or string null)
            :documentation
            "UID of the resource. (when there is a single resource which can be described). More info: http://kubernetes.io/docs/user-guide/identifiers#uids"))
          (:documentation
           "StatusDetails is a set of additional properties that MAY be set by the server to provide additional information about a response. The Reason field of a Status object defines what attributes will be set. Clients must ignore fields that do not match the defined type of each attribute, and should assume that any attribute may be empty, invalid, or under defined."))

(defmethod unmarshal
  ((object status-details) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "causes" source)
    (when present-p
      (setf (slot-value object 'causes)
            (decode-object (cons "array" "StatusCause") value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "group" source)
    (when present-p
      (setf (slot-value object 'group)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "retryAfterSeconds" source)
    (when present-p
      (setf (slot-value object 'retry-after-seconds)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "uid" source)
    (when present-p
      (setf (slot-value object 'uid) (decode-object "string" value)))))


(defclass lifecycle
          (resource)
          ((pre-stop
            :initarg
            :pre-stop
            :type
            (or handler null)
            :documentation
            "PreStop is called immediately before a container is terminated. The container is terminated after the handler completes. The reason for termination is passed to the handler. Regardless of the outcome of the handler, the container is eventually terminated. Other management of the container blocks until the hook completes. More info: https://kubernetes.io/docs/concepts/containers/container-lifecycle-hooks/#container-hooks")
           (post-start
            :initarg
            :post-start
            :type
            (or handler null)
            :documentation
            "PostStart is called immediately after a container is created. If the handler fails, the container is terminated and restarted according to its restart policy. Other management of the container blocks until the hook completes. More info: https://kubernetes.io/docs/concepts/containers/container-lifecycle-hooks/#container-hooks"))
          (:documentation
           "Lifecycle describes actions that the management system should take in response to container lifecycle events. For the PostStart and PreStop lifecycle handlers, management of the container blocks until the action is complete, unless the container process fails, in which case the handler is aborted."))

(defmethod unmarshal
  ((object lifecycle) source)
  (multiple-value-bind (value present-p)
      (gethash "preStop" source)
    (when present-p
      (setf (slot-value object 'pre-stop)
            (decode-object "Handler" value))))
  (multiple-value-bind (value present-p)
      (gethash "postStart" source)
    (when present-p
      (setf (slot-value object 'post-start)
            (decode-object "Handler" value)))))


(defclass pod-template-spec
          (resource)
          ((spec
            :initarg
            :spec
            :type
            (or pod-spec null)
            :documentation
            "Specification of the desired behavior of the pod. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#spec-and-status")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            "Standard object's metadata. More info: https://git.k8s.io/community/contributors/devel/api-conventions.md#metadata"))
          (:documentation
           "PodTemplateSpec describes the data a pod should have when created from a template"))

(defmethod unmarshal
  ((object pod-template-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "PodSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass volume
          (resource)
          ((storageos
            :initarg
            :storageos
            :type
            (or storage-os-volume-source null)
            :documentation
            "StorageOS represents a StorageOS volume attached and mounted on Kubernetes nodes.")
           (cinder
            :initarg
            :cinder
            :type
            (or cinder-volume-source null)
            :documentation
            "Cinder represents a cinder volume attached and mounted on kubelets host machine More info: https://releases.k8s.io/HEAD/examples/mysql-cinder-pd/README.md")
           (aws-elastic-block-store
            :initarg
            :aws-elastic-block-store
            :type
            (or aws-elastic-block-store-volume-source null)
            :documentation
            "AWSElasticBlockStore represents an AWS Disk resource that is attached to a kubelet's host machine and then exposed to the pod. More info: https://kubernetes.io/docs/concepts/storage/volumes#awselasticblockstore")
           (azure-disk
            :initarg
            :azure-disk
            :type
            (or azure-disk-volume-source null)
            :documentation
            "AzureDisk represents an Azure Data Disk mount on the host and bind mount to the pod.")
           (quobyte
            :initarg
            :quobyte
            :type
            (or quobyte-volume-source null)
            :documentation
            "Quobyte represents a Quobyte mount on the host that shares a pod's lifetime")
           (config-map
            :initarg
            :config-map
            :type
            (or config-map-volume-source null)
            :documentation
            "ConfigMap represents a configMap that should populate this volume")
           (flex-volume
            :initarg
            :flex-volume
            :type
            (or flex-volume-source null)
            :documentation
            "FlexVolume represents a generic volume resource that is provisioned/attached using an exec based plugin.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "Volume's name. Must be a DNS_LABEL and unique within the pod. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names")
           (projected
            :initarg
            :projected
            :type
            (or projected-volume-source null)
            :documentation
            "Items for all in one resources secrets, configmaps, and downward API")
           (downward-api
            :initarg
            :downward-api
            :type
            (or downward-api-volume-source null)
            :documentation
            "DownwardAPI represents downward API about the pod that should populate this volume")
           (rbd
            :initarg
            :rbd
            :type
            (or rbd-volume-source null)
            :documentation
            "RBD represents a Rados Block Device mount on the host that shares a pod's lifetime. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md")
           (persistent-volume-claim
            :initarg
            :persistent-volume-claim
            :type
            (or persistent-volume-claim-volume-source null)
            :documentation
            "PersistentVolumeClaimVolumeSource represents a reference to a PersistentVolumeClaim in the same namespace. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistentvolumeclaims")
           (azure-file
            :initarg
            :azure-file
            :type
            (or azure-file-volume-source null)
            :documentation
            "AzureFile represents an Azure File Service mount on the host and bind mount to the pod.")
           (glusterfs
            :initarg
            :glusterfs
            :type
            (or glusterfs-volume-source null)
            :documentation
            "Glusterfs represents a Glusterfs mount on the host that shares a pod's lifetime. More info: https://releases.k8s.io/HEAD/examples/volumes/glusterfs/README.md")
           (secret
            :initarg
            :secret
            :type
            (or secret-volume-source null)
            :documentation
            "Secret represents a secret that should populate this volume. More info: https://kubernetes.io/docs/concepts/storage/volumes#secret")
           (host-path
            :initarg
            :host-path
            :type
            (or host-path-volume-source null)
            :documentation
            "HostPath represents a pre-existing file or directory on the host machine that is directly exposed to the container. This is generally used for system agents or other privileged things that are allowed to see the host machine. Most containers will NOT need this. More info: https://kubernetes.io/docs/concepts/storage/volumes#hostpath")
           (portworx-volume
            :initarg
            :portworx-volume
            :type
            (or portworx-volume-source null)
            :documentation
            "PortworxVolume represents a portworx volume attached and mounted on kubelets host machine")
           (flocker
            :initarg
            :flocker
            :type
            (or flocker-volume-source null)
            :documentation
            "Flocker represents a Flocker volume attached to a kubelet's host machine. This depends on the Flocker control service being running")
           (nfs
            :initarg
            :nfs
            :type
            (or nfs-volume-source null)
            :documentation
            "NFS represents an NFS mount on the host that shares a pod's lifetime More info: https://kubernetes.io/docs/concepts/storage/volumes#nfs")
           (gce-persistent-disk
            :initarg
            :gce-persistent-disk
            :type
            (or gce-persistent-disk-volume-source null)
            :documentation
            "GCEPersistentDisk represents a GCE Disk resource that is attached to a kubelet's host machine and then exposed to the pod. More info: https://kubernetes.io/docs/concepts/storage/volumes#gcepersistentdisk")
           (fc
            :initarg
            :fc
            :type
            (or fc-volume-source null)
            :documentation
            "FC represents a Fibre Channel resource that is attached to a kubelet's host machine and then exposed to the pod.")
           (scale-io
            :initarg
            :scale-io
            :type
            (or scale-io-volume-source null)
            :documentation
            "ScaleIO represents a ScaleIO persistent volume attached and mounted on Kubernetes nodes.")
           (cephfs
            :initarg
            :cephfs
            :type
            (or ceph-fs-volume-source null)
            :documentation
            "CephFS represents a Ceph FS mount on the host that shares a pod's lifetime")
           (git-repo
            :initarg
            :git-repo
            :type
            (or git-repo-volume-source null)
            :documentation
            "GitRepo represents a git repository at a particular revision.")
           (iscsi
            :initarg
            :iscsi
            :type
            (or iscsi-volume-source null)
            :documentation
            "ISCSI represents an ISCSI Disk resource that is attached to a kubelet's host machine and then exposed to the pod. More info: https://releases.k8s.io/HEAD/examples/volumes/iscsi/README.md")
           (empty-dir
            :initarg
            :empty-dir
            :type
            (or empty-dir-volume-source null)
            :documentation
            "EmptyDir represents a temporary directory that shares a pod's lifetime. More info: https://kubernetes.io/docs/concepts/storage/volumes#emptydir")
           (photon-persistent-disk
            :initarg
            :photon-persistent-disk
            :type
            (or photon-persistent-disk-volume-source null)
            :documentation
            "PhotonPersistentDisk represents a PhotonController persistent disk attached and mounted on kubelets host machine")
           (vsphere-volume
            :initarg
            :vsphere-volume
            :type
            (or vsphere-virtual-disk-volume-source null)
            :documentation
            "VsphereVolume represents a vSphere volume attached and mounted on kubelets host machine"))
          (:documentation
           "Volume represents a named volume in a pod that may be accessed by any container in the pod."))

(defmethod unmarshal
  ((object volume) source)
  (multiple-value-bind (value present-p)
      (gethash "storageos" source)
    (when present-p
      (setf (slot-value object 'storageos)
            (decode-object "StorageOSVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "cinder" source)
    (when present-p
      (setf (slot-value object 'cinder)
            (decode-object "CinderVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "awsElasticBlockStore" source)
    (when present-p
      (setf (slot-value object 'aws-elastic-block-store)
            (decode-object "AWSElasticBlockStoreVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "azureDisk" source)
    (when present-p
      (setf (slot-value object 'azure-disk)
            (decode-object "AzureDiskVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "quobyte" source)
    (when present-p
      (setf (slot-value object 'quobyte)
            (decode-object "QuobyteVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "configMap" source)
    (when present-p
      (setf (slot-value object 'config-map)
            (decode-object "ConfigMapVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "flexVolume" source)
    (when present-p
      (setf (slot-value object 'flex-volume)
            (decode-object "FlexVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "projected" source)
    (when present-p
      (setf (slot-value object 'projected)
            (decode-object "ProjectedVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "downwardAPI" source)
    (when present-p
      (setf (slot-value object 'downward-api)
            (decode-object "DownwardAPIVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "rbd" source)
    (when present-p
      (setf (slot-value object 'rbd)
            (decode-object "RBDVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "persistentVolumeClaim" source)
    (when present-p
      (setf (slot-value object 'persistent-volume-claim)
            (decode-object
             "PersistentVolumeClaimVolumeSource"
             value))))
  (multiple-value-bind (value present-p)
      (gethash "azureFile" source)
    (when present-p
      (setf (slot-value object 'azure-file)
            (decode-object "AzureFileVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "glusterfs" source)
    (when present-p
      (setf (slot-value object 'glusterfs)
            (decode-object "GlusterfsVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "secret" source)
    (when present-p
      (setf (slot-value object 'secret)
            (decode-object "SecretVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "hostPath" source)
    (when present-p
      (setf (slot-value object 'host-path)
            (decode-object "HostPathVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "portworxVolume" source)
    (when present-p
      (setf (slot-value object 'portworx-volume)
            (decode-object "PortworxVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "flocker" source)
    (when present-p
      (setf (slot-value object 'flocker)
            (decode-object "FlockerVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "nfs" source)
    (when present-p
      (setf (slot-value object 'nfs)
            (decode-object "NFSVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "gcePersistentDisk" source)
    (when present-p
      (setf (slot-value object 'gce-persistent-disk)
            (decode-object "GCEPersistentDiskVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "fc" source)
    (when present-p
      (setf (slot-value object 'fc)
            (decode-object "FCVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "scaleIO" source)
    (when present-p
      (setf (slot-value object 'scale-io)
            (decode-object "ScaleIOVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "cephfs" source)
    (when present-p
      (setf (slot-value object 'cephfs)
            (decode-object "CephFSVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "gitRepo" source)
    (when present-p
      (setf (slot-value object 'git-repo)
            (decode-object "GitRepoVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "iscsi" source)
    (when present-p
      (setf (slot-value object 'iscsi)
            (decode-object "ISCSIVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "emptyDir" source)
    (when present-p
      (setf (slot-value object 'empty-dir)
            (decode-object "EmptyDirVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "photonPersistentDisk" source)
    (when present-p
      (setf (slot-value object 'photon-persistent-disk)
            (decode-object "PhotonPersistentDiskVolumeSource" value))))
  (multiple-value-bind (value present-p)
      (gethash "vsphereVolume" source)
    (when present-p
      (setf (slot-value object 'vsphere-volume)
            (decode-object "VsphereVirtualDiskVolumeSource" value)))))


(defclass stateful-set-list
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "StatefulSetList" :allocation :class)
           (items :initarg :items :type list :documentation nil)
           (metadata
            :initarg
            :metadata
            :type
            (or list-meta null)
            :documentation
            nil))
          (:documentation
           "StatefulSetList is a collection of StatefulSets."))

(defmethod unmarshal
  ((object stateful-set-list) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "StatefulSet") value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ListMeta" value)))))


(defclass git-repo-volume-source
          (resource)
          ((repository
            :initarg
            :repository
            :type
            string
            :documentation
            "Repository URL")
           (directory :initarg
                      :directory
                      :type
                      (or string null)
                      :documentation
                      "Target directory name. Must not contain or start with '..'.  If '.' is supplied, the volume directory will be the git repository.  Otherwise, if specified, the volume will contain the git repository in the subdirectory with the given name.")
           (revision
            :initarg
            :revision
            :type
            (or string null)
            :documentation
            "Commit hash for the specified revision."))
          (:documentation
           "Represents a volume that is populated with the contents of a git repository. Git repo volumes do not support ownership management. Git repo volumes support SELinux relabeling."))

(defmethod unmarshal
  ((object git-repo-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "repository" source)
    (when present-p
      (setf (slot-value object 'repository)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "directory" source)
    (when present-p
      (setf (slot-value object 'directory)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "revision" source)
    (when present-p
      (setf (slot-value object 'revision)
            (decode-object "string" value)))))


(defclass nfs-volume-source
          (resource)
          ((path
            :initarg
            :path
            :type
            string
            :documentation
            "Path that is exported by the NFS server. More info: https://kubernetes.io/docs/concepts/storage/volumes#nfs")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the NFS export to be mounted with read-only permissions. Defaults to false. More info: https://kubernetes.io/docs/concepts/storage/volumes#nfs")
           (server
            :initarg
            :server
            :type
            string
            :documentation
            "Server is the hostname or IP address of the NFS server. More info: https://kubernetes.io/docs/concepts/storage/volumes#nfs"))
          (:documentation
           "Represents an NFS mount that lasts the lifetime of a pod. NFS volumes do not support ownership management or SELinux relabeling."))

(defmethod unmarshal
  ((object nfs-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "path" source)
    (when present-p
      (setf (slot-value object 'path) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "server" source)
    (when present-p
      (setf (slot-value object 'server)
            (decode-object "string" value)))))


(defclass container
          (resource)
          ((resources
            :initarg
            :resources
            :type
            (or resource-requirements null)
            :documentation
            "Compute Resources required by this container. Cannot be updated. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#resources")
           (liveness-probe
            :initarg
            :liveness-probe
            :type
            (or probe null)
            :documentation
            "Periodic probe of container liveness. Container will be restarted if the probe fails. Cannot be updated. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes")
           (volume-devices
            :initarg
            :volume-devices
            :type
            list
            :documentation
            "volumeDevices is the list of block devices to be used by the container. This is an alpha feature and may change in the future.")
           (volume-mounts
            :initarg
            :volume-mounts
            :type
            list
            :documentation
            "Pod volumes to mount into the container's filesystem. Cannot be updated.")
           (tty
            :initarg
            :tty
            :type
            (or boolean null)
            :documentation
            "Whether this container should allocate a TTY for itself, also requires 'stdin' to be true. Default is false.")
           (stdin
            :initarg
            :stdin
            :type
            (or boolean null)
            :documentation
            "Whether this container should allocate a buffer for stdin in the container runtime. If this is not set, reads from stdin in the container will always result in EOF. Default is false.")
           (env-from
            :initarg
            :env-from
            :type
            list
            :documentation
            "List of sources to populate environment variables in the container. The keys defined within a source must be a C_IDENTIFIER. All invalid keys will be reported as an event when the container is starting. When a key exists in multiple sources, the value associated with the last source will take precedence. Values defined by an Env with a duplicate key will take precedence. Cannot be updated.")
           (image
            :initarg
            :image
            :type
            (or string null)
            :documentation
            "Docker image name. More info: https://kubernetes.io/docs/concepts/containers/images This field is optional to allow higher level config management to default or override container images in workload controllers like Deployments and StatefulSets.")
           (stdin-once
            :initarg
            :stdin-once
            :type
            (or boolean null)
            :documentation
            "Whether the container runtime should close the stdin channel after it has been opened by a single attach. When stdin is true the stdin stream will remain open across multiple attach sessions. If stdinOnce is set to true, stdin is opened on container start, is empty until the first client attaches to stdin, and then remains open and accepts data until the client disconnects, at which time stdin is closed and remains closed until the container is restarted. If this flag is false, a container processes that reads from stdin will never receive an EOF. Default is false")
           (security-context
            :initarg
            :security-context
            :type
            (or security-context null)
            :documentation
            "Security options the pod should run with. More info: https://kubernetes.io/docs/concepts/policy/security-context/ More info: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/")
           (termination-message-policy
            :initarg
            :termination-message-policy
            :type
            (or string null)
            :documentation
            "Indicate how the termination message should be populated. File will use the contents of terminationMessagePath to populate the container status message on both success and failure. FallbackToLogsOnError will use the last chunk of container log output if the termination message file is empty and the container exited with an error. The log output is limited to 2048 bytes or 80 lines, whichever is smaller. Defaults to File. Cannot be updated.")
           (ports
            :initarg
            :ports
            :type
            list
            :documentation
            "List of ports to expose from the container. Exposing a port here gives the system additional information about the network connections a container uses, but is primarily informational. Not specifying a port here DOES NOT prevent that port from being exposed. Any port which is listening on the default \"0.0.0.0\" address inside a container will be accessible from the network. Cannot be updated.")
           (name
            :initarg
            :name
            :type
            string
            :documentation
            "Name of the container specified as a DNS_LABEL. Each container in a pod must have a unique name (DNS_LABEL). Cannot be updated.")
           (env
            :initarg
            :env
            :type
            list
            :documentation
            "List of environment variables to set in the container. Cannot be updated.")
           (readiness-probe
            :initarg
            :readiness-probe
            :type
            (or probe null)
            :documentation
            "Periodic probe of container service readiness. Container will be removed from service endpoints if the probe fails. Cannot be updated. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes")
           (image-pull-policy
            :initarg
            :image-pull-policy
            :type
            (or string null)
            :documentation
            "Image pull policy. One of Always, Never, IfNotPresent. Defaults to Always if :latest tag is specified, or IfNotPresent otherwise. Cannot be updated. More info: https://kubernetes.io/docs/concepts/containers/images#updating-images")
           (command
            :initarg
            :command
            :type
            list
            :documentation
            "Entrypoint array. Not executed within a shell. The docker image's ENTRYPOINT is used if this is not provided. Variable references $(VAR_NAME) are expanded using the container's environment. If a variable cannot be resolved, the reference in the input string will be unchanged. The $(VAR_NAME) syntax can be escaped with a double $$, ie: $$(VAR_NAME). Escaped references will never be expanded, regardless of whether the variable exists or not. Cannot be updated. More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell")
           (termination-message-path
            :initarg
            :termination-message-path
            :type
            (or string null)
            :documentation
            "Optional: Path at which the file to which the container's termination message will be written is mounted into the container's filesystem. Message written is intended to be brief final status, such as an assertion failure message. Will be truncated by the node if greater than 4096 bytes. The total message length across all containers will be limited to 12kb. Defaults to /dev/termination-log. Cannot be updated.")
           (args
            :initarg
            :args
            :type
            list
            :documentation
            "Arguments to the entrypoint. The docker image's CMD is used if this is not provided. Variable references $(VAR_NAME) are expanded using the container's environment. If a variable cannot be resolved, the reference in the input string will be unchanged. The $(VAR_NAME) syntax can be escaped with a double $$, ie: $$(VAR_NAME). Escaped references will never be expanded, regardless of whether the variable exists or not. Cannot be updated. More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell")
           (lifecycle
            :initarg
            :lifecycle
            :type
            (or lifecycle null)
            :documentation
            "Actions that the management system should take in response to container lifecycle events. Cannot be updated.")
           (working-dir
            :initarg
            :working-dir
            :type
            (or string null)
            :documentation
            "Container's working directory. If not specified, the container runtime's default will be used, which might be configured in the container image. Cannot be updated."))
          (:documentation
           "A single application container that you want to run within a pod."))

(defmethod unmarshal
  ((object container) source)
  (multiple-value-bind (value present-p)
      (gethash "resources" source)
    (when present-p
      (setf (slot-value object 'resources)
            (decode-object "ResourceRequirements" value))))
  (multiple-value-bind (value present-p)
      (gethash "livenessProbe" source)
    (when present-p
      (setf (slot-value object 'liveness-probe)
            (decode-object "Probe" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeDevices" source)
    (when present-p
      (setf (slot-value object 'volume-devices)
            (decode-object (cons "array" "VolumeDevice") value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeMounts" source)
    (when present-p
      (setf (slot-value object 'volume-mounts)
            (decode-object (cons "array" "VolumeMount") value))))
  (multiple-value-bind (value present-p)
      (gethash "tty" source)
    (when present-p
      (setf (slot-value object 'tty) (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "stdin" source)
    (when present-p
      (setf (slot-value object 'stdin)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "envFrom" source)
    (when present-p
      (setf (slot-value object 'env-from)
            (decode-object (cons "array" "EnvFromSource") value))))
  (multiple-value-bind (value present-p)
      (gethash "image" source)
    (when present-p
      (setf (slot-value object 'image)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "stdinOnce" source)
    (when present-p
      (setf (slot-value object 'stdin-once)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "securityContext" source)
    (when present-p
      (setf (slot-value object 'security-context)
            (decode-object "SecurityContext" value))))
  (multiple-value-bind (value present-p)
      (gethash "terminationMessagePolicy" source)
    (when present-p
      (setf (slot-value object 'termination-message-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "ports" source)
    (when present-p
      (setf (slot-value object 'ports)
            (decode-object (cons "array" "ContainerPort") value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "env" source)
    (when present-p
      (setf (slot-value object 'env)
            (decode-object (cons "array" "EnvVar") value))))
  (multiple-value-bind (value present-p)
      (gethash "readinessProbe" source)
    (when present-p
      (setf (slot-value object 'readiness-probe)
            (decode-object "Probe" value))))
  (multiple-value-bind (value present-p)
      (gethash "imagePullPolicy" source)
    (when present-p
      (setf (slot-value object 'image-pull-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "command" source)
    (when present-p
      (setf (slot-value object 'command)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "terminationMessagePath" source)
    (when present-p
      (setf (slot-value object 'termination-message-path)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "args" source)
    (when present-p
      (setf (slot-value object 'args)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "lifecycle" source)
    (when present-p
      (setf (slot-value object 'lifecycle)
            (decode-object "Lifecycle" value))))
  (multiple-value-bind (value present-p)
      (gethash "workingDir" source)
    (when present-p
      (setf (slot-value object 'working-dir)
            (decode-object "string" value)))))


(defclass persistent-volume-claim-condition
          (resource)
          ((message
            :initarg
            :message
            :type
            (or string null)
            :documentation
            "Human-readable message indicating details about last transition.")
           (last-probe-time
            :initarg
            :last-probe-time
            :type
            (or string null)
            :documentation
            "Last time we probed the condition.")
           (last-transition-time
            :initarg
            :last-transition-time
            :type
            (or string null)
            :documentation
            "Last time the condition transitioned from one status to another.")
           (type :initarg :type :type string :documentation nil)
           (status :initarg :status :type string :documentation nil)
           (reason
            :initarg
            :reason
            :type
            (or string null)
            :documentation
            "Unique, this should be a short, machine understandable string that gives the reason for condition's last transition. If it reports \"ResizeStarted\" that means the underlying persistent volume is being resized."))
          (:documentation
           "PersistentVolumeClaimCondition contails details about state of pvc"))

(defmethod unmarshal
  ((object persistent-volume-claim-condition) source)
  (multiple-value-bind (value present-p)
      (gethash "message" source)
    (when present-p
      (setf (slot-value object 'message)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastProbeTime" source)
    (when present-p
      (setf (slot-value object 'last-probe-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "lastTransitionTime" source)
    (when present-p
      (setf (slot-value object 'last-transition-time)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "reason" source)
    (when present-p
      (setf (slot-value object 'reason)
            (decode-object "string" value)))))


(defclass deployment-status
          (resource)
          ((unavailable-replicas
            :initarg
            :unavailable-replicas
            :type
            (or integer null)
            :documentation
            "Total number of unavailable pods targeted by this deployment. This is the total number of pods that are still required for the deployment to have 100% available capacity. They may either be pods that are running but not yet available or pods that still have not been created.")
           (available-replicas
            :initarg
            :available-replicas
            :type
            (or integer null)
            :documentation
            "Total number of available pods (ready for at least minReadySeconds) targeted by this deployment.")
           (conditions
            :initarg
            :conditions
            :type
            list
            :documentation
            "Represents the latest available observations of a deployment's current state.")
           (ready-replicas
            :initarg
            :ready-replicas
            :type
            (or integer null)
            :documentation
            "Total number of ready pods targeted by this deployment.")
           (observed-generation
            :initarg
            :observed-generation
            :type
            (or integer null)
            :documentation
            "The generation observed by the deployment controller.")
           (replicas
            :initarg
            :replicas
            :type
            (or integer null)
            :documentation
            "Total number of non-terminated pods targeted by this deployment (their labels match the selector).")
           (updated-replicas
            :initarg
            :updated-replicas
            :type
            (or integer null)
            :documentation
            "Total number of non-terminated pods targeted by this deployment that have the desired template spec.")
           (collision-count
            :initarg
            :collision-count
            :type
            (or integer null)
            :documentation
            "Count of hash collisions for the Deployment. The Deployment controller uses this field as a collision avoidance mechanism when it needs to create the name for the newest ReplicaSet."))
          (:documentation
           "DeploymentStatus is the most recently observed status of the Deployment."))

(defmethod unmarshal
  ((object deployment-status) source)
  (multiple-value-bind (value present-p)
      (gethash "unavailableReplicas" source)
    (when present-p
      (setf (slot-value object 'unavailable-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "availableReplicas" source)
    (when present-p
      (setf (slot-value object 'available-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "conditions" source)
    (when present-p
      (setf (slot-value object 'conditions)
            (decode-object
             (cons "array" "DeploymentCondition")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "readyReplicas" source)
    (when present-p
      (setf (slot-value object 'ready-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "observedGeneration" source)
    (when present-p
      (setf (slot-value object 'observed-generation)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "replicas" source)
    (when present-p
      (setf (slot-value object 'replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "updatedReplicas" source)
    (when present-p
      (setf (slot-value object 'updated-replicas)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "collisionCount" source)
    (when present-p
      (setf (slot-value object 'collision-count)
            (decode-object (cons "integer" "int32") value)))))


(defclass se-linux-options
          (resource)
          ((level
            :initarg
            :level
            :type
            (or string null)
            :documentation
            "Level is SELinux level label that applies to the container.")
           (type
            :initarg
            :type
            :type
            (or string null)
            :documentation
            "Type is a SELinux type label that applies to the container.")
           (role
            :initarg
            :role
            :type
            (or string null)
            :documentation
            "Role is a SELinux role label that applies to the container.")
           (user
            :initarg
            :user
            :type
            (or string null)
            :documentation
            "User is a SELinux user label that applies to the container."))
          (:documentation
           "SELinuxOptions are the labels to be applied to the container"))

(defmethod unmarshal
  ((object se-linux-options) source)
  (multiple-value-bind (value present-p)
      (gethash "level" source)
    (when present-p
      (setf (slot-value object 'level)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "type" source)
    (when present-p
      (setf (slot-value object 'type) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "role" source)
    (when present-p
      (setf (slot-value object 'role) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "user" source)
    (when present-p
      (setf (slot-value object 'user) (decode-object "string" value)))))


(defclass pod-spec
          (resource)
          ((host-ipc
            :initarg
            :host-ipc
            :type
            (or boolean null)
            :documentation
            "Use the host's ipc namespace. Optional: Default to false.")
           (host-network
            :initarg
            :host-network
            :type
            (or boolean null)
            :documentation
            "Host networking requested for this pod. Use the host's network namespace. If this option is set, the ports that will be used must be specified. Default to false.")
           (service-account
            :initarg
            :service-account
            :type
            (or string null)
            :documentation
            "DeprecatedServiceAccount is a depreciated alias for ServiceAccountName. Deprecated: Use serviceAccountName instead.")
           (active-deadline-seconds
            :initarg
            :active-deadline-seconds
            :type
            (or integer null)
            :documentation
            "Optional duration in seconds the pod may be active on the node relative to StartTime before the system will actively try to mark it failed and kill associated containers. Value must be a positive integer.")
           (termination-grace-period-seconds
            :initarg
            :termination-grace-period-seconds
            :type
            (or integer null)
            :documentation
            "Optional duration in seconds the pod needs to terminate gracefully. May be decreased in delete request. Value must be non-negative integer. The value zero indicates delete immediately. If this value is nil, the default grace period will be used instead. The grace period is the duration in seconds after the processes running in the pod are sent a termination signal and the time when the processes are forcibly halted with a kill signal. Set this value longer than the expected cleanup time for your process. Defaults to 30 seconds.")
           (tolerations
            :initarg
            :tolerations
            :type
            list
            :documentation
            "If specified, the pod's tolerations.")
           (node-selector
            :initarg
            :node-selector
            :type
            (or hash-table null)
            :documentation
            "NodeSelector is a selector which must be true for the pod to fit on a node. Selector which must match a node's labels for the pod to be scheduled on that node. More info: https://kubernetes.io/docs/concepts/configuration/assign-pod-node/")
           (priority
            :initarg
            :priority
            :type
            (or integer null)
            :documentation
            "The priority value. Various system components use this field to find the priority of the pod. When Priority Admission Controller is enabled, it prevents users from setting this field. The admission controller populates this field from PriorityClassName. The higher the value, the higher the priority.")
           (host-aliases
            :initarg
            :host-aliases
            :type
            list
            :documentation
            "HostAliases is an optional list of hosts and IPs that will be injected into the pod's hosts file if specified. This is only valid for non-hostNetwork pods.")
           (automount-service-account-token
            :initarg
            :automount-service-account-token
            :type
            (or boolean null)
            :documentation
            "AutomountServiceAccountToken indicates whether a service account token should be automatically mounted.")
           (affinity
            :initarg
            :affinity
            :type
            (or affinity null)
            :documentation
            "If specified, the pod's scheduling constraints")
           (hostname
            :initarg
            :hostname
            :type
            (or string null)
            :documentation
            "Specifies the hostname of the Pod If not specified, the pod's hostname will be set to a system-defined value.")
           (security-context
            :initarg
            :security-context
            :type
            (or pod-security-context null)
            :documentation
            "SecurityContext holds pod-level security attributes and common container settings. Optional: Defaults to empty.  See type description for default values of each field.")
           (service-account-name
            :initarg
            :service-account-name
            :type
            (or string null)
            :documentation
            "ServiceAccountName is the name of the ServiceAccount to use to run this pod. More info: https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/")
           (priority-class-name
            :initarg
            :priority-class-name
            :type
            (or string null)
            :documentation
            "If specified, indicates the pod's priority. \"system-node-critical\" and \"system-cluster-critical\" are two special keywords which indicate the highest priorities with the former being the highest priority. Any other name must be defined by creating a PriorityClass object with that name. If not specified, the pod priority will be default or zero if there is no default.")
           (scheduler-name
            :initarg
            :scheduler-name
            :type
            (or string null)
            :documentation
            "If specified, the pod will be dispatched by specified scheduler. If not specified, the pod will be dispatched by default scheduler.")
           (node-name
            :initarg
            :node-name
            :type
            (or string null)
            :documentation
            "NodeName is a request to schedule this pod onto a specific node. If it is non-empty, the scheduler simply schedules this pod onto that node, assuming that it fits resource requirements.")
           (subdomain
            :initarg
            :subdomain
            :type
            (or string null)
            :documentation
            "If specified, the fully qualified Pod hostname will be \"<hostname>.<subdomain>.<pod namespace>.svc.<cluster domain>\". If not specified, the pod will not have a domainname at all.")
           (restart-policy
            :initarg
            :restart-policy
            :type
            (or string null)
            :documentation
            "Restart policy for all containers within the pod. One of Always, OnFailure, Never. Default to Always. More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle/#restart-policy")
           (init-containers
            :initarg
            :init-containers
            :type
            list
            :documentation
            "List of initialization containers belonging to the pod. Init containers are executed in order prior to containers being started. If any init container fails, the pod is considered to have failed and is handled according to its restartPolicy. The name for an init container or normal container must be unique among all containers. Init containers may not have Lifecycle actions, Readiness probes, or Liveness probes. The resourceRequirements of an init container are taken into account during scheduling by finding the highest request/limit for each resource type, and then using the max of of that value or the sum of the normal containers. Limits are applied to init containers in a similar fashion. Init containers cannot currently be added or removed. Cannot be updated. More info: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/")
           (dns-config
            :initarg
            :dns-config
            :type
            (or pod-dns-config null)
            :documentation
            "Specifies the DNS parameters of a pod. Parameters specified here will be merged to the generated DNS configuration based on DNSPolicy.")
           (containers
            :initarg
            :containers
            :type
            list
            :documentation
            "List of containers belonging to the pod. Containers cannot currently be added or removed. There must be at least one container in a Pod. Cannot be updated.")
           (host-pid
            :initarg
            :host-pid
            :type
            (or boolean null)
            :documentation
            "Use the host's pid namespace. Optional: Default to false.")
           (dns-policy
            :initarg
            :dns-policy
            :type
            (or string null)
            :documentation
            "Set DNS policy for the pod. Defaults to \"ClusterFirst\". Valid values are 'ClusterFirstWithHostNet', 'ClusterFirst', 'Default' or 'None'. DNS parameters given in DNSConfig will be merged with the policy selected with DNSPolicy. To have DNS options set along with hostNetwork, you have to specify DNS policy explicitly to 'ClusterFirstWithHostNet'.")
           (volumes
            :initarg
            :volumes
            :type
            list
            :documentation
            "List of volumes that can be mounted by containers belonging to the pod. More info: https://kubernetes.io/docs/concepts/storage/volumes")
           (image-pull-secrets
            :initarg
            :image-pull-secrets
            :type
            list
            :documentation
            "ImagePullSecrets is an optional list of references to secrets in the same namespace to use for pulling any of the images used by this PodSpec. If specified, these secrets will be passed to individual puller implementations for them to use. For example, in the case of docker, only DockerConfig type secrets are honored. More info: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod")
           (share-process-namespace
            :initarg
            :share-process-namespace
            :type
            (or boolean null)
            :documentation
            "Share a single process namespace between all of the containers in a pod. When this is set containers will be able to view and signal processes from other containers in the same pod, and the first process in each container will not be assigned PID 1. HostPID and ShareProcessNamespace cannot both be set. Optional: Default to false. This field is alpha-level and is honored only by servers that enable the PodShareProcessNamespace feature."))
          (:documentation "PodSpec is a description of a pod."))

(defmethod unmarshal
  ((object pod-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "hostIPC" source)
    (when present-p
      (setf (slot-value object 'host-ipc)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "hostNetwork" source)
    (when present-p
      (setf (slot-value object 'host-network)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "serviceAccount" source)
    (when present-p
      (setf (slot-value object 'service-account)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "activeDeadlineSeconds" source)
    (when present-p
      (setf (slot-value object 'active-deadline-seconds)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "terminationGracePeriodSeconds" source)
    (when present-p
      (setf (slot-value object 'termination-grace-period-seconds)
            (decode-object (cons "integer" "int64") value))))
  (multiple-value-bind (value present-p)
      (gethash "tolerations" source)
    (when present-p
      (setf (slot-value object 'tolerations)
            (decode-object (cons "array" "Toleration") value))))
  (multiple-value-bind (value present-p)
      (gethash "nodeSelector" source)
    (when present-p
      (setf (slot-value object 'node-selector)
            (decode-object "object" value))))
  (multiple-value-bind (value present-p)
      (gethash "priority" source)
    (when present-p
      (setf (slot-value object 'priority)
            (decode-object (cons "integer" "int32") value))))
  (multiple-value-bind (value present-p)
      (gethash "hostAliases" source)
    (when present-p
      (setf (slot-value object 'host-aliases)
            (decode-object (cons "array" "HostAlias") value))))
  (multiple-value-bind (value present-p)
      (gethash "automountServiceAccountToken" source)
    (when present-p
      (setf (slot-value object 'automount-service-account-token)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "affinity" source)
    (when present-p
      (setf (slot-value object 'affinity)
            (decode-object "Affinity" value))))
  (multiple-value-bind (value present-p)
      (gethash "hostname" source)
    (when present-p
      (setf (slot-value object 'hostname)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "securityContext" source)
    (when present-p
      (setf (slot-value object 'security-context)
            (decode-object "PodSecurityContext" value))))
  (multiple-value-bind (value present-p)
      (gethash "serviceAccountName" source)
    (when present-p
      (setf (slot-value object 'service-account-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "priorityClassName" source)
    (when present-p
      (setf (slot-value object 'priority-class-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "schedulerName" source)
    (when present-p
      (setf (slot-value object 'scheduler-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "nodeName" source)
    (when present-p
      (setf (slot-value object 'node-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "subdomain" source)
    (when present-p
      (setf (slot-value object 'subdomain)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "restartPolicy" source)
    (when present-p
      (setf (slot-value object 'restart-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "initContainers" source)
    (when present-p
      (setf (slot-value object 'init-containers)
            (decode-object (cons "array" "Container") value))))
  (multiple-value-bind (value present-p)
      (gethash "dnsConfig" source)
    (when present-p
      (setf (slot-value object 'dns-config)
            (decode-object "PodDNSConfig" value))))
  (multiple-value-bind (value present-p)
      (gethash "containers" source)
    (when present-p
      (setf (slot-value object 'containers)
            (decode-object (cons "array" "Container") value))))
  (multiple-value-bind (value present-p)
      (gethash "hostPID" source)
    (when present-p
      (setf (slot-value object 'host-pid)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "dnsPolicy" source)
    (when present-p
      (setf (slot-value object 'dns-policy)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumes" source)
    (when present-p
      (setf (slot-value object 'volumes)
            (decode-object (cons "array" "Volume") value))))
  (multiple-value-bind (value present-p)
      (gethash "imagePullSecrets" source)
    (when present-p
      (setf (slot-value object 'image-pull-secrets)
            (decode-object
             (cons "array" "LocalObjectReference")
             value))))
  (multiple-value-bind (value present-p)
      (gethash "shareProcessNamespace" source)
    (when present-p
      (setf (slot-value object 'share-process-namespace)
            (decode-object "boolean" value)))))


(defclass persistent-volume-claim-spec
          (resource)
          ((resources
            :initarg
            :resources
            :type
            (or resource-requirements null)
            :documentation
            "Resources represents the minimum resources the volume should have. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#resources")
           (storage-class-name
            :initarg
            :storage-class-name
            :type
            (or string null)
            :documentation
            "Name of the StorageClass required by the claim. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#class-1")
           (volume-mode
            :initarg
            :volume-mode
            :type
            (or persistent-volume-mode null)
            :documentation
            "volumeMode defines what type of volume is required by the claim. Value of Filesystem is implied when not included in claim spec. This is an alpha feature and may change in the future.")
           (selector
            :initarg
            :selector
            :type
            (or label-selector null)
            :documentation
            "A label query over volumes to consider for binding.")
           (volume-name
            :initarg
            :volume-name
            :type
            (or string null)
            :documentation
            "VolumeName is the binding reference to the PersistentVolume backing this claim.")
           (access-modes
            :initarg
            :access-modes
            :type
            list
            :documentation
            "AccessModes contains the desired access modes the volume should have. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#access-modes-1"))
          (:documentation
           "PersistentVolumeClaimSpec describes the common attributes of storage devices and allows a Source for provider-specific attributes"))

(defmethod unmarshal
  ((object persistent-volume-claim-spec) source)
  (multiple-value-bind (value present-p)
      (gethash "resources" source)
    (when present-p
      (setf (slot-value object 'resources)
            (decode-object "ResourceRequirements" value))))
  (multiple-value-bind (value present-p)
      (gethash "storageClassName" source)
    (when present-p
      (setf (slot-value object 'storage-class-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeMode" source)
    (when present-p
      (setf (slot-value object 'volume-mode)
            (decode-object "PersistentVolumeMode" value))))
  (multiple-value-bind (value present-p)
      (gethash "selector" source)
    (when present-p
      (setf (slot-value object 'selector)
            (decode-object "LabelSelector" value))))
  (multiple-value-bind (value present-p)
      (gethash "volumeName" source)
    (when present-p
      (setf (slot-value object 'volume-name)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "accessModes" source)
    (when present-p
      (setf (slot-value object 'access-modes)
            (decode-object
             (cons "array" "PersistentVolumeAccessMode")
             value)))))


(defclass weighted-pod-affinity-term
          (resource)
          ((pod-affinity-term
            :initarg
            :pod-affinity-term
            :type
            pod-affinity-term
            :documentation
            "Required. A pod affinity term, associated with the corresponding weight.")
           (weight
            :initarg
            :weight
            :type
            integer
            :documentation
            "weight associated with matching the corresponding podAffinityTerm, in the range 1-100."))
          (:documentation
           "The weights of all of the matched WeightedPodAffinityTerm fields are added per-node to find the most preferred node(s)"))

(defmethod unmarshal
  ((object weighted-pod-affinity-term) source)
  (multiple-value-bind (value present-p)
      (gethash "podAffinityTerm" source)
    (when present-p
      (setf (slot-value object 'pod-affinity-term)
            (decode-object "PodAffinityTerm" value))))
  (multiple-value-bind (value present-p)
      (gethash "weight" source)
    (when present-p
      (setf (slot-value object 'weight)
            (decode-object (cons "integer" "int32") value)))))


(defclass persistent-volume-claim-volume-source
          (resource)
          ((read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "Will force the ReadOnly setting in VolumeMounts. Default false.")
           (claim-name
            :initarg
            :claim-name
            :type
            string
            :documentation
            "ClaimName is the name of a PersistentVolumeClaim in the same namespace as the pod using this volume. More info: https://kubernetes.io/docs/concepts/storage/persistent-volumes#persistentvolumeclaims"))
          (:documentation
           "PersistentVolumeClaimVolumeSource references the user's PVC in the same namespace. This volume finds the bound PV and mounts that volume for the pod. A PersistentVolumeClaimVolumeSource is, essentially, a wrapper around another type of volume that is owned by someone else (the system)."))

(defmethod unmarshal
  ((object persistent-volume-claim-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "claimName" source)
    (when present-p
      (setf (slot-value object 'claim-name)
            (decode-object "string" value)))))


(defclass stateful-set
          (resource)
          ((api-version :initform "apps/v1" :allocation :class)
           (kind :initform "StatefulSet" :allocation :class)
           (status
            :initarg
            :status
            :type
            (or stateful-set-status null)
            :documentation
            "Status is the current status of Pods in this StatefulSet. This data may be out of date by some window of time.")
           (spec
            :initarg
            :spec
            :type
            (or stateful-set-spec null)
            :documentation
            "Spec defines the desired identities of pods in this set.")
           (metadata
            :initarg
            :metadata
            :type
            (or object-meta null)
            :documentation
            nil))
          (:documentation
           "StatefulSet represents a set of pods with consistent identities. Identities are defined as:
 - Network: A single stable DNS and hostname.
 - Storage: As many VolumeClaims as requested.
The StatefulSet guarantees that a given network identity will always map to the same storage identity."))

(defmethod unmarshal
  ((object stateful-set) source)
  (multiple-value-bind (value present-p)
      (gethash "kind" source)
    (when present-p
      (setf (slot-value object 'kind) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "status" source)
    (when present-p
      (setf (slot-value object 'status)
            (decode-object "StatefulSetStatus" value))))
  (multiple-value-bind (value present-p)
      (gethash "spec" source)
    (when present-p
      (setf (slot-value object 'spec)
            (decode-object "StatefulSetSpec" value))))
  (multiple-value-bind (value present-p)
      (gethash "apiVersion" source)
    (when present-p
      (setf (slot-value object 'api-version)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "metadata" source)
    (when present-p
      (setf (slot-value object 'metadata)
            (decode-object "ObjectMeta" value)))))


(defclass config-map-projection
          (resource)
          ((optional
            :initarg
            :optional
            :type
            (or boolean null)
            :documentation
            "Specify whether the ConfigMap or it's keys must be defined")
           (name
            :initarg
            :name
            :type
            (or string null)
            :documentation
            "Name of the referent. More info: https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#names")
           (items
            :initarg
            :items
            :type
            list
            :documentation
            "If unspecified, each key-value pair in the Data field of the referenced ConfigMap will be projected into the volume as a file whose name is the key and content is the value. If specified, the listed keys will be projected into the specified paths, and unlisted keys will not be present. If a key is specified which is not present in the ConfigMap, the volume setup will error unless it is marked optional. Paths must be relative and may not contain the '..' path or start with '..'."))
          (:documentation
           "Adapts a ConfigMap into a projected volume.

The contents of the target ConfigMap's Data field will be presented in a projected volume as files using the keys in the Data field as the file names, unless the items element is populated with specific mappings of keys to paths. Note that this is identical to a configmap volume source without the default mode."))

(defmethod unmarshal
  ((object config-map-projection) source)
  (multiple-value-bind (value present-p)
      (gethash "optional" source)
    (when present-p
      (setf (slot-value object 'optional)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "name" source)
    (when present-p
      (setf (slot-value object 'name) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "items" source)
    (when present-p
      (setf (slot-value object 'items)
            (decode-object (cons "array" "KeyToPath") value)))))


(defclass tcp-socket-action
          (resource)
          ((host
            :initarg
            :host
            :type
            (or string null)
            :documentation
            "Optional: Host name to connect to, defaults to the pod IP.")
           (port
            :initarg
            :port
            :type
            string
            :documentation
            "Number or name of the port to access on the container. Number must be in the range 1 to 65535. Name must be an IANA_SVC_NAME."))
          (:documentation
           "TCPSocketAction describes an action based on opening a socket"))

(defmethod unmarshal
  ((object tcp-socket-action) source)
  (multiple-value-bind (value present-p)
      (gethash "host" source)
    (when present-p
      (setf (slot-value object 'host) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "port" source)
    (when present-p
      (setf (slot-value object 'port) (decode-object "string" value)))))


(defclass rbd-volume-source
          (resource)
          ((secret-ref
            :initarg
            :secret-ref
            :type
            (or local-object-reference null)
            :documentation
            "SecretRef is name of the authentication secret for RBDUser. If provided overrides keyring. Default is nil. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (monitors
            :initarg
            :monitors
            :type
            list
            :documentation
            "A collection of Ceph monitors. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (read-only
            :initarg
            :read-only
            :type
            (or boolean null)
            :documentation
            "ReadOnly here will force the ReadOnly setting in VolumeMounts. Defaults to false. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (keyring
            :initarg
            :keyring
            :type
            (or string null)
            :documentation
            "Keyring is the path to key ring for RBDUser. Default is /etc/ceph/keyring. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (image
            :initarg
            :image
            :type
            string
            :documentation
            "The rados image name. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (pool
            :initarg
            :pool
            :type
            (or string null)
            :documentation
            "The rados pool name. Default is rbd. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (user
            :initarg
            :user
            :type
            (or string null)
            :documentation
            "The rados user name. Default is admin. More info: https://releases.k8s.io/HEAD/examples/volumes/rbd/README.md#how-to-use-it")
           (fs-type
            :initarg
            :fs-type
            :type
            (or string null)
            :documentation
            "Filesystem type of the volume that you want to mount. Tip: Ensure that the filesystem type is supported by the host operating system. Examples: \"ext4\", \"xfs\", \"ntfs\". Implicitly inferred to be \"ext4\" if unspecified. More info: https://kubernetes.io/docs/concepts/storage/volumes#rbd"))
          (:documentation
           "Represents a Rados Block Device mount that lasts the lifetime of a pod. RBD volumes support ownership management and SELinux relabeling."))

(defmethod unmarshal
  ((object rbd-volume-source) source)
  (multiple-value-bind (value present-p)
      (gethash "secretRef" source)
    (when present-p
      (setf (slot-value object 'secret-ref)
            (decode-object "LocalObjectReference" value))))
  (multiple-value-bind (value present-p)
      (gethash "monitors" source)
    (when present-p
      (setf (slot-value object 'monitors)
            (decode-object (cons "array" "string") value))))
  (multiple-value-bind (value present-p)
      (gethash "readOnly" source)
    (when present-p
      (setf (slot-value object 'read-only)
            (decode-object "boolean" value))))
  (multiple-value-bind (value present-p)
      (gethash "keyring" source)
    (when present-p
      (setf (slot-value object 'keyring)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "image" source)
    (when present-p
      (setf (slot-value object 'image)
            (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "pool" source)
    (when present-p
      (setf (slot-value object 'pool) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "user" source)
    (when present-p
      (setf (slot-value object 'user) (decode-object "string" value))))
  (multiple-value-bind (value present-p)
      (gethash "fsType" source)
    (when present-p
      (setf (slot-value object 'fs-type)
            (decode-object "string" value)))))

