(in-package :mu-cl-resources)

(defparameter *supply-cache-headers-p* t
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
(setf *cache-model-properties-p* t)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")
(defparameter *cache-count-queries* t)
(defparameter *cache-model-properties* t)
(setf sparql:*experimental-no-application-graph-for-sudo-select-queries* t)
(defparameter *max-group-sorted-properties* nil)

(read-domain-file "products.json")
(read-domain-file "users.json")
