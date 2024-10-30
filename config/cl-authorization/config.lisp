;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

;; (push (make-instance 'delta-logging-handler) *delta-handlers*)
(add-delta-messenger "http://delta-notifier/")
(setf *log-delta-messenger-message-bus-processing* nil)

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* t) ; change nil to t for logging requests to virtuoso (and the response)
(setf *backend* "http://triplestore:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* t) ; change nil to t for logging all incoming requests

;;;;;;;;;;;;;;;;
;;; prefix types
(in-package :type-cache)

;;;;;;;;;;;;;;;;;
;;; access rights

(in-package :acl)

;; these three are precautions and may be removed in the future
(defparameter *access-specifications* nil)
(defparameter *graphs* nil)
(defparameter *rights* nil)

;; Prefixes used in the constraints below (not in the SPARQL queries)
(define-prefixes
  ;; Core
  :mu "http://mu.semte.ch/vocabularies/core/"
  :session "http://mu.semte.ch/vocabularies/session/"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  ;; extra
  :gr "http://purl.org/goodrelations/v1#"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :dct "http://purl.org/dc/terms/"
  :nfo "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
  :nie "http://www.semanticdesktop.org/ontologies/2007/01/19/nie#"
  :dbpedia "http://dbpedia.org/resource/"
  :schema "http://schema.org/"
  :locn "http://www.w3.org/ns/locn#"
  :regorg "http://www.w3.org/ns/regorg#"
  :foaf "http://xmlns.com/foaf/0.1/"
  :price "http://data.rollvolet.be/vocabularies/pricing/"
  :stock "http://data.rollvolet.be/vocabularies/stock-management/")


(supply-allowed-group "public")

(supply-allowed-group "authenticated-access"
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT ?account WHERE {
              <SESSION_ID> session:account ?account .
          } LIMIT 1")

(define-graph public ("http://mu.semte.ch/graphs/public"))

(grant (read)
       :to-graph public
       :for-allowed-group "public")

(define-graph rollvolet-write ("http://mu.semte.ch/graphs/rollvolet")
  ("gr:BusinessEntity" -> _)
  ("gr:BusinessEntityType" -> _)
  ("gr:Offering" -> _)
  ("gr:SomeItems" -> _)
  ("gr:UnitPriceSpecification" -> _)
  ("schema:ContactPoint" -> _)
  ("locn:Address" -> _)
  ("stock:WarehouseLocation" -> _)
  ("stock:WarehouseDepartment" -> _)
  ("nfo:FileDataObject" -> _)
  ("ext:UnitCode" -> _)
  ("ext:ProductCategory" -> _)
  ("ext:OrganizationType" -> _)
  ("schema:Country" -> _)
  ("schema:Language" -> _)
)

(grant (read write)
       :to-graph rollvolet-write
       :for-allowed-group "authenticated-access")  


(define-graph users ("http://mu.semte.ch/graphs/users")
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" -> _)
)

(type-cache::add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session")
(define-graph sessions ("http://mu.semte.ch/graphs/sessions")
  ("session:Session" -> _) ; uris with prefix http://mu.semte.ch/sessions/
  )

; note: read ignores defined predicates and always gives access to full graph
(grant (read)
       :to-graph (users sessions)
       :for-allowed-group "authenticated-access")
