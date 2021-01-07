
(in-package :blog-server)

;; Hunchentoot manual:
;; https://edicl.github.io/hunchentoot/#dispatch-easy-handlers

(defclass server (acceptor)
  ((dispatch-table
    :initform '()
    :initarg :dispatch-table
    :accessor server-dispatch-table
    :documentation "List of dispatch functions")
   (db
    :initform (sqlite:connect ":memory:")
    :initarg :db
    :accessor server-db
    :documentation "Database used by this server.
Default value is the default in-memory sqlite database."))
  (:default-initargs))

(defmethod acceptor-dispatch-request
    ((server server) request)
  ;; Try REQUEST on each dispatcher in turn.
  (mapc (lambda (dispatcher)
	  (let ((handler (funcall dispatcher request)))
	    (when handler ; Handler found. Call it and return the result.
	      (return-from acceptor-dispatch-request
                (funcall handler)))))
	(server-dispatch-table server))
  (call-next-method))

(defmethod acceptor-status-message
    ((server server) (http-status-code (eql +http-not-found+)) &rest _)
  ;; Handle 404.
  (handle-static-file
   (merge-pathnames "404.html" (acceptor-document-root server))))

(defmethod acceptor-log-access :after
    ((server server) &key return-code)
  ;; Log access to the database.
  ;; NOTE: It should be unlikely that ".html" appears in the middle of
  ;; some URI...
  (when (and (or (search ".html" (script-name*))
                 (eq (script-name*) "/"))
             ;; Wayback Machine plugin makes another request. We only
             ;; want normal requests. This is technically necessary
             ;; but doesn’t harm to have.
             (search "Mozilla" (user-agent)))
    (let ((time (iso-time))
          (uri (request-uri*))
          (ip (real-remote-addr))
          (db (server-db server)))
      (if (execute-single
           db
           "select * from view where ip=? and uri=?" ip uri)
          ;; Update.
          (execute-non-query
           db
           "update view set time=?, code=? where ip=? and uri=?"
           time return-code ip uri)
          ;; Insert.
          (execute-non-query
           db
           "insert into view (ip, code, time, uri) values (?, ?, ?, ?)"
           ip return-code time uri)))))



(defun record-like ()
  (let ((ip (real-remote-addr))
        (time (iso-time))
        (uri (post-parameter "path"))
        (db (server-db (request-acceptor *request*))))
    (when uri
      (if (execute-single
           db
           "select * from like where ip=? and uri=?" ip uri)
          ;; Update.
          (execute-non-query
           db
           "update like set time=? where ip=? and uri=?"
           time ip uri)
          ;; Insert
          (execute-non-query
           db
           "insert into like (ip, time, uri) values (?, ?, ?)"
           ip time uri))))
  (handle-static-file #p"reply-like.html"))

;; Database semantic:
;;
;; create table view (
;; ip char(16),
;; code integer,
;; time text,
;; uri text,
;; primary key (ip, uri)
;; );

;; create table like (
;; ip char(16),
;; time text,
;; uri text,
;; primary key (ip, uri)
;; );

(defvar *server* (make-instance
                  'server
                  :port 4386
                  :document-root "../"
                  :access-log-destination "./access.log"
                  :message-log-destination "./message.log"
                  :db (sqlite:connect "./database.sqlite3")))

(push (create-prefix-dispatcher "/like" #'record-like)
      (server-dispatch-table *server*))

(start *server*)
