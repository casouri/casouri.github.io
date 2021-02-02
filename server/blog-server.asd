(asdf:defsystem blog-server
  :name "blog-server"
  :description "My blog server."
  :depends-on (:hunchentoot :sqlite :cl-fad)
  :components ((:file "blog-server")))
