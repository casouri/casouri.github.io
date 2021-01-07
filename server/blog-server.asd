(asdf:defsystem blog-server
  :name "blog-server"
  :description "My blog server."
  :depends-on (:hunchentoot :sqlite)
  :components ((:file "blog-server")))
