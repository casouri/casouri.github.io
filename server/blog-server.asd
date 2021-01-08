(asdf:defsystem blog-server
  :name "blog-server"
  :description "My blog server."
  :depends-on (:hunchentoot :sqlite :quri)
  :components ((:file "blog-server")))
