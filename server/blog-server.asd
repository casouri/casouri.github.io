(asdf:defsystem yuan.blog-server
  :name "yuan.blog-server"
  :description "My blog server."
  :depends-on (:hunchentoot :sqlite)
  :components ((:file "blog-server")))
