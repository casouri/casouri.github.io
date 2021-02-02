(asdf:defsystem blog-server
  :name "blog-server"
  :description "My blog server."
  :depends-on (:hunchentoot :sqlite :fad)
  :components ((:file "blog-server")))
