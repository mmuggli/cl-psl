;;;; cl-psl.asd

(asdf:defsystem #:cl-psl
  :serial t
  :description "Describe cl-psl here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "cl-psl")))

