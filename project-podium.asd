(defsystem project-podium
  :author "Alexander R Cavaliere <alex.cavaliere@sharpnotions.com>"
  :maintainer "Alexander R Cavaliere <alex.cavaliere@sharpnotions.com>"
  :license "GPLv3"
  :version "0.1"
  :homepage "https://github.com/arcavaliere/project-podium"
  :bug-tracker "https://github.com/arcavaliere/project-podium/issues"
  :source-control (:git "git@github.com:arcavaliere/project-podium.git")
  :depends-on (:lucerne
               :lucerne-auth
               :dexador
               :quri
               :clack
               :cl-json
               :cl-pass
               :clsql
               :mito)
  :defsystem-depends-on (:asdf-linguist)
  :components ((:module "assets"
                :components
                ((:module "css"
                  :components
                  ((:sass "style")))
                 (:module "js"
                  :components
                  ((:static-file "scripts.js")))))
               (:module "src"
                :serial t
                :components
                (
                 (:file "models")
                 (:file "project-podium")
                 )))
  :description "Project proposal and tracking platform"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op project-podium-test))))
