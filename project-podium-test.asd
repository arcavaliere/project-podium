(defsystem project-podium-test
  :author "Alexander R Cavaliere <alex.cavaliere@sharpnotions.com>"
  :license "GPLv3"
  :description "Tests for project-podium."
  :depends-on (:project-podium
               :fiveam
               :clsql)
  :components ((:module "t"
                :serial t
                :components
                ((:file "project-podium")))))
