(in-package :cl-user)
(defpackage project-podium-test
  (:use :cl :fiveam :clsql))
(in-package :project-podium-test)

(def-suite tests
  :description "project-podium tests.")
(in-suite tests)

(test simple-test
  (is
   (equal 1 1))
  (is-true
   (and t t)))

(test database-creation
  (project-podium-models:connect-to-database "test.db")
  (is
   (not (equal clsql:*default-database* nil)))
  (clsql:disconnect))

(run! 'tests)
