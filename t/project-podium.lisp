(in-package :cl-user)
(defpackage project-podium-test
  (:use :cl :fiveam)
  (:import-from :project-podium.models))
(in-package :project-podium-test)

(def-suite tests
  :description "project-podium tests.")
(in-suite tests)

(test simple-test
  (is
   (equal 1 1))
  (is-true
   (and t t)))


run! 'tests)
