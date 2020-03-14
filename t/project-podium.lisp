(in-package :cl-user)
(defpackage project-podium-test
  (:use :cl :fiveam :clsql))
(in-package :project-podium-test)

(defun create-user (username full-name email password)
  (project-podium.models:register-user :username username
                                       :full-name full-name
                                       :email email
                                       :password password))

(def-suite tests
  :description "project-podium tests.")
(in-suite tests)

(test simple-test
  (is
   (equal 1 1))
  (is-true
   (and t t)))

(test database
  (project-podium.models:connect-to-database (concatenate 'string "test" (write-to-string (get-universal-time)) ".db"))
  (is
   (not (equal clsql:*default-database* nil)))
  ;; Make some test objects in the database
  (let* ((test-user (create-user (concatenate 'string "test-user-" (write-to-string (get-universal-time)) ) "Test User" "test@test.com" "Test1234"))
         (test-project (project-podium.models:create-project :name "Test Project"
                                                             :leader test-user
                                                             :summary "Test Project"
                                                             :status "In Progress"
                                                             :members (list test-user))))
    ;; Can we retrieve a user?
    (is (equal (project-podium.models:user-id (car (project-podium.models:find-user (project-podium.models:user-id test-user))))
               (project-podium.models:user-id test-user))))
  (clsql:disconnect))

(run! 'tests)
