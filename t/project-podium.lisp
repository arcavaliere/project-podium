(in-package :cl-user)
(defpackage project-podium-test
  (:use :cl :fiveam :mito))
(in-package :project-podium-test)

(def-suite tests
  :description "project-podium tests.")
(in-suite tests)

(defun compare-users (user-1 user-2)
  (and (equal (project-podium.models:user-username user-1) (project-podium.models:user-username user-2))
       (equal (project-podium.models:user-email user-1) (project-podium.models:user-email user-2))
       (equal (project-podium.models:user-full-name user-1) (project-podium.models:user-full-name user-2))
       (equal (project-podium.models:user-password user-1) (project-podium.models:user-password user-2))))

(defun compare-legs (leg-1 leg-2)
  (and (equal (project-podium.models:project-leg-status leg-1) (project-podium.models:project-leg-status leg-2))
       (equal (project-podium.models:project-leg-due-date leg-1) (project-podium.models:project-leg-due-date leg-2))
       (equal (project-podium.models:project-leg-deliverable leg-1) (project-podium.models:project-leg-deliverable leg-2))
       (compare-project-leg-members leg-1 leg-2)))

(defun compare-projects (project-1 project-2)
  (and (compare-users (project-podium.models:project-leader project-1) (project-podium.models:project-leader project-2))
       (equal (project-podium.models:project-name project-1) (project-podium.models:project-name project-2))
       (equal (project-podium.models:project-summary project-1) (project-podium.models:project-summary project-2))
       (equal (project-podium.models:project-status project-1) (project-podium.models:project-status project-2))
       (compare-project-members project-1 project-2)))

(defun compare-project-members (project-1 project-2)
  (let ((members-1 (mapcar #'project-podium.models:user
                           (project-podium.models:get-project-members project-1)))
        (members-2 (mapcar #'project-podium.models:user
                           (project-podium.models:get-project-members project-2))))
    (if (> (count nil (mapcar #'compare-users members-1 members-2)) 0)
        nil
        t)))

(defun compare-project-leg-members (leg-1 leg-2)
  (let ((members-1 (mapcar #'project-podium.models:user
                           (project-podium.models:get-project-leg-members leg-1)))
        (members-2 (mapcar #'project-podium.models:user
                           (project-podium.models:get-project-leg-members leg-2))))
    (if (> (count nil (mapcar #'compare-users members-1 members-2)) 0)
        nil
        t)))

(test database
  (project-podium.models:connect-to-database-local "project_podium_test" "podium" "podium")
  (project-podium.models:initialize-database)
  (let* ((test-user (project-podium.models:create-user :username "testuser"
                                                      :full-name "Test User"
                                                      :email "test@test.com"
                                                       :password "Test1234"))
         (test-user-1 (project-podium.models:create-user :username "testuser1"
                                                         :full-name "Test User 1"
                                                         :email "test1@test.com"
                                                         :password "Test1234"))
        (test-project (project-podium.models:create-project :name "Test Project"
                                                            :leader test-user
                                                            :summary "This is a test project"
                                                            :status "In Progress"
                                                            :members (list test-user test-user-1)))
        (test-leg (project-podium.models:create-project-leg :project test-project
                                                            :leader test-user
                                                            :members (list test-user test-user-1)
                                                            :deliverable "Testing!"
                                                            :due-date "1/1/1970"
                                                            :status "In Progress")))
    (is-true (compare-users (project-podium.models:get-user (mito:object-id test-user)) test-user))
    (is-true (compare-projects (project-podium.models:get-projects :leader test-user) test-project))
    (is-true (compare-projects (project-podium.models:get-projects :project-id (mito:object-id test-project)) test-project))
    (is-true (compare-legs (project-podium.models:get-project-legs :leader test-user) test-leg))
    (is-true (compare-legs (project-podium.models:get-project-legs :project-leg-id (mito:object-id test-leg)) test-leg))
    (project-podium.models:change-project (mito:object-id test-project) :remove-leg t :leg test-leg)
    (mito:delete-dao test-project)
    (mito:delete-dao test-user)))

(run! 'tests)
