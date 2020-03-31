(in-package :cl-user)
(defpackage project-podium.models
  (:use :cl :mito)
  (:export
   :user
   :id
   :user-username
   :user-full-name
   :user-email
   :user-password)
  (:export
   :project
   :id
   :project-name
   :project-leader
   :project-summary
   :project-status)
  (:export
   :project-leg
   :id
   :project-leg-project
   :project-leg-leader
   :project-leg-deliverable
   :project-leg-due-date
   :project-leg-status)
  (:export
   :user-project
   :user
   :project)
  (:export
   :user-leg
   :user
   :project-leg)
  (:export
   :get-user
   :get-users
   :create-user
   :get-projects
   :get-project-legs
   :create-project
   :create-project-leg
   :change-project
   :change-project-leg
   :connect-to-database-local
   :disconnect-from-database
   :initialize-database
   :get-project-members
   :get-project-leg-members
   :get-primary-key-value
   :get-user-projects
   :get-user-project-legs))
(in-package :project-podium.models)

;; Models
(defclass user ()
  ((username :accessor user-username
             :initarg :username
             :col-type (:varchar 32))
   (full-name :accessor user-full-name
              :initarg :full-name
              :col-type (:varchar 64))
   (email :accessor user-email
          :initarg :email
          :col-type (or (:varchar 64) :null))
   (password :accessor user-password
             :initarg :password
             :col-type (:varchar 256)))
  (:metaclass mito:dao-table-class))


(defclass project ()
  ((name :accessor project-name
         :initarg :name
         :col-type (:varchar 64))
   (leader :accessor project-leader
           :col-type user
           :initarg :leader)
   (summary :accessor project-summary
            :initarg :summary
            :col-type (:varchar 256))
   (status :accessor project-status
           :initarg :status
           :col-type (:varchar 48)))
  (documentation "A project")
  (:metaclass mito:dao-table-class))

(defclass user-project ()
  ((user :accessor user
         :initarg :user
         :col-type user)
   (project :accessor project
            :initarg :project
            :col-type project))
   (documentation "Join Table for Many to Many relationship between Users and Projects")
   (:metaclass mito:dao-table-class))

(defclass project-leg ()
  ((project :accessor project-leg-project
            :initarg :project
            :col-type project)
   (leader :accessor project-leg-leader
           :initarg :leader
           :col-type user)
   (deliverable :accessor project-leg-deliverable
                :initarg :deliverable
                :col-type (:varchar 256))
   (due-date :accessor project-leg-due-date
             :initarg :due-date
             :col-type (:varchar 16))
   (status :accessor project-leg-status
           :initarg :status
           :col-type (:varchar 48)))
  (documentation "A project leg")
  (:metaclass mito:dao-table-class))

(defclass user-leg ()
  ((project-leg :accessor project-leg
                :col-type project-leg
                :initarg :project-leg)
   (user :accessor user
         :col-type user
         :initarg :user))
   (documentation "Join Table for Many To Many Relationship between Users and Legs")
   (:metaclass mito:dao-table-class))

;; Storage

(defun connect-to-database-local (db-name username password)
  (mito:connect-toplevel :postgres :database-name db-name :username username :password password))

(defun disconnect-from-database () (mito:disconnect-toplevel))

(defun initialize-database ()
  (mapcar #'mito:ensure-table-exists (list 'user 'project 'project-leg 'user-project 'user-leg)))

;; Helpers

(defun get-primary-key-value (database-object)
  (mito:object-id database-object))

(defun get-users ()
  (mito:select-dao 'user))

(defun get-user (id)
  (mito:find-dao 'user :id id))

(defun create-user (&key username full-name email password)
  (let ((user (make-instance 'user
                              :username username
                              :full-name full-name
                              :email email
                              :password (cl-pass:hash password))))
     (mito:insert-dao user)
     user))

(defun get-projects (&key project-id leader)
    (cond ((not (eq project-id nil)) (mito:find-dao 'project :id project-id))
          ((not (eq leader nil)) (mito:find-dao 'project :leader leader))
          (t (mito:select-dao 'project))))

(defun get-project-legs (&key project project-leg-id leader)
    (cond ((not (eq project nil)) (mito:find-dao 'project-leg :project project))
          ((not (eq project-leg-id nil)) (mito:find-dao 'project-leg :id project-leg-id))
          ((not (eq leader nil)) (mito:find-dao 'project-leg :leader leader))
          (t (mito:select-dao 'project-leg))))

(defun get-project-members (project)
  (mito:select-dao 'user-project  (sxql:where (:= `project_id (mito:object-id project)))))

(defun get-project-leg-members (leg)
  (mito:select-dao 'user-leg (sxql:where (:= `project_leg_id (mito:object-id leg)))))

(defun get-user-projects (user)
  (let ((mappings (mito:select-dao 'user-project (sxql:where (:= 'user_id (mito:object-id user))))))
    (mapcar (lambda (mapping) (get-projects :project-id (mito:object-id (project mapping)))) mappings)))

(defun get-user-project-legs (user)
  (let ((mappings (mito:select-dao 'user-leg (sxql:where (:= 'user_id (mito:object-id user))))))
    (mapcar (lambda (mapping) (get-project-legs :project-leg-id (mito:object-id (project-leg mapping)))) mappings)))

(defun create-project (&key name leader summary status members)
  (let* ((project (make-instance 'project
                                :name name
                                :leader leader
                                :summary summary
                                :status status))
        (project-members (mapcar (lambda (user) (make-instance 'user-project
                                                               :project project
                                                               :user user))
                                 (append members (list leader)))))
    (mito:insert-dao project)
    (mapcar #'mito:insert-dao project-members)
    project))

(defun create-project-leg (&key project leader members deliverable due-date status)
  (let* ((leg (make-instance 'project-leg
                            :project project
                            :leader leader
                            :deliverable deliverable
                            :due-date due-date
                            :status status))
        (leg-members (mapcar (lambda (user) (make-instance 'user-leg
                                                           :project-leg leg
                                                           :user user))
                             members)))
    (mito:insert-dao leg)
    (mapcar #'mito:insert-dao leg-members)
    leg))

(defun change-project (id &key name leader summary status remove-leg leg add-member remove-member member)
  (let ((project (mito:find-dao 'project :id id)))
    (if (not (equal name nil)) (setf (slot-value project 'project-name) name))
    (if (not (equal leader nil)) (setf (slot-value project 'project-leader) leader))
    (if (not (equal summary nil)) (setf (slot-value project 'project-summary) summary))
    (if (not (equal status nil)) (setf (slot-value project 'project-status) status))
    (if (and (equal remove-leg t) (not (eq leg nil))) (remove-leg-from-project-and-db (mito:object-id leg)))
    (if (and (equal add-member t) (not (eq member nil))) (add-member-to-project project member))
    (if (and (equal remove-member t) (not (eq member nil))) (remove-member-from-project project member))
    (mito:save-dao project)
    project))


(defun change-project-leg (id &key project leader deliverable due-date status add-member remove-member member)
  (let ((leg (mito:find-dao 'project-leg :id id)))
    (if (not (equal project nil)) (setf (slot-value leg 'project-leg-project) project))
    (if (not (equal leader nil)) (setf (slot-value leg 'project-leg-leader-id) leader))
    (if (not (equal deliverable nil)) (setf (slot-value leg 'project-leg-deliverable) deliverable))
    (if (not (equal due-date nil)) (setf (slot-value leg 'project-leg-due-date) due-date))
    (if (not (equal status nil)) (setf (slot-value leg 'project-leg-status) status))
    (if (and (equal add-member t) (not (eq member nil))) (add-member-to-leg leg member))
    (if (and (equal remove-member t) (not (eq member nil))) (remove-member-from-leg leg member))
    (mito:save-dao leg)
    leg))

;; Internals

(defun remove-leg-from-project-and-db (leg-id)
  (let ((leg (mito:find-dao 'project-leg :id leg-id)))
    (mito:delete-dao leg)
    (mapcar #'mito:delete-dao (mito:select-dao 'user-leg (sxql:where (:= 'project_leg_id leg-id))))))

(defun add-member-to-project (project member)
  (let ((mapping (make-instance 'user-project :user member
                                              :project project)))
    (mito:insert-dao mapping)))

(defun remove-member-from-project (project member)
  (mito:delete-dao (mito:find-dao 'user-project :user member :project project)))

(defun add-member-to-leg (leg member)
  (let ((mapping (make-instance 'user-leg :user member :project-leg leg)))
    (mito:insert-dao mapping)))

(defun remove-member-from-leg (leg member)
  (mito:delete-dao (mito:find-dao 'user-leg :user member :project-leg leg)))
