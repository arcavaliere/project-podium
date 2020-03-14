(in-package :cl-user)
(defpackage project-podium.models
  (:use :cl :clsql)
  (:export
    :user
    :user-username
    :user-full-name
    :user-email
    :user-password)
  (:export
      :project
      :project-name
      :project-leader
      :project-members
      :projec-summary
      :project-legs
      :project-status)
  (:export
      :project-leg
      :project-leg-project
      :project-leg-leader
      :project-leg-members
      :project-leg-deliverable
      :project-leg-due-date
      :project-leg-status)
  (:export
      :find-user
      :register-user
      :get-projects
      :get-project-legs
      :create-project
      :create-project-leg
      :change-project
      :change-project-leg
      :connect-to-database))
(in-package :project-podium.models)
(clsql:file-enable-sql-reader-syntax)

;; Models

(clsql:def-view-class user ()
  ((user-id :accessor user-user-id
            :db-kind :key
            :db-constraints (:not-null :unique)
            :type integer
            :initarg :user-id)
   (username :accessor user-username
             :initarg :username
             :type (string 30))
   (full-name :accessor user-full-name
              :initarg :full-name
              :type (string 60))
   (email :accessor user-email
          :initarg :email
          :type (string 85)
          :nulls-ok t)
   (password :accessor user-passowrd
             :initarg :password
             :type (string 64)))
  (documentation "A user")
  (:base-table user))

(clsql:def-view-class project ()
  ((id :accessor project-id
       :db-kind :key
       :db-constraints (:not-null :unique)
       :type integer
       :initarg :id)
   (name :accessor project-name
         :initarg :name
         :type (string 64))
   (leader :accessor project-leader
           :initarg :leader
           :db-kind :join
           :db-info (:join-class user
                     :home-key project-id
                     :foreign-key user-id
                     :set nil))
   (members :accessor project-members
            :initiarg :members
            :db-kind :join
            :db-info (:join-class user
                      :home-key id
                      :foreign-key user-id
                      :set t))
   (summary :accessor project-summary
            :initarg :summary
            :type (string 256))
   (legs :accessor project-legs
         :initarg :legs
         :db-kind (:join-class project-leg
                   :home-key project-id
                   :foreign-key project-leg-id
                   :set t))
   (status :accessor project-status
           :initarg :status
           :type (string 48)))
  (documentation "A project")
  (:base-table project))

(clsql:def-view-class project-leg ()
  ((id :accessor project-leg-id
       :db-kind :key
       :db-constraints (:not-null :unique)
       :type integer
       :initarg :id)
   (project :accessor project-leg-project
            :initarg :project
            :db-kind :join
            :db-info (:join-class project
                      :home-key project-leg-id
                      :foreign-key project-id
                      :set nil))
   (leader :accessor project-leg-leader
           :initarg :leader
           :db-kind :join
           :db-info (:join-class user
                     :home-key project-leg-id
                     :foreign-key user-id
                    :set nil))
   (members :accessor project-leg-members
            :initarg :members
            :db-kind :join
            :db-info (:join-class user
                      :home-key project-leg-id
                      :foreign-key user-id
                      :set t))
   (deliverable :accessor project-leg-deliverable
                :initarg :deliverable
                :type (string 256))
   (due-date :accessor project-leg-due-date
             :initarg :due-date
             :type (string 30))
   (status :accessor project-leg-status
           :initarg :status
           :type (string 48)))
  (documentation "A project leg")
  (:base-table project-leg))

;; Storage

(defun connect-to-database (database-name)
  (clsql:connect database-name :database-type :sqlite3)
  (let ((tables (list 'user 'project 'project-leg)))
    (mapcar #'create-table-if-missing tables)))

(defun create-table-if-missing (table-name)
  (if (not (clsql:table-exists-p table-name))
      (clsql:create-view-from-class table-name)
      t))

;; Helpers

(defun find-user (user-id)
  (clsql:select 'user :where [= [slot-value 'user-id user-id]]))

(defun register-user (&key username full-name email password)
  (let ((user (make-instance 'user
                  :username username
                  :full-name full-name
                  :email email
                  :password (cl-pass:hash password))))
     (clsql:update-records-from-instance user)
   user))

(defun get-projects (&key project-id leader-id)
    (cond ((not (eq project-id nil)) (clsql:select 'project :where [= [slot-value 'project-id project-id]]))
          ((not (eq leader-id nil)) (clsql:select 'project :where [= [slot-value 'project-leader leader-id]]))
          (t (clsql:select 'project))))

(defun get-project-legs (&key project-id project-leg-id leader-id)
    (cond ((not (eq project-id nil)) (clsql:select 'project-leg :where [= [slot-value 'project-leg-project project-id]]))
          ((not (eq project-leg-id nil)) (clsql:select 'project-leg :where [= [slot-value 'project-leg-id project-leg-id]]))
          ((not (eq leader-id nil)) (clsql:select 'project-leg :where [= [slot-value 'project-leg-leader leader-id]]))
          (t (clsql:select 'project-leg))))

(defun create-project (&key name leader summary status members)
  (let ((project (make-instance 'project
                                :name name
                                :leader leader
                                :summary summary
                                :status status
                                :members members
                                :legs nil)))
    (clsql:update-records-from-instance project)
    project))

(defun create-project-leg (&key project leader members deliverable due-date status)
  (let ((leg (make-instance 'project-leg
                            :project project
                            :leader leader
                            :members members
                            :deliverable deliverable
                            :due-date due-date
                            :status status)))
    (clsql:update-records-from-instance leg)
    leg))

(defun change-project (id &key name leader members summary legs status)
  (let ((project (car (clsql:select 'project :where [= ['slot-value project-id id]]))))
    (cond ((not (eq name nil)) (setf (slot-value project 'project-name) name))
          ((not (eq leader nil)) (setf (slot-value project 'project-leader) leader))
          ((not (eq members nil)) (setf (slot-value project 'project-members) members))
          ((not (eq summary nil)) (setf (slot-value project 'project-summary) summary))
          ((not (eq legs nil)) (setf (slot-value project 'project-legs) legs))
          ((not (eq status nil)) (setf (slot-value project 'project-status) status)))
    (clsql:update-records-from-instance project)
    project))

(defun change-project-leg (id &key project leader members deliverable due-date status)
  (let ((leg (car (clsql:select 'project-leg :where [= ['slot-value project-leg-id id]]))))
    (cond ((not (eq project nil)) (setf (slot-value leg 'project-leg-project) project))
          ((not (eq leader nil)) (setf (slot-value leg 'project-leg-leader) leader))
          ((not (eq members nil)) (setf (slot-value leg 'project-leg-members) members))
          ((not (eq deliverable nil)) (setf (slot-value leg 'project-leg-deliverable) deliverable))
          ((not (eq due-date nil)) (setf (slot-value leg 'project-leg-due-date) due-date))
          ((not (eq status nil)) (setf (slot-value leg 'project-leg-status) status)))
    (clsql:update-records-from-instance leg)
    leg))
