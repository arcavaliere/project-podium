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
      :project-leg-leader
      :project-leg-members
      :project-leg-deliverable
      :project-leg-due-date
      :project-leg-status)
  (:export
      :find-user
      :register-user
      :projects
      :project-legs
      :create-project
      :create-project-leg
      :change-project
      :change-project-leg))
(in-package :project-podium.models)


;; Models

(clsql:def-view-class user ()
  ((user-id :accessor user-user-id
            :db-kind :key
            :db-constraints :not-null
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
       :db-constraints :not-null
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

(clsql:def-view-class project-leg
  ((id :accessor project-leg-id
       :db-kind :key
       :db-constraints :not-null
       :type integer
       :initarg :id)
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
