(in-package :cl-user)
(defpackage project-podium
  (:use :cl :lucerne)
  (:export
   :app
   :run
   :halt)
  (:documentation "Main project-podium code."))
(in-package :project-podium)
(annot:enable-annot-syntax)

;;; Helpers

(defun existing-user (username email)
  (if (> (count nil (mapcar (lambda (user) (cond ((equal (project-podium.models:user-username user) username) nil)
                                                 ((equal (project-podium.models:user-email user) email) nil)
                                                 (t t)))
                            (project-podium.models:get-users))) 0)
         nil
         t))

(defun find-user-by-username (username)
  (let ((user-list (project-podium.models:get-users)))
    (remove nil (mapcar (lambda (user) (if (equal username (project-podium.models:user-username user))
                                           user
                                           nil))
                        user-list))))

;;; App

(defapp app
  :middlewares (clack.middleware.session:<clack-middleware-session>
                (clack.middleware.static:<clack-middleware-static>
                 :root (asdf:system-relative-pathname :project-podium #p"assets/")
                 :path "/static/")))

;;; Templates

(djula:add-template-directory
 (asdf:system-relative-pathname :project-podium #p"templates/"))

(defparameter +index+ (djula:compile-template* "index.html"))
(defparameter +project-details+ (djula:compile-template* "index.html"))
(defparameter +project-leg-details+ (djula:compile-template* "index.html"))
(defparameter +project-list+ (djula:compile-template* "index.html"))

;;; Views

@route app "/"
(defview index ()
  (render-template (+index+) :user-list (project-podium.models:get-users)))

@route app (:post "/sign-up")
(defview sign-up ()
  (with-params (username full-name email password password-repeat)
    (if (existing-user username email)
        (render-template (+index+) :error "Username or email already in use!")
        (if (string= password password-repeat)
            (progn
              (project-podium.models:create-user :username username
                                                 :full-name full-name
                                                 :email email
                                                 :password password)
              (redirect "/"))
            (render-template (+index+) :error "Entered passwords did not match!")))))

@route app (:post "/sign-in")
(defview sign-in ()
  (with-params (username password)
    (let ((user (find-user-by-username username)))
      (if (not (equal user nil))
          (if (cl-pass:check-password password (project-podium.models:user-password user))
              (progn
                (lucerne-auth:login (project-podium.models:user-username user))
                (redirect "/"))
              (render-template (+index+) :error "Bad Password!")))
      (render-template (+index+) :error "User does not exist!"))))

@route app "/projects"
(defview projects ()
  (render-template (+project-list+) :project-list (project-podium.models:get-projects)))

@route app "/projects/:project-id"
(defview project-display (project-id)
  (let ((project (project-podium.models:get-projects :project-id project-id)))
    (render-template (+project-details+) :project-name (project-podium.models:project-name project)
                     :project-status (project-podium.models:project-status project)
                     :project-summary (project-podium.models:project-summary project)
                     :project-leader (project-podium.models:project-leader project)
                     :project-legs (project-podium.models:get-project-legs :project project))))

@route app "/projects/:project-id/leg/:project-leg-id"
(defview project-leg-display (project-id project-leg-id)
  (let ((project (project-podium.models:get-projects :project-id project-id))
        (project-leg (project-podium.models:get-project-legs :project-leg-id project-leg-id)))
    (render-template (+project-leg-details+)
                     :project project
                     :project-leg-deliverable (project-podium.models:project-leg-deliverable project-leg)
                     :project-leg-status (project-podium.models:project-leg-status project-leg)
                     :project-leg-due-date (project-podium.models:project-leg-due-date project-leg)
                     :project-leg-leader (project-podium.models:project-leg-leader project-leg)
                     :project-leg-members (project-podium.models:get-project-leg-members project-leg))))

@route app (:post "/projects/create")
(defview project-create ()
  (with-params (name leader summary status members)
    (let* ((leader-user (project-podium.models:get-user (find-user-by-username leader)))
           (member-users (mapcar #'find-user-by-username members))
           (project (project-podium.models:create-project :name name :leader leader-user :summary summary :status status :members member-users)))
    (redirect (format nil "/projects/~A" (project-podium.models:get-primary-key-value project))))))

@route app (:post "/projects/:project-id/edit")
(defview project-edit (project-id)
  (with-params (name leader summary status remove-leg leg add-member remove-member member)
    (let* ((leader-user (if (not (equal leader nil)) (find-user-by-username leader)))
           (member-user (if (not (equal member nil)) (find-user-by-username member)))
           (leg-obj (if (not (equal leg nil)) (project-podium.models:get-project-legs :project-leg-id leg))))
      (project-podium.models:change-project project-id :name name
                                                       :leader leader-user
                                                       :summary summary
                                                       :status status
                                                       :remove-leg remove-leg
                                                       :leg leg-obj
                                                       :add-member add-member
                                                       :remove-member remove-member
                                                       :member member-user)
      (redirect (format nil "/projects/~A" project-id)))))

@route app (:post "/project-legs/create")
(defview project-leg-create ()
  (with-params (project-id leader members deliverable due-date status)
    (let* ((leader (find-user-by-username leader))
           (member-users (mapcar #'find-user-by-username members))
           (project (project-podium.models:get-projects :project-id project-id))
           (project-leg (project-podium.models:create-project-leg :project project :leader leader :members member-users :deliverable deliverable :due-date due-date :status status)))
      (redirect (format nil "/projects/~A/leg/~A" (project-podium.models:get-primary-key-value project) (project-podium.models:get-primary-key-value project-leg))))))

@route app (:post "/project/:project-id/leg/:project-leg-id/edit")
(defview project-leg-edit (project-id project-leg-id)
  (with-params (leader deliverable due-date status add remove member)
    (let* ((leader-user (if (not (equal leader nil)) (find-user-by-username leader)))
           (member-user (if (not (equal member nil)) (find-user-by-username member))))
      (project-podium.models:change-project-leg project-leg-id :project (project-podium.models:get-projects :project-id project-id)
                                                               :leader leader-user
                                                               :deliverable deliverable
                                                               :due-date due-date
                                                               :status status
                                                               :add-member add
                                                               :remove-member remove
                                                               :member member-user)
      (redirect (format nil "/projects/~A/leg/~A" project-id project-leg-id)))))

; @route app (:post "/projects")
; (defview projects ()
;   ())

;;; Entry Point

(defun run ()
  (project-podium.models:connect-to-database-local "project_podium" "podium" "podium")
  (project-podium.models:initialize-database)
  (lucerne:start app :address "0.0.0.0" :port 8080))

(defun halt ()
  (project-podium.models:disconnect-from-database)
  (lucerne:stop app))
