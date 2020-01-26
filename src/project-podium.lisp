(in-package :cl-user)
(defpackage project-podium
  (:use :cl :lucerne)
  (:export :app)
  (:documentation "Main project-podium code."))
(in-package :project-podium)
(annot:enable-annot-syntax)

;;; App

(defapp app
  :middlewares ((clack.middleware.static:<clack-middleware-static>
                 :root (asdf:system-relative-pathname :project-podium #p"assets/")
                 :path "/static/")))

;;; Templates

(djula:add-template-directory
 (asdf:system-relative-pathname :project-podium #p"templates/"))

(defparameter +index+ (djula:compile-template* "index.html"))

;;; Views

@route app "/"
(defview index ()
  (render-template (+index+)))

; @route app (:post "/projects")
; (defview projects ()
;   ())
