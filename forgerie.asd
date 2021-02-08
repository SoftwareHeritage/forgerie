(asdf:defsystem forgerie
 :description "Adapter between different software forges"
 :version "0.1"
 :maintainer "Open Tech Strategies"
 :author "Open Tech Strategies"
 :serial t
 :components
 ((:module "src/main"
   :components
   ((:module "core"
     :components
     ((:file "package")
      (:file "user")
      (:file "run")))
    (:module "phabricator"
     :components
     ((:file "package")
      (:file "base")
      (:file "import")))
    (:module "gitlab"
     :components
     ((:file "package")
      (:file "base")
      (:file "export")))))
  (:module "config"
   :components ((:file "config"))))
 :depends-on (:cl-mysql :drakma :jsown :cl-ppcre))
