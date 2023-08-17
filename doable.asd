(require 'asdf)

(asdf:defsystem "doable"
  :description "A flexible task-management framework."
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:local-time #:closer-mop #:clack #:ningle #:cl-json #:alexandria #:cl-who #:parenscript #:cl-css)
  :components ((:file "doable")))

