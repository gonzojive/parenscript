;;;; -*- lisp -*-

(asdf:defsystem :parenscript
  :name "parenscript"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :maintainer "Vladimir Sedach <vsedach@gmail.com>"
  :licence "BSD"
  :description "Parenscript is a Lispy language that compiles to JavaScript."
  :components ((:static-file "parenscript.asd")
               (:module :src
                        :serial t
                        :components ((:file "package")
                                     (:file "utils")
                                     (:file "namespace")
                                     (:file "parse-lambda-list")
                                     (:file "compiler")
                                     (:file "printer")
                                     (:file "compilation-interface")
                                     (:file "special-forms")
                                     (:file "macros")
                                     (:file "deprecated-interface")
                                     (:file "js-dom-symbol-exports")
                                     (:file "swank-parenscript")
                                     ;; standard library
                                     (:module :lib
                                              :components ((:file "ps-html")
                                                           (:file "ps-loop")
                                                           (:file "ps-dom"))
                                              :depends-on ("compilation-interface"))))
               (:module :runtime
                        :components ((:file "ps-runtime-lib"))
                        :depends-on (:src)))
  :depends-on (:cl-ppcre :anaphora))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :parenscript))))
  (asdf:operate 'asdf:load-op :parenscript.test)
  (funcall (intern (symbol-name '#:run-tests)
                   (find-package '#:parenscript-test))))
