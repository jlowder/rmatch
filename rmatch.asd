(defsystem rmatch
    :name "rmatch"
    :version "0.1.0"
    :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
    :licence "MIT"
    :description "Racket-style matching functions"
    :components ((:module "src"
                          :components ((:file "rmatch")))))

(defsystem rmatch-test
  :depends-on (:lisp-unit :rmatch)
  :components ((:module "test"
                        :components ((:file "rmatch-test")))))
