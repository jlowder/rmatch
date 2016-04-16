(defsystem rmatch
  :name "rmatch"
  :version "0.1.0"
  :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
  :license "MIT"
  :description "Racket-style matching functions"
  :long-description "Common lisp implementation of some macros from racket/match: match-let, match-let*, and defun/match. Built on top of Paul Graham's destructuring code from On Lisp."
  :components ((:module "src"
                        :components ((:file "rmatch")))))
