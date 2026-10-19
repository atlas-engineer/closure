;;; Closure layout and paint engine.
;;; The engine owns HTML styling and layout. Cairo is only the cross-platform
;;; rasterizer (image surfaces, PNG, and the same drawing model on every OS).
;;; A JavaScript engine is a later layer; see *script-evaluator*.

(defpackage :closure.engine
  (:use :cl)
  (:export
   #:render-html
   #:layout-html
   #:find-box
   #:hit-test
   #:box-x
   #:box-y
   #:box-width
   #:box-height
   #:box-id
   #:box-tag
   #:style-value
   #:describe-box
   #:*script-evaluator*))
