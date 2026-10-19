;;; HTML5 compatibility checks for Closure.
;;;
;;; 1. Delegate to cl-html5-parser's html5lib suite (tokenizer + tree construction).
;;; 2. Smoke-test Closure's HTML5 → PT bridge on representative modern markup.

(defpackage :closure-html5-tests
  (:use :cl)
  (:export #:run-html5-compatibility-tests))

(in-package :closure-html5-tests)

(defun run-html5lib-suite ()
  "Run html5lib tests bundled with cl-html5-parser. Returns T on success."
  (ql:quickload :cl-html5-parser-tests :silent t)
  (let ((hard-failures 0))
    (handler-bind
        ((warning #'muffle-warning)
         (error
          (lambda (c)
            (incf hard-failures)
            (format *error-output* "~&HTML5lib failure: ~A~%" c)
            (let ((restart (find-restart 'html5-parser-tests::skip c)))
              (cond
                (restart (invoke-restart restart))
                ((find-restart 'continue c) (invoke-restart 'continue))
                (t (return-from run-html5lib-suite nil)))))))
      (let ((*standard-output* (make-broadcast-stream)))
        (html5-parser-tests::test-tokenizer)
        (html5-parser-tests::test-parser)))
    (zerop hard-failures)))

(defun smoke-test-closure-html5-bridge ()
  "Parse HTML5 samples through Closure's PT bridge."
  (ql:quickload :closure/html5 :silent t)
  (let* ((samples
          (list
           "<!DOCTYPE html><html><body><article><h1>Hi</h1><p>Hello</p></article></body></html>"
           "<!DOCTYPE html><section><header>Head</header><nav>Nav</nav><main>Main</main><footer>Foot</footer></section>"
           "<!DOCTYPE html><div><video src=\"x.mp4\"></video><canvas></canvas></div>"
           "<!DOCTYPE html><p>A<img src=\"a.png\" alt=\"x\">B</p>"))
         (ok t))
    (dolist (html samples ok)
      (let ((pt (closure.html5:parse-html5-to-pt html)))
        (unless (and (sgml::pt-p pt)
                     (eq (sgml:pt-name pt) :html))
          (format *error-output* "~&Bridge failed for:~%~A~%" html)
          (setf ok nil))))))

(defun run-html5-compatibility-tests ()
  (format t "~&;; Running html5lib compatibility suite ...~%")
  (unless (run-html5lib-suite)
    (error "html5lib compatibility suite reported failures"))
  (format t "~&;; html5lib suite: PASS~%")
  (format t "~&;; Running Closure HTML5→PT bridge smoke tests ...~%")
  (unless (smoke-test-closure-html5-bridge)
    (error "Closure HTML5 bridge smoke tests failed"))
  (format t "~&;; Closure HTML5 bridge: PASS~%")
  t)
