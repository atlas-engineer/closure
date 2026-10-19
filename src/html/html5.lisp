;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SGML; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: HTML5 parsing for Closure (via cl-html5-parser)
;;; ---------------------------------------------------------------------------
;;;
;;; Uses the html5lib-compatible parser so Closure accepts modern HTML5
;;; documents. Tree construction follows the HTML5 algorithm; the result is
;;; converted into Closure's SGML PT model for the existing renderer.

(defpackage :closure.html5
  (:use :cl)
  (:export #:parse-html
           #:parse-html5-to-pt
           #:html5-xmls->pt
           #:input->html-string))

(in-package :closure.html5)

(defun html5-xmls->pt (node)
  "Convert an HTML5 :XMLS tree node into an SGML PT."
  (cond
    ((null node) nil)
    ((stringp node)
     (sgml::make-pt :name :pcdata
                    :attrs (runes:string-rod node)
                    :children nil))
    ((and (consp node) (eq (car node) :comment))
     ;; Closure's PT model has no comment nodes; drop them.
     nil)
    ((consp node)
     (destructuring-bind (name attrs &rest children) node
       (let* ((tag (if (consp name)
                       ;; Namespaced name: (local-name . namespace-uri)
                       (intern (string-upcase (car name)) :keyword)
                       (intern (string-upcase name) :keyword)))
              (attr-plist
               (loop for pair in (or attrs '())
                     for key = (if (consp (first pair))
                                   (first (first pair))
                                   (first pair))
                     for value = (second pair)
                     nconc (list (intern (string-upcase key) :keyword)
                                 (runes:string-rod (or value "")))))
              (kids (loop for child in children
                          for pt = (html5-xmls->pt child)
                          when pt collect pt)))
         (sgml::make-pt :name tag :attrs attr-plist :children kids))))
    (t
     nil)))

(defun input->html-string (input &optional (charset :utf-8))
  "Normalize various Closure input sources into a Unicode string."
  (declare (ignore charset))
  (etypecase input
    (string input)
    (pathname
     (with-open-file (in input :direction :input :element-type 'character
                         :external-format :utf-8)
       (with-output-to-string (out)
         (loop for line = (read-line in nil nil)
               while line
               do (write-line line out)))))
    (stream
     (if (subtypep (stream-element-type input) 'character)
         (with-output-to-string (out)
           (loop for line = (read-line input nil nil)
                 while line
                 do (write-line line out)))
         ;; Byte stream: let the caller/pass-through handle encoding.
         input))
    (t
     ;; Gstreams and other Closure I/O objects expose character reads.
     (glisp:gstream-as-string input))))

(defun parse-html5-to-pt (input &optional (charset :utf-8))
  "Parse HTML5 from INPUT and return an SGML PT root (:HTML)."
  (let* ((source (input->html-string input charset))
         (xmls (if (streamp source)
                   (html5-parser:parse-html5 source :encoding charset :dom :xmls)
                   (html5-parser:parse-html5 source :dom :xmls))))
    (or (html5-xmls->pt xmls)
        (sgml::make-pt :name :html
                       :attrs nil
                       :children
                       (list (sgml::make-pt :name :head :attrs nil :children nil)
                             (sgml::make-pt :name :body :attrs nil :children nil))))))

(defun parse-html (input &optional (charset :utf-8))
  "HTML entry point for Closure: HTML5-compatible parse into PT."
  (parse-html5-to-pt input charset))
