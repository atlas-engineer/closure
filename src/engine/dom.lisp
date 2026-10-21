;;; DOM built from the HTML5 parser. Layout never touches McCLIM or the old PT tree.

(in-package :closure.engine)

(defstruct dom-node
  parent
  (children nil))

(defstruct (dom-document (:include dom-node))
  (stylesheets nil)
  (keyframes nil))

(defstruct (dom-element (:include dom-node))
  (tag "")
  (id nil)
  (classes nil)
  (attrs nil)
  style
  before-style
  after-style)

(defstruct (dom-text (:include dom-node))
  (text ""))

(defun whitespace-char-p (c)
  (and (characterp c)
       (member c '(#\space #\tab #\newline #\return #\page))))

(defun split-ws (string)
  (let ((out nil) (start nil) (n (length string)))
    (loop for i from 0 below n
          for c = (char string i)
          do (if (whitespace-char-p c)
                 (when start
                   (push (subseq string start i) out)
                   (setf start nil))
                 (unless start (setf start i))))
    (when start (push (subseq string start) out))
    (nreverse out)))

(defun whitespace-only-p (string)
  (or (null string) (every #'whitespace-char-p string)))

(defun dom-attr (el name)
  (cdr (assoc name (dom-element-attrs el) :test #'string-equal)))

(defun set-dom-attr (el name value)
  "Set or replace an attribute. VALUE is stored as a string."
  (let* ((key (string-downcase name))
         (text (if (stringp value) value (princ-to-string value)))
         (cell (assoc key (dom-element-attrs el) :test #'string-equal)))
    (if cell
        (setf (cdr cell) text)
        (push (cons key text) (dom-element-attrs el)))
    text))

(defun remove-dom-attr (el name)
  (setf (dom-element-attrs el)
        (remove name (dom-element-attrs el) :key #'car :test #'string-equal))
  nil)

(defun form-options (el)
  (let ((opts nil))
    (labels ((walk (n)
               (dolist (c (element-children n))
                 (if (string-equal (dom-element-tag c) "option")
                     (push c opts)
                     (walk c)))))
      (walk el))
    (nreverse opts)))

(defun form-control-value (el)
  "The current value of a form control. Empty string when EL is not one."
  (let ((tag (and (dom-element-p el) (dom-element-tag el))))
    (cond
      ((string= tag "textarea") (dom-text-content el))
      ((string= tag "button") (or (dom-attr el "value") (dom-text-content el)))
      ((string= tag "select")
       (let* ((opts (form-options el))
              (hit (or (find-if (lambda (o) (dom-attr o "selected")) opts)
                       (first opts))))
         (if hit (or (dom-attr hit "value") (dom-text-content hit)) "")))
      ((string= tag "input")
       (let ((type (string-downcase (or (dom-attr el "type") "text"))))
         (cond
           ((member type '("checkbox" "radio") :test #'string=)
            (if (dom-attr el "checked") "on" ""))
           ((member type '("submit" "button" "reset") :test #'string=)
            (or (dom-attr el "value")
                (cond ((string= type "submit") "Submit")
                      ((string= type "reset") "Reset")
                      (t ""))))
           (t (or (dom-attr el "value") "")))))
      (t ""))))

(defun set-form-control-value (el text)
  (let ((tag (and (dom-element-p el) (dom-element-tag el))))
    (cond
      ((or (string= tag "textarea") (string= tag "button"))
       (set-element-text el text))
      ((string= tag "select")
       (set-dom-attr el "value" text)
       (dolist (o (form-options el))
         (if (or (string= text (dom-text-content o))
                 (and (dom-attr o "value") (string= text (dom-attr o "value"))))
             (set-dom-attr o "selected" "selected")
             (remove-dom-attr o "selected"))))
      ((string= tag "input")
       (let ((type (string-downcase (or (dom-attr el "type") "text"))))
         (cond
           ((member type '("checkbox" "radio") :test #'string=)
            (if (or (string= text "") (string-equal text "false") (string= text "off"))
                (remove-dom-attr el "checked")
                (set-dom-attr el "checked" "checked")))
           (t (set-dom-attr el "value" text))))))
    text))

(defun url-encode (string)
  "application/x-www-form-urlencoded. Space is +."
  (with-output-to-string (out)
    (flet ((emit (n) (format out "%~2,'0X" n)))
      (loop for c across (or string "")
            for code = (char-code c)
            do (cond
                 ((or (alphanumericp c) (find c "-._~")) (write-char c out))
                 ((char= c #\space) (write-char #\+ out))
                 ((< code 128) (emit code))
                 ((< code 2048)
                  (emit (logior #xc0 (ldb (byte 5 6) code)))
                  (emit (logior #x80 (ldb (byte 6 0) code))))
                 (t
                  (emit (logior #xe0 (ldb (byte 4 12) code)))
                  (emit (logior #x80 (ldb (byte 6 6) code)))
                  (emit (logior #x80 (ldb (byte 6 0) code)))))))))

(defun form-ancestor (el)
  (loop for n = el then (and n (dom-node-parent n))
        while (dom-element-p n)
        when (string-equal (dom-element-tag n) "form")
          return n))

(defun submit-control-p (el)
  (and (dom-element-p el)
       (let ((tag (string-downcase (dom-element-tag el)))
             (type (string-downcase (or (dom-attr el "type") ""))))
         (or (and (string= tag "button")
                  (not (member (if (plusp (length type)) type "submit")
                               '("button" "reset") :test #'string=)))
             (and (string= tag "input")
                  (member type '("submit" "image") :test #'string=))))))

(defun form-controls (form)
  (let ((out nil))
    (labels ((walk (n)
               (dolist (c (element-children n))
                 (unless (string-equal (dom-element-tag c) "form")
                   (when (member (dom-element-tag c) '("input" "select" "textarea" "button")
                                 :test #'string-equal)
                     (push c out))
                   (walk c)))))
      (walk form))
    (nreverse out)))

(defun control-successful-p (el submit)
  (let ((tag (string-downcase (dom-element-tag el)))
        (type (string-downcase (or (dom-attr el "type") ""))))
    (cond
      ((or (null (dom-attr el "name")) (dom-attr el "disabled")) nil)
      ((and (string= tag "input") (member type '("checkbox" "radio") :test #'string=))
       (not (null (dom-attr el "checked"))))
      ((or (and (string= tag "input")
                (member type '("submit" "button" "reset" "image" "file") :test #'string=))
           (string= tag "button"))
       (eq el submit))
      (t t))))

(defun form-query (form submit)
  (with-output-to-string (out)
    (let ((first t))
      (dolist (el (form-controls form))
        (when (control-successful-p el submit)
          (unless first (write-char #\& out))
          (setf first nil)
          (write-string (url-encode (dom-attr el "name")) out)
          (write-char #\= out)
          (write-string (url-encode (form-control-value el)) out))))))

(defun form-method (form)
  (if (string-equal (or (dom-attr form "method") "get") "post")
      :post
      :get))

(defun form-request (form submit base)
  "Returns the method, the request URL, and the body. POST carries the query in the body."
  (if (eq (form-method form) :post)
      (let* ((action (dom-attr form "action"))
             (abs (if (and action (plusp (length action)))
                      (or (resolve-url action base) action)
                      (or base "")))
             (hash (position #\# abs)))
        (values :post (subseq abs 0 (or hash (length abs))) (form-query form submit)))
      (values :get (form-action-url form submit base) "")))

(defun form-action-url (form submit base)
  "Absolute GET URL for FORM, including the encoded query."
  (let* ((action (dom-attr form "action"))
         (abs (if (and action (plusp (length action)))
                  (or (resolve-url action base) action)
                  (or base "")))
         (query (form-query form submit))
         (hash (position #\# abs))
         (body (subseq abs 0 (or hash (length abs))))
         (frag (if hash (subseq abs hash) "")))
    (if (plusp (length query))
        (concatenate 'string body (if (position #\? body) "&" "?") query frag)
        abs)))

(defun control-label (el)
  "Text painted inside a form control."
  (let ((tag (dom-element-tag el)))
    (cond
      ((and (string= tag "input")
            (member (string-downcase (or (dom-attr el "type") "text"))
                    '("checkbox" "radio") :test #'string=))
       (if (dom-attr el "checked")
           (if (string-equal (dom-attr el "type") "radio") "●" "✓")
           ""))
      ((string= tag "select") (form-control-value el))
      ((string= tag "textarea") (dom-text-content el))
      ((string= tag "input")
       (let ((type (string-downcase (or (dom-attr el "type") "text"))))
         (if (member type '("color" "range" "file") :test #'string=)
             ""
             (or (dom-attr el "value") (dom-attr el "placeholder") ""))))
      (t ""))))

(defun element-children (node)
  (when node
    (remove-if-not #'dom-element-p (dom-node-children node))))

(defun dom-text-content (el)
  (with-output-to-string (out)
    (labels ((walk (n)
               (dolist (c (dom-node-children n))
                 (cond ((dom-text-p c) (write-string (dom-text-text c) out))
                       ((dom-element-p c) (walk c))))))
      (walk el))))

(defun element-kids (el)
  (remove-if-not #'dom-element-p (dom-node-children el)))

(defun isindex-expansion-p (el)
  "The HTML parser turns isindex into a form Chrome does not paint."
  (when (and (dom-element-p el) (string-equal (dom-element-tag el) "form"))
    (let ((kids (element-kids el)))
      (and (= (length kids) 3)
           (string-equal (dom-element-tag (first kids)) "hr")
           (string-equal (dom-element-tag (second kids)) "label")
           (string-equal (dom-element-tag (third kids)) "hr")
           (some (lambda (inner)
                   (and (string-equal (dom-element-tag inner) "input")
                        (string-equal (or (dom-attr inner "name") "") "isindex")))
                 (element-kids (second kids)))))))

(defun drop-isindex-forms (node)
  (when (dom-element-p node)
    (setf (dom-node-children node)
          (remove-if #'isindex-expansion-p (dom-node-children node)))
    (dolist (c (dom-node-children node))
      (drop-isindex-forms c))))

(defun import-document (html5-document)
  (let ((doc (make-dom-document)))
    (html5-parser:element-map-children
     (lambda (c)
       (let ((k (import-document-child c doc)))
         (when k (push k (dom-node-children doc)))))
     html5-document)
    (setf (dom-node-children doc) (nreverse (dom-node-children doc)))
    (dolist (c (dom-node-children doc))
      (drop-isindex-forms c))
    doc))

(defun document-title-text (doc)
  "Text of the title element, or the empty string."
  (let ((title (and doc (find-element doc "title"))))
    (if title (dom-text-content title) "")))

(defun set-document-title (doc text)
  "Replace the title element's text, creating the element when it is missing."
  (let ((title (or (find-element doc "title")
                   (let ((head (or (find-element doc "head")
                                   (let ((html (find-element doc "html")))
                                     (when html
                                       (let ((created (make-dom-element :parent html :tag "head")))
                                         (push created (dom-node-children html))
                                         created))))))
                     (when head
                       (let ((created (make-dom-element :parent head :tag "title")))
                         (push created (dom-node-children head))
                         created))))))
    (when title
      (set-element-text title text))
    text))

(defun find-element (node tag)
  (labels ((walk (n)
             (dolist (c (dom-node-children n))
               (when (dom-element-p c)
                 (when (string-equal (dom-element-tag c) tag)
                   (return-from find-element c))
                 (walk c)))))
    (walk node)
    nil))

(defun map-elements (node fn)
  (labels ((walk (n)
             (dolist (c (dom-node-children n))
               (when (dom-element-p c)
                 (funcall fn c)
                 (walk c)))))
    (walk node)))

(defun find-element-by-id (node id)
  (when (and node id)
    (labels ((walk (n)
               (dolist (c (dom-node-children n))
                 (when (dom-element-p c)
                   (when (and (dom-element-id c) (string= (dom-element-id c) id))
                     (return-from find-element-by-id c))
                   (walk c)))))
      (walk node)
      nil)))

(defun set-element-text (el text)
  (let ((node (make-dom-text :parent el :text (or text ""))))
    (setf (dom-node-children el) (list node))
    text))

(defun set-element-inner-html (el html)
  (let* ((frag (html5-parser:parse-html5-fragment (or html "")
                                                  :container (or (dom-element-tag el) "div")))
         (kids nil))
    (html5-parser:element-map-children
     (lambda (child)
       (let ((imported (import-document-child child el)))
         (when imported (push imported kids))))
     frag)
    (setf (dom-node-children el) (nreverse kids))
    html))

(defun import-document-child (node parent)
  "Import one HTML5 parser node. Used by innerHTML and by import-document."
  (case (html5-parser:node-type node)
    (:element
     (let ((el (make-dom-element
                :parent parent
                :tag (string-downcase (html5-parser:node-name node)))))
       (html5-parser:element-map-attributes
        (lambda (name namespace value)
          (declare (ignore namespace))
          (let ((n (string-downcase name)))
            (push (cons n value) (dom-element-attrs el))
            (cond ((string= n "id") (setf (dom-element-id el) value))
                  ((string= n "class")
                   (setf (dom-element-classes el) (split-ws value))))))
        node)
       (html5-parser:element-map-children
        (lambda (c)
          (let ((k (import-document-child c el)))
            (when k (push k (dom-node-children el)))))
        node)
       (setf (dom-node-children el) (nreverse (dom-node-children el)))
       el))
    (:text
     (let ((v (html5-parser:node-value node)))
       (when (and v (plusp (length v)))
         (make-dom-text :parent parent :text v))))
    ((:document :document-fragment)
     (let ((wrapper (make-dom-element :parent parent :tag "fragment")))
       (html5-parser:element-map-children
        (lambda (c)
          (let ((k (import-document-child c wrapper)))
            (when k (push k (dom-node-children wrapper)))))
        node)
       (setf (dom-node-children wrapper) (nreverse (dom-node-children wrapper)))
       wrapper))
    (t nil)))

(defun sibling-index (el &key of-type from-end)
  (let* ((parent (dom-node-parent el))
         (sibs (remove-if-not
                (lambda (c)
                  (and (dom-element-p c)
                       (or (not of-type)
                           (string-equal (dom-element-tag c) (dom-element-tag el)))))
                (dom-node-children parent))))
    (let ((pos (position el sibs)))
      (when pos
        (if from-end
            (- (length sibs) pos)
            (1+ pos))))))
