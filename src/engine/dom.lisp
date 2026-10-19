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

(defun import-document (html5-document)
  (let ((doc (make-dom-document)))
    (html5-parser:element-map-children
     (lambda (c)
       (let ((k (import-document-child c doc)))
         (when k (push k (dom-node-children doc)))))
     html5-document)
    (setf (dom-node-children doc) (nreverse (dom-node-children doc)))
    doc))

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
