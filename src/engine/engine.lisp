;;; Public entry points. JavaScript is intentionally not executed here;
;;; bind *script-evaluator* when a script engine is added.

(in-package :closure.engine)

(defvar *script-evaluator* nil
  "Reserved for the JavaScript engine. When bound to a function, it is
called as (funcall *script-evaluator* element source-string) for each
script element after style and layout. Left unset until that engine exists.")

(defparameter *ua-css*
  "html { display: block; }
   head, script, style, title, meta, link, template, base { display: none; }
   body { display: block; margin: 8px; }
   div, p, h1, h2, h3, h4, h5, h6, article, section, nav, header, footer, main,
   figure, figcaption, details, summary, dialog, hgroup, menu, dir, center, hr,
   blockquote, pre, form, ul, ol, dl, dd, dt, address, fieldset, legend
   { display: block; }
   h1, h2, h3, h4, h5, h6, b, strong, th { font-weight: bold; }
   em, i, cite, dfn, var { font-style: italic; }
   h1 { font-size: 2em; margin-top: 0.67em; margin-bottom: 0.67em; }
   h2 { font-size: 1.5em; margin-top: 0.83em; margin-bottom: 0.83em; }
   h3 { font-size: 1.17em; margin-top: 1em; margin-bottom: 1em; }
   p, ul, ol, dl, blockquote, pre { margin-top: 1em; margin-bottom: 1em; }
   ul, ol { padding-left: 40px; }
   ul { list-style-type: disc; }
   ol { list-style-type: decimal; }
   li { display: list-item; }
   span, a, b, i, em, strong, code, small, label, mark, time, abbr, cite, dfn,
   kbd, samp, var, sub, sup, u, s, q, button { display: inline; }
   a { color: #0000ee; }
   code, kbd, samp, pre { font-family: monospace; }
   pre { white-space: pre; }
   table { display: table; }
   thead { display: table-header-group; }
   tbody { display: table-row-group; }
   tfoot { display: table-footer-group; }
   tr { display: table-row; }
   td, th { display: table-cell; padding: 1px; }
   img, video, canvas, svg, input, select, textarea, iframe { display: inline-block; }
   video, canvas { width: 300px; height: 150px; }
   hr { border-top: 1px solid #ccc; margin-top: 0.5em; margin-bottom: 0.5em; }
   center { text-align: center; }
   button, input, select, textarea { font-family: sans-serif; }")

(defun collect-rules (sheets)
  (let ((rules nil)
        (frames (make-hash-table :test #'equal)))
    (dolist (sh sheets)
      (setf rules (append rules (stylesheet-rules sh)))
      (maphash (lambda (k v) (setf (gethash k frames) v))
               (stylesheet-keyframes sh)))
    (values rules frames)))

(defun frame-raw (frame property)
  (dolist (d (cdr frame))
    (when (eq (css-decl-property d) property)
      (return (css-decl-raw d)))))

(defun mix (a b p)
  (+ (* a (- 1d0 p)) (* b p)))

(defun apply-animations (element frames time)
  (when (and (dom-element-p element) (dom-element-style element))
    (let* ((st (dom-element-style element))
           (name (computed-animation-name st))
           (dur (computed-animation-duration st)))
      (when (and (stringp name) dur (> dur 0d0))
        (let ((track (gethash (string-downcase name) frames)))
          (when (and track (>= (length track) 2))
            (let* ((iter (computed-animation-iteration st))
                   (u (if (eq iter :infinite)
                          (mod (/ time dur) 1d0)
                          (min 1d0 (max 0d0 (/ time dur)))))
                   (pct (* u 100d0))
                   (lo (car track))
                   (hi (car (last track))))
              (loop for (a b) on track
                    while b
                    when (and (>= pct (car a)) (<= pct (car b)))
                      do (setf lo a hi b))
              (let* ((span (max 0.0001d0 (- (car hi) (car lo))))
                     (p (/ (- pct (car lo)) span))
                     (c0 (frame-raw lo :color))
                     (c1 (frame-raw hi :color))
                     (o0 (frame-raw lo :opacity))
                     (o1 (frame-raw hi :opacity)))
                (when (and o0 o1)
                  (let ((a (ignore-errors (float (read-from-string o0) 1d0)))
                        (b (ignore-errors (float (read-from-string o1) 1d0))))
                    (when (and a b)
                      (setf (computed-opacity st) (mix a b p)))))
                (when (and c0 c1)
                  (let ((ca (car (parse-color-tokens (css-tokenize c0))))
                        (cb (car (parse-color-tokens (css-tokenize c1)))))
                    (when (and (consp ca) (consp cb))
                      (setf (computed-color st)
                            (list (mix (first ca) (first cb) p)
                                  (mix (second ca) (second cb) p)
                                  (mix (third ca) (third cb) p)
                                  (mix (or (fourth ca) 1d0) (or (fourth cb) 1d0) p))))))))))))
    (dolist (c (element-children element))
      (apply-animations c frames time))))

(defun note-scripts (element)
  (when (and *script-evaluator* (dom-element-p element))
    (when (string= (dom-element-tag element) "script")
      (funcall *script-evaluator* element (dom-text-content element)))
    (dolist (c (element-children element))
      (note-scripts c))))

(defun layout-html (html &key (width 1024) (height 768) (stylesheet "") (time 0d0))
  "Parse HTML, cascade CSS, and lay the document out. Returns the root box."
  (let* ((*viewport-width* (float width 1d0))
         (*viewport-height* (float height 1d0))
         (*root-font-size* 16d0)
         (doc (import-document (html5-parser:parse-html5 (or html ""))))
         (html-el (or (find-element doc "html")
                      (first (element-children doc))))
         (sheets (list (parse-stylesheet *ua-css* :origin :ua :width width :height height))))
    (when (and stylesheet (plusp (length stylesheet)))
      (setf sheets (append sheets
                           (list (parse-stylesheet stylesheet :origin :author
                                                   :width width :height height)))))
    (let ((style-els nil))
      (map-elements doc (lambda (el)
                          (when (string= (dom-element-tag el) "style")
                            (push el style-els))))
      (dolist (el (nreverse style-els))
        (setf sheets (append sheets
                             (list (parse-stylesheet (dom-text-content el) :origin :author
                                                     :width width :height height))))))
    (unless html-el
      (return-from layout-html nil))
    (multiple-value-bind (rules frames) (collect-rules sheets)
      (cascade-tree html-el rules)
      (apply-animations html-el frames (float time 1d0))
      (note-scripts html-el)
      (layout-document html-el))))

(defun render-html (html &key (width 1024) (height 768) (stylesheet "")
                           (output nil) (time 0d0) (scroll-y 0d0))
  "Lay HTML out and, when OUTPUT is a pathname, paint it to a PNG with Cairo.
Returns the root box. Cairo is the cross-platform rasterizer; the same paint
path targets an image surface here and a window surface on each OS."
  (let ((root (layout-html html :width width :height height
                           :stylesheet stylesheet :time time)))
    (when (and root output)
      (let* ((doc-h (max height
                         (ceiling (+ (box-y root) (box-h root) 8))))
             (png-h (min doc-h 8192)))
        (paint-png root output width png-h :scroll-y scroll-y)))
    root))
