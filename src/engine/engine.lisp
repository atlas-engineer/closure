;;; Public entry points. Classic scripts run in the Lisp interpreter.
;;; Bind *script-evaluator* to replace that interpreter.

(in-package :closure.engine)

(defvar *navigation-depth* 0
  "How many location.href navigations are already in progress.")

(defvar *navigation-enabled* t
  "When NIL, location.href is recorded but does not replace the document.")

(defvar *script-evaluator* nil
  "Reserved for the JavaScript engine. When bound to a function, it is
called as (funcall *script-evaluator* element source-string) for each
script element after style and layout. Left unset until that engine exists.")

(defparameter *ua-css*
  "html { display: block; }
   head, script, style, title, meta, link, template, base, basefont, noframes, noembed, param, source, area, noscript { display: none; }
   frameset, frame { display: block; }
   body { display: block; margin: 8px; }
   div, p, h1, h2, h3, h4, h5, h6, article, section, nav, aside, header, footer, main,
   figure, figcaption, details, summary, dialog, hgroup, menu, dir, center, hr,
   blockquote, pre, form, ul, ol, dl, dd, dt, address, fieldset, legend
   { display: block; }
   h1, h2, h3, h4, h5, h6, b, strong, th { font-weight: bold; }
   em, i, cite, dfn, var, address { font-style: italic; }
   h1 { font-size: 2em; margin-top: 0.67em; margin-bottom: 0.67em; }
   h2 { font-size: 1.5em; margin-top: 0.83em; margin-bottom: 0.83em; }
   h3 { font-size: 1.17em; margin-top: 1em; margin-bottom: 1em; }
   h4 { font-size: 1em; margin-top: 1.33em; margin-bottom: 1.33em; }
   h5 { font-size: 0.83em; margin-top: 1.67em; margin-bottom: 1.67em; }
   h6 { font-size: 0.67em; margin-top: 2.33em; margin-bottom: 2.33em; }
   p, ul, ol, menu, dir, dl, blockquote, pre, listing, xmp, plaintext { margin-top: 1em; margin-bottom: 1em; }
   ul, ol, menu, dir { padding-left: 40px; }
   dd { margin-left: 40px; }
   blockquote { margin-left: 40px; margin-right: 40px; }
   figure { margin: 1em 40px; }
   ul, menu, dir { list-style-type: disc; }
   ol { list-style-type: decimal; }
   li { display: list-item; }
   span, a, b, i, em, strong, code, small, big, tt, nobr, wbr, label, mark, time, abbr, cite, dfn,
   bdi, bdo, data, picture, map, output, font,
   kbd, samp, var, sub, sup, u, ins, s, del, strike, q, button { display: inline; }
   s, del, strike { text-decoration: line-through; }
   u, ins { text-decoration: underline; }
   small { font-size: smaller; }
   big { font-size: larger; }
   mark { background-color: #ffff00; color: black; }
   q:before { content: open-quote; }
   q:after { content: close-quote; }
   ruby, rt { display: inline; }
   ruby > rt { font-size: 50%; }
   rp { display: none; }
   sub { vertical-align: sub; font-size: smaller; }
   sup { vertical-align: super; font-size: smaller; }
   a:link { color: #0000ee; text-decoration: underline; }
   code, kbd, samp, tt, pre, listing, xmp, plaintext { font-family: monospace; font-size: 13px; }
   pre, listing, xmp, plaintext { display: block; white-space: pre; }
   abbr[title], acronym[title] { text-decoration: underline dotted; }
   nobr { white-space: nowrap; }
   marquee { display: inline-block; width: 100%; white-space: nowrap; overflow: hidden; }
   table { display: table; border-spacing: 2px; }
   caption { display: table-caption; text-align: center; }
   thead { display: table-header-group; }
   tbody { display: table-row-group; }
   tfoot { display: table-footer-group; }
   tr { display: table-row; }
   td, th { display: table-cell; padding: 1px; vertical-align: middle; }
   th { text-align: center; }
   fieldset { margin-left: 2px; margin-right: 2px; border: 2px groove #efefef;
              padding: 0.35em 0.75em 0.625em; }
   legend { padding-left: 2px; padding-right: 2px; }
   summary { display: list-item; list-style-type: disclosure-closed; list-style-position: inside; }
   details[open] > summary { list-style-type: disclosure-open; }
   search { display: block; }
   output { display: inline; }
   datalist { display: none; }
   slot { display: contents; }
   dialog { display: none; position: absolute; border: 3px solid #000; padding: 1em; background: #fff; }
   dialog[open] { display: block; }
   [popover] { position: fixed; display: none; background: #fff; border: 3px solid; padding: 0.25em; }
   img, video, canvas, svg, input, select, textarea, iframe { display: inline-block; }
   iframe { width: 300px; height: 150px; border: 2px solid #9a9a9a; }
   object { display: inline; width: 300px; height: 150px; }
   video, canvas { width: 300px; height: 150px; }
   video { object-fit: contain; }
   audio { display: none; }
   audio[controls] { display: inline-block; width: 300px; height: 54px; background: #f1f3f4; border-radius: 27px; }
   progress { display: inline-block; width: 160px; height: 16px; vertical-align: -0.2em; }
   meter { display: inline-block; width: 80px; height: 16px; vertical-align: -0.2em; }
   hr { display: block; margin-top: 0.5em; margin-bottom: 0.5em; border: 1px inset #808080; }
   center { text-align: center; }
   button, input, select, textarea { font-family: sans-serif; font-size: smaller; }
   button { display: inline-block; background: #efefef; border: 2px outset #efefef; padding: 1px 6px; }
   input { background: #fff; box-sizing: border-box; border: 1px solid #767676;
     padding: 1px 2px; width: 177px; height: 21px; }
   input[type=hidden] { display: none; }
   input[type=submit], input[type=reset], input[type=button] {
     background: #efefef; border: 2px outset #efefef; padding: 1px 6px; width: auto; height: auto; }
   input[type=file] { width: 253px; height: 21px; padding: 0; border: none; }
   input[type=image] { border: none; padding: 0; width: auto; height: auto; background: transparent; }
   input[type=range] { width: 129px; height: 16px; padding: 0; border: none; margin-left: 2px; margin-right: 2px; }
   input[type=checkbox] { margin: 3px 3px 3px 4px; }
   input[type=radio] { margin: 3px 3px 0px 5px; }
   input[type=color] { box-sizing: border-box; width: 50px; height: 27px; padding: 1px 2px;
     border: 1px solid #767676; background: #efefef; }
   input[type=time] { box-sizing: border-box; width: 103px; height: 24px; padding: 0 0 0 4px;
     border: 1px solid #767676; font-family: monospace; }
   input[type=date], input[type=month], input[type=week], input[type=datetime-local] {
     box-sizing: border-box; width: auto; height: 23px; padding: 0 0 0 4px;
     border: 1px solid #767676; font-family: monospace; }
   select { background: #efefef; border: 1px solid #767676; padding: 0; }
   textarea { font-family: monospace; background: #fff; border: 1px solid #767676; padding: 2px; width: 175px; height: 34px; white-space: pre-wrap; overflow: auto; }
   button:disabled {
     color: rgba(16, 16, 16, 0.3);
     background-color: rgba(239, 239, 239, 0.3);
     border-top-color: rgba(118, 118, 118, 0.3);
     border-right-color: rgba(118, 118, 118, 0.3);
     border-bottom-color: rgba(118, 118, 118, 0.3);
     border-left-color: rgba(118, 118, 118, 0.3);
   }
   input:disabled, textarea:disabled {
     color: rgb(84, 84, 84);
     background-color: rgba(239, 239, 239, 0.3);
     border-top-color: rgba(118, 118, 118, 0.3);
     border-right-color: rgba(118, 118, 118, 0.3);
     border-bottom-color: rgba(118, 118, 118, 0.3);
     border-left-color: rgba(118, 118, 118, 0.3);
   }
   select:disabled { color: rgb(128, 128, 128); background-color: #efefef; opacity: 0.7; }")

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

(defun layout-html (html &key (width 1024) (height 768) (stylesheet "") (time 0d0) base)
  "Parse HTML, cascade CSS, and lay the document out. Returns the root box."
  (let* ((*viewport-width* (float width 1d0))
         (*viewport-height* (float height 1d0))
         (*root-font-size* 16d0)
         (source (or html ""))
         (doc (import-document (html5-parser:parse-html5 source)))
         (html-el (or (find-element doc "html")
                      (first (element-children doc))))
         (sheets (list (parse-stylesheet *ua-css* :origin :ua :width width :height height))))
    (unless html-el
      (return-from layout-html nil))
    (let ((*document-base* (effective-base doc (or base *document-base*))))
      (let ((*pending-navigation* nil))
        (cond
          (*script-evaluator* (note-scripts html-el))
          (t (js-execute-document doc)))
        (when (and *navigation-enabled* *pending-navigation* (< *navigation-depth* 4))
          (let* ((abs (or (resolve-url *pending-navigation* *document-base*)
                          *pending-navigation*))
                 (next-html (fetch-css-text abs))
                 (from-html source)
                 (from-base *document-base*))
            (when (and next-html (plusp (length next-html)))
              (return-from layout-html
                (multiple-value-bind (root new-base more)
                    (let ((*navigation-depth* (1+ *navigation-depth*)))
                      (layout-html next-html :width width :height height
                                   :stylesheet stylesheet :time time :base abs))
                  (let ((from-root (let ((*navigation-enabled* nil))
                                     (layout-html from-html :width width :height height
                                                  :stylesheet stylesheet :time time
                                                  :base from-base))))
                    (values root new-base
                            (append (list (list from-root from-base)) more)))))))))
      (when (and stylesheet (plusp (length stylesheet)))
        (setf sheets (append sheets
                             (list (parse-stylesheet stylesheet :origin :author
                                                     :width width :height height
                                                     :base *document-base*)))))
      (let ((authored nil))
        (map-elements doc (lambda (el)
                            (cond
                              ((string= (dom-element-tag el) "style")
                               (push (list :style (dom-text-content el)) authored))
                              ((and (string= (dom-element-tag el) "link")
                                    (search "stylesheet" (or (dom-attr el "rel") "")
                                            :test #'char-equal))
                               (push (list :link (dom-attr el "href")) authored)))))
        (dolist (item (nreverse authored))
          (let ((text (cond
                        ((eq (first item) :style) (second item))
                        ((second item)
                         (let ((abs (resolve-url (second item) *document-base*)))
                           (fetch-css-text abs)))
                        (t ""))))
            (when (and text (plusp (length text)))
              (let ((sheet-base (if (eq (first item) :link)
                                    (url-directory (or (resolve-url (second item) *document-base*)
                                                       ""))
                                    *document-base*)))
                (setf sheets (append sheets
                                     (list (parse-stylesheet text :origin :author
                                                             :width width :height height
                                                             :base sheet-base)))))))))
      (multiple-value-bind (rules frames) (collect-rules sheets)
        (cascade-tree html-el rules)
        (apply-animations html-el frames (float time 1d0))
        (values (layout-document html-el) *document-base* nil)))))

(defun render-html (html &key (width 1024) (height 768) (stylesheet "")
                           (output nil) (time 0d0) (scroll-y 0d0) base)
  "Lay HTML out and, when OUTPUT is a pathname, paint it to a PNG with Cairo.
Returns the root box. Cairo is the cross-platform rasterizer; the same paint
path targets an image surface here and a window surface on each OS.
BASE is the document URL. A <base href> in the document overrides it."
  (multiple-value-bind (root doc-base)
      (layout-html html :width width :height height
                   :stylesheet stylesheet :time time :base base)
    (let ((*document-base* doc-base))
      (when (and root output)
        (let* ((doc-h (max height
                           (ceiling (+ (box-y root) (box-h root) 8))))
               (png-h (if (> scroll-y 0d0) height (min doc-h 8192))))
          (paint-png root output width png-h :scroll-y scroll-y)))
      root)))

(defun submit-form (root x y scroll base width height)
  "If X,Y hits a submit control, GET or POST the form and lay out the response.
Returns the new root and the request URL, or NIL."
  (let* ((yy (+ (float y 1d0) (float scroll 1d0)))
         (xx (float x 1d0))
         (submit nil))
    (labels ((walk (box)
               (when (and box
                          (<= (box-x box) xx (+ (box-x box) (box-w box)))
                          (<= (box-y box) yy (+ (box-y box) (box-h box)))
                          (> (box-w box) 0d0) (> (box-h box) 0d0))
                 (when (submit-control-p (box-element box))
                   (setf submit (box-element box)))
                 (dolist (c (box-children box)) (walk c)))))
      (walk root))
    (let ((form (and submit (form-ancestor submit))))
      (when form
        (multiple-value-bind (method url body) (form-request form submit base)
          (let ((html (if (eq method :post)
                          (fetch-post url body)
                          (fetch-css-text url))))
          (when (and html (plusp (length html)))
            (multiple-value-bind (root final departed)
                (layout-html html :width width :height height :base url)
              (when root
                (values root (or final url) departed))))))))))

(defun activate-hit (root x y scroll base width height)
  (multiple-value-bind (root2 base2 departed) (submit-form root x y scroll base width height)
    (if root2
        (values root2 base2 departed)
        (follow-link root x y scroll base width height))))

(defun follow-link (root x y scroll base width height)
  "If X,Y hits an anchor, fetch its href in Lisp and lay the target out.
Returns the new root and the absolute base URL, or NIL."
  (let* ((hit (hit-test root x y scroll))
         (href (link-href hit)))
    (when href
      (let* ((abs (or (resolve-url href base) href))
             (html (fetch-css-text abs)))
        (when (and html (plusp (length html)))
          (multiple-value-bind (root final departed)
              (layout-html html :width width :height height :base abs)
            (when root
              (values root (or final abs) departed))))))))

(setf *activate-link* #'activate-hit)
