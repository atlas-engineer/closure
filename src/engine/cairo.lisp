;;; Thin Cairo FFI. Cairo is the cross-platform raster backend: the same
;;; calls paint an image surface here and a window surface on X11, Wayland,
;;; Windows, or macOS.

(in-package :closure.engine)

(cffi:define-foreign-library cairo
  (:unix (:or "libcairo.so.2" "libcairo.so"))
  (t (:default "libcairo")))

(cffi:define-foreign-library freetype
  (:unix (:or "libfreetype.so.6" "libfreetype.so"))
  (t (:default "freetype")))

(cffi:defcfun ("cairo_image_surface_create" cairo-image-surface-create)
    :pointer
  (format :int) (width :int) (height :int))

(cffi:defcfun ("cairo_xlib_surface_create" cairo-xlib-surface-create) :pointer
  (display :pointer) (drawable :unsigned-long) (visual :pointer)
  (width :int) (height :int))

(cffi:defcfun ("cairo_create" cairo-create) :pointer (target :pointer))
(cffi:defcfun ("cairo_destroy" cairo-destroy) :void (cr :pointer))
(cffi:defcfun ("cairo_surface_destroy" cairo-surface-destroy) :void (surface :pointer))
(cffi:defcfun ("cairo_surface_write_to_png" cairo-surface-write-to-png) :int
  (surface :pointer) (filename :string))
(cffi:defcfun ("cairo_surface_flush" cairo-surface-flush) :void (surface :pointer))
(cffi:defcfun ("cairo_image_surface_get_data" cairo-image-surface-get-data) :pointer
  (surface :pointer))
(cffi:defcfun ("cairo_image_surface_get_stride" cairo-image-surface-get-stride) :int
  (surface :pointer))
(cffi:defcfun ("cairo_image_surface_create_from_png" cairo-image-surface-create-from-png)
    :pointer
  (filename :string))
(cffi:defcfun ("cairo_image_surface_get_width" cairo-image-surface-get-width) :int
  (surface :pointer))
(cffi:defcfun ("cairo_image_surface_get_height" cairo-image-surface-get-height) :int
  (surface :pointer))
(cffi:defcfun ("cairo_surface_status" cairo-surface-status) :int (surface :pointer))
(cffi:defcfun ("cairo_surface_mark_dirty" cairo-surface-mark-dirty) :void (surface :pointer))
(cffi:defcfun ("cairo_set_source_surface" cairo-set-source-surface) :void
  (cr :pointer) (surface :pointer) (x :double) (y :double))
(cffi:defcfun ("cairo_status" cairo-status) :int (cr :pointer))
(cffi:defcfun ("cairo_status_to_string" cairo-status-to-string) :string (status :int))

(cffi:defcfun ("cairo_save" cairo-save) :void (cr :pointer))
(cffi:defcfun ("cairo_restore" cairo-restore) :void (cr :pointer))
(cffi:defcfun ("cairo_translate" cairo-translate) :void (cr :pointer) (tx :double) (ty :double))
(cffi:defcfun ("cairo_scale" cairo-scale) :void (cr :pointer) (sx :double) (sy :double))
(cffi:defcfun ("cairo_rotate" cairo-rotate) :void (cr :pointer) (radians :double))
(cffi:defcfun ("cairo_push_group" cairo-push-group) :void (cr :pointer))
(cffi:defcfun ("cairo_pop_group_to_source" cairo-pop-group-to-source) :void (cr :pointer))
(cffi:defcfun ("cairo_paint" cairo-paint) :void (cr :pointer))
(cffi:defcfun ("cairo_paint_with_alpha" cairo-paint-with-alpha) :void (cr :pointer) (alpha :double))

(cffi:defcfun ("cairo_set_source_rgba" cairo-set-source-rgba) :void
  (cr :pointer) (r :double) (g :double) (b :double) (a :double))
(cffi:defcfun ("cairo_pattern_create_linear" cairo-pattern-create-linear) :pointer
  (x0 :double) (y0 :double) (x1 :double) (y1 :double))
(cffi:defcfun ("cairo_pattern_create_radial" cairo-pattern-create-radial) :pointer
  (cx0 :double) (cy0 :double) (r0 :double)
  (cx1 :double) (cy1 :double) (r1 :double))
(cffi:defcfun ("cairo_pattern_add_color_stop_rgba" cairo-pattern-add-color-stop-rgba) :void
  (pattern :pointer) (offset :double) (r :double) (g :double) (b :double) (a :double))
(cffi:defcfun ("cairo_set_source" cairo-set-source) :void
  (cr :pointer) (source :pointer))
(cffi:defcfun ("cairo_pattern_destroy" cairo-pattern-destroy) :void
  (pattern :pointer))
(cffi:defcfun ("cairo_pattern_set_extend" cairo-pattern-set-extend) :void
  (pattern :pointer) (extend :int))
(cffi:defcfun ("cairo_rectangle" cairo-rectangle) :void
  (cr :pointer) (x :double) (y :double) (w :double) (h :double))
(cffi:defcfun ("cairo_fill" cairo-fill) :void (cr :pointer))
(cffi:defcfun ("cairo_fill_preserve" cairo-fill-preserve) :void (cr :pointer))
(cffi:defcfun ("cairo_stroke" cairo-stroke) :void (cr :pointer))
(cffi:defcfun ("cairo_new_path" cairo-new-path) :void (cr :pointer))
(cffi:defcfun ("cairo_new_sub_path" cairo-new-sub-path) :void (cr :pointer))
(cffi:defcfun ("cairo_close_path" cairo-close-path) :void (cr :pointer))
(cffi:defcfun ("cairo_arc" cairo-arc) :void
  (cr :pointer) (xc :double) (yc :double) (radius :double) (a1 :double) (a2 :double))
(cffi:defcfun ("cairo_set_line_width" cairo-set-line-width) :void (cr :pointer) (w :double))
(cffi:defcfun ("cairo_set_dash" cairo-set-dash) :void
  (cr :pointer) (dashes :pointer) (n :int) (offset :double))
(cffi:defcfun ("cairo_clip" cairo-clip) :void (cr :pointer))
(cffi:defcfun ("cairo_reset_clip" cairo-reset-clip) :void (cr :pointer))

(cffi:defcfun ("cairo_select_font_face" cairo-select-font-face) :void
  (cr :pointer) (family :string) (slant :int) (weight :int))
(cffi:defcfun ("cairo_set_font_size" cairo-set-font-size) :void (cr :pointer) (size :double))
(cffi:defcfun ("cairo_font_options_create" cairo-font-options-create) :pointer)
(cffi:defcfun ("cairo_font_options_set_hint_metrics" cairo-font-options-set-hint-metrics) :void
  (options :pointer) (metrics :int))
(cffi:defcfun ("cairo_font_options_set_hint_style" cairo-font-options-set-hint-style) :void
  (options :pointer) (style :int))
(cffi:defcfun ("cairo_set_font_options" cairo-set-font-options) :void
  (cr :pointer) (options :pointer))
(cffi:defcfun ("cairo_get_scaled_font" cairo-get-scaled-font) :pointer
  (cr :pointer))
(cffi:defcfun ("cairo_ft_scaled_font_lock_face" cairo-ft-lock-face) :pointer
  (scaled :pointer))
(cffi:defcfun ("cairo_ft_scaled_font_unlock_face" cairo-ft-unlock-face) :void
  (scaled :pointer))
(cffi:defcfun ("FT_Get_Char_Index" ft-get-char-index) :unsigned-int
  (face :pointer) (char :unsigned-long))
(cffi:defcfun ("FT_Get_Kerning" ft-get-kerning) :int
  (face :pointer) (left :unsigned-int) (right :unsigned-int) (mode :int) (kerning :pointer))

(cffi:defcstruct ft-vector
  (x :long)
  (y :long))
(cffi:defcfun ("cairo_move_to" cairo-move-to) :void (cr :pointer) (x :double) (y :double))
(cffi:defcfun ("cairo_line_to" cairo-line-to) :void (cr :pointer) (x :double) (y :double))
(cffi:defcfun ("cairo_curve_to" cairo-curve-to) :void
  (cr :pointer) (x1 :double) (y1 :double) (x2 :double) (y2 :double) (x3 :double) (y3 :double))
(cffi:defcfun ("cairo_show_text" cairo-show-text) :void (cr :pointer) (utf8 :string))
(cffi:defcfun ("cairo_text_extents" cairo-text-extents) :void
  (cr :pointer) (utf8 :string) (extents :pointer))
(cffi:defcfun ("cairo_font_extents" cairo-font-extents) :void
  (cr :pointer) (extents :pointer))

(cffi:defcstruct cairo-text-extents
  (x-bearing :double)
  (y-bearing :double)
  (width :double)
  (height :double)
  (x-advance :double)
  (y-advance :double))

(cffi:defcstruct cairo-font-extents
  (ascent :double)
  (descent :double)
  (height :double)
  (max-x-advance :double)
  (max-y-advance :double))

(defvar *cairo* nil)
(defvar *cairo-surface* nil)
(defvar *measure-cache* (make-hash-table :test #'equal)
  "Case-sensitive. EQUALP would treat W and w as the same advance.")

(defun ensure-cairo ()
  (unless (and *cairo* (not (cffi:null-pointer-p *cairo*)))
    (cffi:use-foreign-library freetype)
    (cffi:use-foreign-library cairo)
    (setf *cairo-surface* (cairo-image-surface-create 0 8 8))
    (setf *cairo* (cairo-create *cairo-surface*))
    (when (cffi:null-pointer-p *cairo*)
      (error "Cairo did not create a drawing context.")))
  *cairo*)

(defvar *sans-face* nil
  "Concrete family for the generic sans-serif, chosen so bold is a different face.")

(defvar *serif-face* nil
  "Concrete family for the generic serif. Browsers default to a Times-like face.")

(defvar *resolving-sans* nil)

(defun cairo-family (family)
  (let ((name (string-downcase
               (cond ((stringp family) family)
                     ((and (consp family) (stringp (car family))) (car family))
                     ((and (consp family) (symbolp (car family)))
                      (string-downcase (symbol-name (car family))))
                     ((symbolp family) (string-downcase (symbol-name family)))
                     (t "sans-serif")))))
    (cond ((member name '("serif" "times") :test #'string=)
           (if *resolving-sans* "Serif" (resolve-serif-face)))
          ((member name '("monospace" "courier" "courier new" "ui-monospace") :test #'string=)
           "Monospace")
          ((member name '("sans-serif" "sans" "arial" "helvetica" "noto sans") :test #'string=)
           (if *resolving-sans* "Sans" (resolve-sans-face)))
          ((plusp (length name)) name)
          (t (if *resolving-sans* "Sans" (resolve-sans-face))))))

(defun cairo-slant (style)
  (if (member style '(:italic :oblique)) 1 0))

(defun cairo-weight (weight)
  (cond ((member weight '(:bold :bolder)) 1)
        ((and (numberp weight) (>= weight 600)) 1)
        (t 0)))

(defvar *font-options* nil)

(defun unhinted-font-options ()
  "Fractional advances. Hinted metrics round to whole pixels and disagree
with the browser's Times widths."
  (ensure-cairo)
  (unless *font-options*
    (setf *font-options* (cairo-font-options-create))
    ;; CAIRO_HINT_METRICS_OFF and CAIRO_HINT_STYLE_NONE.
    (cairo-font-options-set-hint-metrics *font-options* 1)
    (cairo-font-options-set-hint-style *font-options* 1))
  *font-options*)

(defun select-cairo-font (cr family slant weight size)
  (cairo-set-font-options cr (unhinted-font-options))
  (cairo-select-font-face cr family slant weight)
  (cairo-set-font-size cr (float size 1d0)))

(defvar *kern-cache* (make-hash-table :test #'equal))

(defun resolved-slant (style)
  (cond ((member style '(:italic :oblique)) 1)
        ((eql style 1) 1)
        (t 0)))

(defun resolved-weight (weight)
  (cond ((member weight '(:bold :bolder)) 1)
        ((and (numberp weight) (or (eql weight 1) (>= weight 600))) 1)
        (t 0)))

(defvar *kern-cr* nil)

(defun kern-context ()
  "A context used only to read kerning, so locking its face cannot
disturb the context that measures text."
  (ensure-cairo)
  (unless (and *kern-cr* (not (cffi:null-pointer-p *kern-cr*)))
    (setf *kern-cr* (cairo-create *cairo-surface*)))
  *kern-cr*)

(defun pair-kern (left right size family weight style)
  "Pixels to add between LEFT and RIGHT. Negative tucks the second glyph in."
  (let* ((cr (kern-context))
         (slant (resolved-slant style))
         (heavy (resolved-weight weight))
         (key (list (char-code left) (char-code right)
                    (float size 1d0)
                    (cairo-family family)
                    heavy slant)))
    (or (gethash key *kern-cache*)
        (setf (gethash key *kern-cache*)
              (progn
                (select-cairo-font cr (cairo-family family) slant heavy size)
                (let* ((scaled (cairo-get-scaled-font cr))
                       (face (and scaled (not (cffi:null-pointer-p scaled))
                                  (cairo-ft-lock-face scaled))))
                  (unwind-protect
                      (if (or (null face) (cffi:null-pointer-p face))
                          0d0
                          (cffi:with-foreign-object (vec '(:struct ft-vector))
                            (if (zerop (ft-get-kerning face
                                                       (ft-get-char-index face (char-code left))
                                                       (ft-get-char-index face (char-code right))
                                                       2 vec)) ; FT_KERNING_UNSCALED, font units
                                (let ((units (cffi:mem-ref face :unsigned-short 136)))
                                  (if (and (>= units 16) (<= units 16384))
                                      (/ (* (float (cffi:foreign-slot-value vec '(:struct ft-vector) 'x) 1d0)
                                            (float size 1d0))
                                         units)
                                      0d0))
                                0d0)))
                    (when (and face (not (cffi:null-pointer-p face)))
                      (cairo-ft-unlock-face scaled)))))))))

(defun kerning-sum (text size family weight style)
  (if (or (null text) (< (length text) 2))
      0d0
      (loop for i from 1 below (length text)
            sum (pair-kern (char text (1- i)) (char text i)
                           size family weight style))))

(defun snap-layout-unit (px)
  "Nearest 1/64px, with a half-unit rounding away from zero. That is the
unit a browser stores a used width in."
  (let ((scaled (* px 64d0)))
    (/ (float (truncate (+ scaled (if (minusp scaled) -0.5d0 0.5d0))) 1d0)
       64d0)))

(defun %measure-uncached (string size family weight slant)
  (let ((cr (ensure-cairo)))
    (select-cairo-font cr (cairo-family family) slant
                       (if (and (numberp weight) (>= weight 600)) 1 weight)
                       size)
    (cffi:with-foreign-objects ((te '(:struct cairo-text-extents))
                                (fe '(:struct cairo-font-extents)))
      (cairo-font-extents cr fe)
      (let ((asc (cffi:foreign-slot-value fe '(:struct cairo-font-extents) 'ascent))
            (desc (cffi:foreign-slot-value fe '(:struct cairo-font-extents) 'descent))
            (height (cffi:foreign-slot-value fe '(:struct cairo-font-extents) 'height)))
        (if (or (null string) (zerop (length string)))
            (values 0d0 asc desc height)
            (progn
              (cairo-text-extents cr string te)
              (values (snap-layout-unit
                       (+ (cffi:foreign-slot-value te '(:struct cairo-text-extents) 'x-advance)
                          (kerning-sum string size family
                                       (if (and (numberp weight) (>= weight 600)) 1 weight)
                                       slant)))
                      asc desc height)))))))

(defun measure-text (string size family weight style)
  "Return (values advance ascent descent) in CSS pixels."
  (let* ((slant (cairo-slant style))
         (w (cairo-weight weight))
         (key (list string (float size 1d0) (cairo-family family) w slant)))
    (or (gethash key *measure-cache*)
        (setf (gethash key *measure-cache*)
              (multiple-value-list (%measure-uncached string size family w slant))))))

(defun text-advance (string size family weight style)
  (first (measure-text string size family weight style)))

(defun resolve-serif-face ()
  "Pick a Times-like serif whose bold face is wider than its regular face."
  (unless *serif-face*
    (let ((*resolving-sans* t))
      (setf *serif-face*
            (or (find-if (lambda (name)
                           (let ((regular (text-advance "bold" 16d0 name :normal :normal))
                                 (heavy (text-advance "bold" 16d0 name :bold :normal)))
                             (> heavy regular)))
                         '("Times New Roman" "Liberation Serif" "Times"))
                "Times New Roman"))))
  *serif-face*)

(defun resolve-sans-face ()
  "Pick a sans family whose bold face is wider than its regular face."
  (unless *sans-face*
    (let ((*resolving-sans* t))
      (setf *sans-face*
            (or (find-if (lambda (name)
                           (let ((regular (text-advance "bold" 16d0 name :normal :normal))
                                 (heavy (text-advance "bold" 16d0 name :bold :normal)))
                             (> heavy regular)))
                         '("DejaVu Sans" "Liberation Sans"))
                "Sans"))))
  *sans-face*)

(defun font-height (size family weight style)
  "Normal line height. Each of ascent, descent, and line gap rounds to a
pixel, then they add. That is the spacing browsers use."
  (let* ((m (measure-text "Mg" size family weight style))
         (asc (second m))
         (desc (third m))
         (gap (max 0d0 (- (fourth m) asc desc))))
    (float (+ (round asc) (round desc) (round gap)) 1d0)))

(defun font-ascent (size family weight style)
  "Ascent rounded to a pixel, matching the inline box a browser reports."
  (float (round (second (measure-text "Mg" size family weight style))) 1d0))

(defun font-descent (size family weight style)
  (float (round (third (measure-text "Mg" size family weight style))) 1d0))

(defvar *png-surfaces* (make-hash-table :test #'equal)
  "Decoded PNG surfaces keyed by namestring. Cairo decodes the file; layout and paint stay in Lisp.")

(defun load-png-surface (pathname)
  "Return a cached Cairo image surface for PATHNAME, or NIL if it is not a readable PNG."
  (let ((probed (probe-file pathname)))
    (unless probed
      (return-from load-png-surface nil))
    (ensure-cairo)
    (let* ((name (namestring probed))
           (cached (gethash name *png-surfaces*)))
      (when cached
        (return-from load-png-surface cached))
      (let ((surface (cairo-image-surface-create-from-png name)))
        (cond
          ((or (cffi:null-pointer-p surface)
               (not (zerop (cairo-surface-status surface))))
           (unless (cffi:null-pointer-p surface)
             (cairo-surface-destroy surface))
           nil)
          (t (setf (gethash name *png-surfaces*) surface)))))))

(defun png-intrinsic-size (pathname)
  "Return the pixel width and height of a local PNG, or NIL."
  (let ((surface (load-png-surface pathname)))
    (when surface
      (values (float (cairo-image-surface-get-width surface) 1d0)
              (float (cairo-image-surface-get-height surface) 1d0)))))
