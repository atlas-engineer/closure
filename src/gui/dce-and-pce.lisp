;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUE-GUI2; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Various Caches for Images et al
;;;   Created: 1999-05-25
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999,2001 by Gilbert Baumann

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(in-package :CLUE-GUI2)

;;  2001-11-17  GB      - restructured DCACHE

;;  1999-08-21  GB      - REDRAW-NEW-LAZY-IMAGE: changed according to new 
;;                        display list layout

;;; ---------------------------------------------------------------------------

(defstruct dce
  url
  presentation
  data
  lock)

;;;

(defun dcache-get (document url presentation
                            &key lazy-p callback)
  (let ((url (if (url:url-p url) (url:unparse-url url) url)))
    (let* ((dce
            (mp/with-lock (*dcache-lock*)
              (or
               (find-if (lambda (el)
                          (and (equal (dce-url el) url)
                               (eq (dce-presentation el) presentation)))
                        *dcache*)
               (let ((new-dce (make-dce :url url
                                        :presentation presentation
                                        :data :work-in-progress
                                        :lock (mp/make-lock :name "dce lock")))
                     (flag nil))
                 (r2::run-process-on-behalf-of-document
                  document
                  (lambda ()
                    (mp/with-lock ((dce-lock new-dce))
                      (setf flag t)
                      (setf (dce-data new-dce)
                            (dcache-generate-presentation presentation document url)) )))
                 (mp/process-wait "foo"
                                  (lambda () flag))
                 (push new-dce *dcache*)
                 new-dce)))))
      (if lazy-p
          (progn
            (r2::run-process-on-behalf-of-document
             document
             (lambda ()
               (funcall callback
                        (mp/with-lock ((dce-lock dce))
                          (dce-data dce)))))
            nil)
          (mp/with-lock ((dce-lock dce))
            (dce-data dce)) ))))

(defmethod dcache-generate-presentation ((presentation (eql :aimage)) document url)
  (handler-case (aimage-from-url-real document url)
                (error ()
                       :error)))

(defun aimage-from-url-real (document url)
  (let ((res (r2::url->aimage-real document url nil)))
    (when res
      (setf (getf (imagelib:aimage-plist res) :url)
        url))
    (or res :error)))

(defun aimage-from-url (document url)
  (dcache-get document url :aimage))

(defun aimage-from-url-lazy (document url callback)
  (dcache-get document url :aimage :lazy-p t :callback callback))

(defun my-ignore-errors-fn (continuation)
  (multiple-value-bind (res condition)
      (ignore-errors (funcall continuation))
    (when condition
      (format T "~&Error in background process:~%~A" condition)
      (describe condition *standard-output*))
    res))

(defmacro my-ignore-errors (&rest body)
  `(my-ignore-errors-fn (lambda ()
                          ,@body)))

(defstruct pce
  aimage
  width
  height
  pixmap
  refcount)

(defun make-pixmap-from-aimage (drawable aimage width height)
  (dolist (k *pixmap-cache*
            (let ((res  (really-make-pixmap-from-aimage
                         drawable aimage width height)))
              (when *debug-pixmap-cache-p*
                (format T "~&;; ++ [init] ~A ~Dx~D "
                        (getf (imagelib:aimage-plist aimage) :url)
                        width 
                        height))
              (push (make-pce :aimage aimage
                              :width width
                              :height height
                              :pixmap res
                              :refcount 1)
                    *pixmap-cache*)
              res))
    (when (and (eq (pce-aimage k) aimage)
               (eql (pce-width k) width)
               (eql (pce-height k) height))
      (when *debug-pixmap-cache-p*
        (format T "~&;; ++ ~A ~Dx~D "
                (getf (imagelib:aimage-plist aimage) :url)
                width 
                height))
      (incf (pce-refcount k))
      (return (pce-pixmap k)))))

(defun really-make-pixmap-from-aimage (drawable aimage width height)
  (multiple-value-list
      (gui::aimage->pixmap+mask/raw drawable
                                    (imagelib:scale-aimage aimage width height))))

(defun reset-caches ()
  (setf *dcache* nil
        *pixmap-cache* nil))

(defun ws/x11::aimage->pixmap+mask (drawable aimage)
  (make-pixmap-from-aimage drawable aimage 
                           (imagelib:aimage-width aimage)
                           (imagelib:aimage-height aimage)))

(defclass r2::ro/img ()
  ((url                    :initarg :url)
   (aim-orig :initform nil :initarg :aim-orig)
   (aim      :initform nil)
   (width    :initform nil)
   (height   :initform nil)
   (pixmap   :initform nil)
   (mask     :initform nil)))

(defmethod print-object ((self r2::ro/img) sink)
  (format sink "#<~S url=~S>" (type-of self) 
          (if (slot-boundp self 'url)
              (slot-value self 'url)
            :unbound)))

(defmethod deconstruct-robj ((self r2::ro/img))
  (with-slots ((aim-orig aim-orig) (pixmap pixmap) (mask mask)) self
    (when pixmap
      (deref-aimage-pixmap aim-orig (list pixmap mask))
      (setf pixmap nil
            mask nil))))

(defun deref-aimage-pixmap (aimage pixmap)
  (let ((pce (find pixmap *pixmap-cache* :key #'pce-pixmap :test #'equal)))
    (assert (not (null pce)))
    (assert (> (pce-refcount pce) 0))
    (when *debug-pixmap-cache-p*
      (format T "~&;; -- ~A ~Dx~D "
              (getf (imagelib:aimage-plist (pce-aimage pce)) :url)
              (pce-width pce)
              (pce-height pce)))
    (decf (pce-refcount pce))))

(defun flush-pixmap-cache ()
  (let ((n 0))
    (setf *pixmap-cache*
      (mapcan (lambda (pce)
                (cond ((eql (pce-refcount pce) 0)
                       (and (car (pce-pixmap pce))
                            (incf n (* (xlib:drawable-width (car (pce-pixmap pce))) 
                                       (xlib:drawable-height (car (pce-pixmap pce)))))
                            (xlib:free-pixmap (car (pce-pixmap pce))))
                       (and (cadr (pce-pixmap pce))
                            (incf n (* (xlib:drawable-width (cadr (pce-pixmap pce))) 
                                       (xlib:drawable-height (cadr (pce-pixmap pce)))))
                            (xlib:free-pixmap (cadr (pce-pixmap pce))))
                     
                       nil)
                      (t
                       (list pce))))
              *pixmap-cache*))
    n))

;;;;;

(defmethod r2::ro/intrinsic-size ((self r2::ro/img)) ;; -> width; height; depth
  (with-slots (aim-orig) self
    (values (r2::aimage-width aim-orig) (r2::aimage-height aim-orig) 0)))

(defmethod r2::ro/size ((self r2::ro/img))      ;; -> width; height; depth
  (with-slots (width height) self
    (assert (and width height))
    (values width height 0)))

(defmethod r2::ro/resize ((self r2::ro/img) new-width new-height)
  (with-slots (width height aim aim-orig pixmap mask) self
    (cond ((and new-width new-height)
           (setf width (round new-width)
                 height (round new-height)) )
          (t
           (let ((ow (r2::aimage-width aim-orig))
                 (oh (r2::aimage-height aim-orig)))
             ;;
             (when (and (not new-width) (not new-height))
               (setf new-width ow
                     new-height oh))
             (when (not new-width)
               (setf new-width (* new-height (/ ow oh))))
             (when (not new-height)
               (setf new-height (* new-width (/ oh ow))))
             ;; damit da mal nix passiert:
             (setf new-width (max 1 (round new-width)))
             (setf new-height (max 1 (round new-height)))
             (unless (and (eql new-width width) (eql new-height height))
               (setf width new-width
                     height new-height
                     pixmap nil mask nil
                     aim nil #+(OR) (if aim (imagelib:scale-aimage aim-orig new-width new-height) nil) ))))) ))

(defun ensure-ro/img-pixmap (drawable self)
  (with-slots (aim-orig width height pixmap mask) self
    (when aim-orig
      (unless pixmap
        (let ((r (make-pixmap-from-aimage drawable aim-orig width height)))
          (setf pixmap (car r)
                mask   (cadr r)))))))

(defmethod r2::x11-draw-robj (drawable gcontext (self r2::ro/img) box x y)
  (declare (ignore box))
  (setq x (round x))
  (setq y (round y))
  (with-slots ((aim-orig aim-orig) (width width) (height height)
               (pixmap pixmap)
               (mask   mask)) 
      self
    (ensure-ro/img-pixmap drawable self)
    (when aim-orig
      (cond ((not (null mask))
             (xlib:with-gcontext (gcontext :clip-mask mask
                                           :clip-x x
                                           :clip-y (- y height))
               (xlib:copy-area pixmap gcontext 0 0 width height
                               drawable x (- y height))) )
            (t
             (xlib:copy-area pixmap gcontext 0 0 width height
                             drawable x (- y height) ))))))


;;; ----------------------------------------------------------------------------------------------------

(defclass ro/lazy-img (r2::ro/img)
  ()
  )

(defmethod gui::make-image-replacement ((device clue-device)
                                        document
                                        &key url width height)
  (cond ((and width height)
         (make-lazy-image-replacement device document url width height))
        (t
         (let ((aim (r2::url->aimage document url)))
           (cond ((eql aim :error)
                  (make-instance 'r2::ro/img
                    :url url
                    :aim-orig (renderer::broken-aimage document)))
                 (t
                  (make-instance 'r2::ro/img
                    :url url
                    :aim-orig aim) ))))))

(defmethod redraw-new-lazy-image (document htview ro)
  ;; ro - the image r2::robj
  ;; seek for the containing abox
  (when (r2::document-display-list document) ; display-list there already?
    (dolist (k (gui:display-list-items (r2::document-display-list document)))
      ;; grrf. find the parent of this robj
      (r2::map-boxen (lambda (box)
                       (when (and (r2::abox-p box) 
                                  (find-if (lambda (x)
                                             (and (r2::rbox-p x)
                                                  (eq (r2::rbox-obj x) ro)))
                                           (r2::abox-contents box)) )
                         (multiple-value-bind (x0 y0 x1 y1)
                             (values (r2::abox-bx0 box) (r2::abox-by0 box) 
                                     (r2::abox-bx1 box) (r2::abox-by1 box))
                           (cond ((and x0 y0 x1 y1)
                                  ;; queue a redraw
                                  ;; this could be optimized further, it is enough to
                                  ;; redraw only this box.
                                  (gui-post document
                                            'clue:display
                                            htview
                                            x0 y0
                                            (- x1 x0) (- y1 y0) ))
                                 (t
                                  (warn "Hugh?! a box without a bounding box?")) ))))
                     k))))

(defmethod make-lazy-image-replacement (device document url width height)
  (let ((ro (make-instance 'r2::ro/img :url url)))
    (let ((aim (aimage-from-url-lazy 
                document 
                url
                ;; callback, called when the image is ready:
                #'(lambda (value)
                    (if (eq value :error)
                        (setf (slot-value ro 'aim-orig)
                          (renderer::broken-aimage document))
                      (setf (slot-value ro 'aim-orig) value))
                    (redraw-new-lazy-image document (slot-value device 'htview) ro) ))))
      (setf (slot-value ro 'aim-orig)
        (if (eq aim :error)
            (r2::broken-aimage document)
          aim))
      ro)))

#||
(defmethod make-lazy-image-replacement (device document url width height)
  (let ((ro (make-instance 'r2::ro/img :url url)))
    (setf (slot-value ro 'aim-orig) nil) ;erstmal gar nichts
    ;; rest asynchron
    (let ((aim (aimage-from-url-lazy 
                document 
                url
                ;; callback, called when the image is ready:
                #'(lambda (value)
                    (if (eq value :error)
                        (setf (slot-value ro 'aim-orig)
                              (renderer::broken-aimage document))
                      (setf (slot-value ro 'aim-orig) value))
                    (redraw-new-lazy-image document (slot-value device 'htview) ro) ))))
      (when aim
        (setf (slot-value ro 'aim-orig) aim)))
    ro))
||#