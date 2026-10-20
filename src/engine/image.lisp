;;; Local image loading for the layout engine.
;;; GIF uses the same Lisp reader as src/imagelib/gif.lisp (Skippy).
;;; JPEG is decoded here: src/imagelib/jpeg.lisp shells out to djpeg, and the
;;; engine does not call that. Cairo only receives finished pixels.

(in-package :closure.engine)

(defvar *image-surfaces* (make-hash-table :test #'equal)
  "Decoded GIF and JPEG surfaces keyed by namestring.")

(defun rgb-surface (width height pixel)
  "Build an ARGB32 Cairo surface. PIXEL is called as (x y) and returns R G B A."
  (ensure-cairo)
  (let ((surface (cairo-image-surface-create 0 width height)))
    (when (or (cffi:null-pointer-p surface)
              (not (zerop (cairo-surface-status surface))))
      (unless (cffi:null-pointer-p surface)
        (cairo-surface-destroy surface))
      (return-from rgb-surface nil))
    (let ((stride (cairo-image-surface-get-stride surface))
          (data (cairo-image-surface-get-data surface)))
      (dotimes (y height)
        (dotimes (x width)
          (multiple-value-bind (r g b a) (funcall pixel x y)
            (setf (cffi:mem-ref data :uint32 (+ (* y stride) (* x 4)))
                  (logior (logand b #xff)
                          (ash (logand g #xff) 8)
                          (ash (logand r #xff) 16)
                          (ash (logand (or a 255) #xff) 24)))))))
    (cairo-surface-mark-dirty surface)
    surface))

(defun gif-surface (pathname)
  "Decode a GIF the way imagelib does, without the McCLIM image wrapper."
  (let* ((stream (skippy:load-data-stream pathname))
         (image (skippy:last-image stream))
         (table (or (skippy:color-table image) (skippy:color-table stream)))
         (transparent (skippy:transparency-index image))
         (w (skippy:width image))
         (h (skippy:height image)))
    (rgb-surface w h
                 (lambda (x y)
                   (let ((index (skippy:pixel-ref image x y)))
                     (if (eql index transparent)
                         (values 0 0 0 0)
                         (multiple-value-bind (r g b)
                             (skippy:color-rgb (skippy:color-table-entry table index))
                           (values r g b 255))))))))

(defparameter +jpeg-zigzag+
  #(0  1  8 16  9  2  3 10
    17 24 32 25 18 11  4  5
    12 19 26 33 40 48 41 34
    27 20 13  6  7 14 21 28
    35 42 49 56 57 50 43 36
    29 22 15 23 30 37 44 51
    58 59 52 45 38 31 39 46
    53 60 61 54 47 55 62 63))

(defparameter *jpeg-cos*
  (let ((a (make-array '(8 8) :element-type 'double-float)))
    (dotimes (u 8 a)
      (dotimes (x 8)
        (setf (aref a u x)
              (cos (* (/ pi 16d0) u (float (+ (* 2 x) 1) 1d0))))))))

(define-condition jpeg-marker (condition)
  ((code :initarg :code :reader jpeg-marker-code)))

(defstruct jbits data (i 0) (acc 0) (n 0))

(defun jbit (bits)
  (when (zerop (jbits-n bits))
    (let ((data (jbits-data bits))
          (i (jbits-i bits)))
      (when (>= i (length data))
        (error "Truncated JPEG."))
      (let ((b (aref data i)))
        (incf i)
        (when (= b #xff)
          (when (>= i (length data))
            (error "Truncated JPEG."))
          (let ((n (aref data i)))
            (incf i)
            (unless (zerop n)
              (setf (jbits-i bits) i)
              (error 'jpeg-marker :code n))))
        (setf (jbits-i bits) i
              (jbits-acc bits) b
              (jbits-n bits) 8))))
  (decf (jbits-n bits))
  (ldb (byte 1 (jbits-n bits)) (jbits-acc bits)))

(defun jbits-align (bits)
  (setf (jbits-n bits) 0))

(defun jpeg-huffman (counts symbols)
  (let ((table (make-hash-table :test #'equal))
        (code 0)
        (si 0))
    (loop for len from 1 to 16 do
      (dotimes (k (aref counts (1- len)))
        (setf (gethash (cons len code) table) (aref symbols si))
        (incf si)
        (incf code))
      (setf code (ash code 1)))
    table))

(defun jpeg-symbol (bits table)
  (let ((code 0))
    (loop for len from 1 to 16 do
      (setf code (logior (ash code 1) (jbit bits)))
      (multiple-value-bind (sym ok) (gethash (cons len code) table)
        (when ok (return-from jpeg-symbol sym))))
    (error "Bad JPEG Huffman code.")))

(defun jpeg-receive (bits n)
  (let ((v 0))
    (dotimes (i n v)
      (setf v (logior (ash v 1) (jbit bits))))))

(defun jpeg-extend (v n)
  (if (or (zerop n) (>= v (ash 1 (1- n))))
      v
      (+ v 1 (- (ash 1 n)))))

(defun jpeg-idct (coef)
  "Level-shifted 8x8 samples from dequantized coefficients in natural order."
  (let ((out (make-array 64 :element-type 'double-float)))
    (dotimes (y 8 out)
      (dotimes (x 8)
        (let ((sum 0d0))
          (dotimes (v 8)
            (dotimes (u 8)
              (let ((cu (if (zerop u) #.(/ (sqrt 2d0)) 1d0))
                    (cv (if (zerop v) #.(/ (sqrt 2d0)) 1d0)))
                (incf sum (* cu cv
                             (aref coef (+ (* v 8) u))
                             (aref *jpeg-cos* u x)
                             (aref *jpeg-cos* v y))))))
          (setf (aref out (+ (* y 8) x))
                (max 0d0 (min 255d0 (+ (/ sum 4d0) 128d0)))))))))

(defun jpeg-decode-block (bits dc-table ac-table quant pred)
  "Return (values idct-samples new-dc-predictor)."
  (let ((coef (make-array 64 :element-type 'double-float :initial-element 0d0))
        (dc (+ pred (let ((size (jpeg-symbol bits dc-table)))
                      (if (zerop size)
                          0
                          (jpeg-extend (jpeg-receive bits size) size))))))
    (setf (aref coef 0) (* dc (aref quant 0)))
    (let ((k 1))
      (loop while (< k 64) do
        (let* ((sym (jpeg-symbol bits ac-table))
               (run (ash sym -4))
               (size (logand sym #xf)))
          (cond
            ((and (zerop run) (zerop size))
             (setf k 64))
            ((and (= run 15) (zerop size))
             (incf k 16))
            (t
             (incf k run)
             (when (< k 64)
               (let ((pos (aref +jpeg-zigzag+ k)))
                 (setf (aref coef pos)
                       (* (jpeg-extend (jpeg-receive bits size) size)
                          (aref quant pos)))))
             (incf k))))))
    (values (jpeg-idct coef) dc)))

(defun jpeg-sample (blocks h v px py hmax vmax)
  "Nearest-neighbor sample of one component inside an MCU."
  (let* ((sx (floor (* px h) hmax))
         (sy (floor (* py v) vmax))
         (bx (floor sx 8))
         (by (floor sy 8))
         (blk (aref blocks (+ (* by h) bx))))
    (aref blk (+ (* (mod sy 8) 8) (mod sx 8)))))

(defun jpeg-clamp (v)
  (max 0 (min 255 (round v))))

(defstruct jparse
  data
  (i 0)
  (quant (make-array 4))
  (dc (make-array 4))
  (ac (make-array 4))
  comps
  (width 0)
  (height 0)
  (ri 0))

(defstruct jcomp id h v qt (pred 0))

(defun jp-u8 (p)
  (when (>= (jparse-i p) (length (jparse-data p)))
    (error "Truncated JPEG."))
  (prog1 (aref (jparse-data p) (jparse-i p))
    (incf (jparse-i p))))

(defun jp-u16 (p)
  (+ (ash (jp-u8 p) 8) (jp-u8 p)))

(defun jp-marker (p)
  (unless (= (jp-u8 p) #xff)
    (error "JPEG marker expected."))
  (loop for b = (jp-u8 p) while (= b #xff) finally (return b)))

(defun jpeg-find-comp (p id)
  (find id (jparse-comps p) :key #'jcomp-id))

(defun jpeg-read-dqt (p end)
  (loop while (< (jparse-i p) end) do
    (let* ((info (jp-u8 p))
           (q (make-array 64 :element-type 'double-float)))
      (dotimes (k 64)
        (setf (aref q (aref +jpeg-zigzag+ k))
              (float (if (plusp (ash info -4)) (jp-u16 p) (jp-u8 p)) 1d0)))
      (setf (aref (jparse-quant p) (logand info #xf)) q))))

(defun jpeg-read-dht (p end)
  (loop while (< (jparse-i p) end) do
    (let* ((info (jp-u8 p))
           (counts (make-array 16 :element-type 'fixnum))
           (nsym 0))
      (dotimes (k 16)
        (setf (aref counts k) (jp-u8 p))
        (incf nsym (aref counts k)))
      (let ((symbols (make-array nsym)))
        (dotimes (k nsym)
          (setf (aref symbols k) (jp-u8 p)))
        (setf (aref (if (zerop (ash info -4)) (jparse-dc p) (jparse-ac p))
                    (logand info #xf))
              (jpeg-huffman counts symbols))))))

(defun jpeg-read-sof (p)
  (jp-u8 p)
  (setf (jparse-height p) (jp-u16 p)
        (jparse-width p) (jp-u16 p))
  (let ((nc (jp-u8 p))
        (comps nil))
    (dotimes (k nc)
      (let ((id (jp-u8 p))
            (hv (jp-u8 p))
            (qt (jp-u8 p)))
        (push (make-jcomp :id id :h (ash hv -4) :v (logand hv #xf) :qt qt) comps)))
    (setf (jparse-comps p) (nreverse comps))))

(defun jpeg-mcu-pixel (decoded px py hmax vmax)
  (flet ((at (rec)
           (destructuring-bind (c blocks) rec
             (jpeg-sample blocks (jcomp-h c) (jcomp-v c) px py hmax vmax))))
    (if (= (length decoded) 1)
        (let ((g (jpeg-clamp (at (first decoded)))))
          (logior g (ash g 8) (ash g 16) (ash 255 24)))
        (let* ((yy (at (first decoded)))
               (cb (- (at (second decoded)) 128d0))
               (cr (- (at (third decoded)) 128d0)))
          (logior (jpeg-clamp (+ yy (* 1.772d0 cb)))
                  (ash (jpeg-clamp (+ yy (* -0.344136d0 cb) (* -0.714136d0 cr))) 8)
                  (ash (jpeg-clamp (+ yy (* 1.402d0 cr))) 16)
                  (ash 255 24))))))

(defun jpeg-decode-mcu (bits scan quant)
  (let ((decoded nil))
    (dolist (item scan)
      (destructuring-bind (c dc ac) item
        (let* ((h (jcomp-h c))
               (v (jcomp-v c))
               (blocks (make-array (* h v))))
          (dotimes (bi (* h v))
            (multiple-value-bind (samples pred)
                (jpeg-decode-block bits dc ac (aref quant (jcomp-qt c)) (jcomp-pred c))
              (setf (jcomp-pred c) pred
                    (aref blocks bi) samples)))
          (push (list c blocks) decoded))))
    (nreverse decoded)))

(defun jpeg-scan (p)
  (let ((len (jp-u16 p))
        (ns (jp-u8 p))
        (scan nil))
    (declare (ignore len))
    (dotimes (k ns)
      (let* ((id (jp-u8 p))
             (tdta (jp-u8 p))
             (c (jpeg-find-comp p id)))
        (unless c
          (error "JPEG scan component ~A is not in the frame." id))
        (push (list c
                    (aref (jparse-dc p) (ash tdta -4))
                    (aref (jparse-ac p) (logand tdta #xf)))
              scan)))
    (setf scan (nreverse scan))
    (jp-u8 p)
    (jp-u8 p)
    (jp-u8 p)
    (let* ((comps (jparse-comps p))
           (hmax (reduce #'max comps :key #'jcomp-h))
           (vmax (reduce #'max comps :key #'jcomp-v))
           (mw (* 8 hmax))
           (mh (* 8 vmax))
           (width (jparse-width p))
           (height (jparse-height p))
           (nx (ceiling width mw))
           (ny (ceiling height mh))
           (bits (make-jbits :data (jparse-data p) :i (jparse-i p)))
           (rgb (make-array (* width height) :element-type '(unsigned-byte 32)))
           (ri (jparse-ri p)))
      (dotimes (my ny)
        (dotimes (mx nx)
          (let ((mcu-index (+ (* my nx) mx)))
            (when (and (plusp ri) (plusp mcu-index) (zerop (mod mcu-index ri)))
              (jbits-align bits)
              (let ((rst (marker-from bits)))
                (unless (<= #xd0 rst #xd7)
                  (error "JPEG restart marker expected.")))
              (dolist (c comps)
                (setf (jcomp-pred c) 0))))
          (let ((decoded (jpeg-decode-mcu bits scan (jparse-quant p))))
            (dotimes (py mh)
              (dotimes (px mw)
                (let ((x (+ (* mx mw) px))
                      (y (+ (* my mh) py)))
                  (when (and (< x width) (< y height))
                    (setf (aref rgb (+ (* y width) x))
                          (jpeg-mcu-pixel decoded px py hmax vmax)))))))))
      (setf (jparse-i p) (jbits-i bits))
      rgb)))

(defun jpeg-decode (pathname)
  "Decode a baseline JPEG. Returns width, height, and a Cairo surface, or NIL."
  (let* ((data (with-open-file (in pathname :element-type '(unsigned-byte 8))
                 (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
                   (read-sequence bytes in)
                   bytes)))
         (p (make-jparse :data data))
         (pixels nil))
    (unless (and (= (jp-u8 p) #xff) (= (jp-u8 p) #xd8))
      (return-from jpeg-decode nil))
    (loop
      (let ((m (jp-marker p)))
        (cond
          ((= m #xd9) (return))
          ((= m #xda) (setf pixels (jpeg-scan p)))
          ((<= #xd0 m #xd7))
          (t
           (let* ((len (jp-u16 p))
                  (end (+ (jparse-i p) (- len 2))))
             (cond
               ((= m #xdb) (jpeg-read-dqt p end))
               ((= m #xc4) (jpeg-read-dht p end))
               ((= m #xc0) (jpeg-read-sof p))
               ((= m #xdd) (setf (jparse-ri p) (jp-u16 p)))
               ((or (= m #xc1) (= m #xc2))
                (return-from jpeg-decode nil)))
             (setf (jparse-i p) end))))))
    (unless (and pixels (plusp (jparse-width p)) (plusp (jparse-height p)))
      (return-from jpeg-decode nil))
    (let ((w (jparse-width p))
          (h (jparse-height p)))
      (values w h
              (rgb-surface w h
                           (lambda (x y)
                             (let ((px (aref pixels (+ (* y w) x))))
                               (values (ldb (byte 8 16) px)
                                       (ldb (byte 8 8) px)
                                       (ldb (byte 8 0) px)
                                       (ldb (byte 8 24) px)))))))))


(defun marker-from (bits)
  "Read a marker at the next byte of a byte-aligned entropy stream."
  (let ((data (jbits-data bits))
        (i (jbits-i bits)))
    (loop while (and (< i (length data)) (= (aref data i) #xff)) do (incf i))
    (when (>= i (length data))
      (error "Truncated JPEG."))
    (prog1 (aref data i)
      (setf (jbits-i bits) (1+ i)))))

(defvar *document-base* nil
  "Document URL used to resolve relative img src values. A <base href> wins.")

(defun url-scheme (url)
  (let ((colon (position #\: url)))
    (when (and colon (> colon 1)
               (every (lambda (c)
                        (or (alphanumericp c) (find c "+-.")))
                      (subseq url 0 colon)))
      (string-downcase (subseq url 0 colon)))))

(defun url-origin (url)
  (let* ((scheme (url-scheme url))
         (rest (subseq url (+ (length scheme) 3)))
         (slash (position #\/ rest)))
    (concatenate 'string scheme "://" (if slash (subseq rest 0 slash) rest))))

(defun url-directory (url)
  "Directory of URL, keeping a trailing slash and dropping the last segment otherwise."
  (let* ((cut (or (position #\# url) (length url)))
         (path (subseq url 0 (or (position #\? url :end cut) cut))))
    (if (and (plusp (length path)) (char= (char path (1- (length path))) #\/))
        path
        (let ((slash (position #\/ path :from-end t)))
          (if slash (subseq path 0 (1+ slash)) path)))))

(defun resolve-url (src base)
  "Resolve SRC against BASE. Absolute URLs, including file:, are unchanged."
  (cond
    ((or (null src) (not (stringp src)) (zerop (length src))) nil)
    ((url-scheme src) src)
    ((and (>= (length src) 2) (string= src "//" :end1 2))
     (concatenate 'string (or (and base (url-scheme base)) "https") ":" src))
    ((char= (char src 0) #\/)
     (if (and base (url-scheme base) (not (string= (url-scheme base) "file")))
         (concatenate 'string (url-origin base) src)
         src))
    ((null base) src)
    (t (concatenate 'string (url-directory base) src))))

(defun effective-base (doc explicit)
  "The document URL: <base href> resolved against EXPLICIT, or EXPLICIT itself."
  (let ((href (let ((el (and doc (find-element doc "base"))))
                (and el (dom-attr el "href")))))
    (if (and href (plusp (length href)))
        (or (resolve-url href explicit) href)
        explicit)))

(defun fetch-http-image (url)
  "Download an http or https image with Drakma and return the cache pathname."
  (let* ((dir (merge-pathnames "closure-images/" (uiop:temporary-directory)))
         (slash (position #\/ url :from-end t))
         (leaf (subseq url (if slash (1+ slash) 0)))
         (q (position #\? leaf))
         (leaf (if q (subseq leaf 0 q) leaf))
         (safe (remove-if-not (lambda (c) (or (alphanumericp c) (find c ".-_"))) leaf))
         (safe (subseq safe 0 (min 40 (length safe))))
         (name (format nil "~(~16,'0x~)-~A" (sxhash url) (if (plusp (length safe)) safe "image")))
         (dest (merge-pathnames name dir)))
    (ensure-directories-exist dest)
    (unless (probe-file dest)
      (multiple-value-bind (body status)
          (drakma:http-request url :force-binary t)
        (unless (and (integerp status) (<= 200 status 299) (vectorp body) (plusp (length body)))
          (return-from fetch-http-image nil))
        (with-open-file (out dest :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede :if-does-not-exist :create)
          (write-sequence body out))))
    dest))

(defvar *css-seen* nil
  "Stylesheet URLs currently being loaded, so @import cannot loop.")

(defun fetch-post (url body)
  "POST a form body and return the response text. Empty string on failure."
  (handler-case
      (multiple-value-bind (payload status)
          (drakma:http-request url :method :post
                               :content (or body "")
                               :content-type "application/x-www-form-urlencoded")
        (if (and (integerp status) (<= 200 status 299))
            (cond ((stringp payload) payload)
                  ((vectorp payload) (map 'string #'code-char payload))
                  (t ""))
            ""))
    (error () "")))

(defun fetch-css-text (url)
  "Load a stylesheet from a file or from http(s). Returns an empty string on failure."
  (when (or (null url) (not (stringp url)) (zerop (length url))
            (member url *css-seen* :test #'string=))
    (return-from fetch-css-text ""))
  (let ((*css-seen* (cons url *css-seen*)))
    (handler-case
        (cond
          ((member (url-scheme url) '("http" "https") :test #'string=)
           (multiple-value-bind (body status) (drakma:http-request url)
             (if (and (integerp status) (<= 200 status 299))
                 (cond ((stringp body) body)
                       ((vectorp body) (map 'string #'code-char body))
                       (t ""))
                 "")))
          (t
           (let* ((plain (let ((q (position #\? url))
                               (h (position #\# url)))
                           (subseq url 0 (or (and q h (min q h)) q h (length url)))))
                  (path (cond
                          ((and (>= (length plain) 7) (string-equal plain "file://" :end1 7))
                           (pathname (subseq plain 7)))
                          ((and (plusp (length plain)) (char= (char plain 0) #\/))
                           (pathname plain))
                          (t nil))))
             (if (and path (probe-file path))
                 (uiop:read-file-string path)
                 ""))))
      (error () ""))))

(defun image-pathname (src)
  "Local file for an img src. Relative URLs use *document-base*. http and https are fetched in Lisp."
  (let ((resolved (resolve-url src *document-base*)))
    (cond
      ((or (null resolved) (zerop (length resolved))) nil)
      ((member (url-scheme resolved) '("http" "https") :test #'string=)
       (handler-case (fetch-http-image resolved)
         (error () nil)))
      ((and (>= (length resolved) 7) (string-equal resolved "file://" :end1 7))
       (pathname (subseq resolved 7)))
      ((char= (char resolved 0) #\/)
       (pathname resolved))
      (t nil))))

(defun file-magic (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((b0 (read-byte in nil nil))
          (b1 (read-byte in nil nil))
          (b2 (read-byte in nil nil)))
      (cond
        ((and (eql b0 #x89) (eql b1 #x50) (eql b2 #x4e)) :png)
        ((and (eql b0 #x47) (eql b1 #x49) (eql b2 #x46)) :gif)
        ((and (eql b0 #xff) (eql b1 #xd8)) :jpeg)
        (t nil)))))

(defun load-image-surface (pathname)
  "Return a cached Cairo surface for a local PNG, GIF, or baseline JPEG."
  (let ((probed (probe-file pathname)))
    (unless probed
      (return-from load-image-surface nil))
    (let* ((name (namestring probed))
           (cached (or (gethash name *png-surfaces*)
                       (gethash name *image-surfaces*))))
      (when cached
        (return-from load-image-surface cached))
      (handler-case
          (case (file-magic probed)
            (:png (load-png-surface probed))
            (:gif (setf (gethash name *image-surfaces*) (gif-surface probed)))
            (:jpeg
             (multiple-value-bind (w h surface) (jpeg-decode probed)
               (declare (ignore w h))
               (when surface
                 (setf (gethash name *image-surfaces*) surface))))
            (t nil))
        (error () nil)))))

(defun image-intrinsic-size (pathname)
  "Return the pixel width and height of a local PNG, GIF, or JPEG, or NIL."
  (let ((surface (load-image-surface pathname)))
    (when surface
      (values (float (cairo-image-surface-get-width surface) 1d0)
              (float (cairo-image-surface-get-height surface) 1d0)))))
