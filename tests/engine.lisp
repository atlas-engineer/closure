(defpackage :closure.engine.tests
  (:use :cl)
  (:export #:run-tests))

(in-package :closure.engine.tests)

(defun ~ (a b &optional (eps 1.5d0))
  (<= (abs (- (float a 1d0) (float b 1d0))) eps))

(defun check (label ok)
  (format t "~&  ~A ~A~%" (if ok "ok" "FAIL") label)
  (unless ok
    (error "Engine test failed: ~A" label))
  ok)

(defun test-selectors-and-cascade ()
  (let* ((html "<style>
                  p { color: black; }
                  #a { color: red; margin-left: 10px; }
                  .n:nth-child(2n+1) { color: blue; }
                </style>
                <div id='a' class='n'>One</div>
                <div class='n'>Two</div>")
         (root (closure.engine:layout-html html :width 400 :height 200 :stylesheet "body{margin:0}")))
    (let ((a (closure.engine:find-box root "a")))
      (check "id box exists" a)
      (check "id specificity beats element color"
             (equalp '(1d0 0d0 0d0 1d0) (closure.engine:style-value a :color)))
      (check "margin from #a" (~ (closure.engine:box-x a) 10d0)))))

(defun test-flex ()
  (let* ((html "<div id='row' style='display:flex;width:300px;height:20px;justify-content:space-between'>
                  <div id='a' style='width:50px;height:20px;background:#c00'></div>
                  <div id='b' style='width:50px;height:20px;background:#0c0'></div>
                </div>")
         (root (closure.engine:layout-html html :width 400 :height 200 :stylesheet "body{margin:0}"))
         (row (closure.engine:find-box root "row"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b")))
    (check "flex row" (and row a b))
    (check "flex start" (~ (closure.engine:box-x a) (closure.engine:box-x row)))
    (check "flex end" (~ (closure.engine:box-x b)
                         (+ (closure.engine:box-x row) 250d0)))
    (check "flex width" (~ (closure.engine:box-width a) 50d0))))

(defun test-flex-wrap ()
  (let* ((html "<div id='row' style='display:flex;flex-wrap:wrap;width:100px;gap:10px'>
                  <div id='a' style='width:60px;height:10px'></div>
                  <div id='b' style='width:60px;height:10px'></div>
                  <div id='c' style='width:40px;height:10px'></div>
                  <div id='d' style='width:40px;height:10px'></div>
                </div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (row (closure.engine:find-box root "row"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c"))
         (d (closure.engine:find-box root "d")))
    (format t "~&  flex wrap: a ~A b ~A c ~A d ~A row-h ~A~%"
            (and a (list (closure.engine:box-x a) (closure.engine:box-y a)))
            (and b (list (closure.engine:box-x b) (closure.engine:box-y b)))
            (and c (list (closure.engine:box-x c) (closure.engine:box-y c)))
            (and d (list (closure.engine:box-x d) (closure.engine:box-y d)))
            (and row (closure.engine:box-height row)))
    (check "wrapped flex boxes" (and row a b c d))
    (check "first item stays on the first line"
           (and row a (~ (closure.engine:box-x a) (closure.engine:box-x row))
                (~ (closure.engine:box-y a) (closure.engine:box-y row))))
    (check "second item wraps to the next line"
           (and a b (~ (closure.engine:box-x b) (closure.engine:box-x a))
                (~ (closure.engine:box-y b) (+ (closure.engine:box-y a) 20d0))))
    (check "later items share a line"
           (and c d (~ (closure.engine:box-y c) (closure.engine:box-y d))
                (~ (closure.engine:box-x d) (+ (closure.engine:box-x c) 50d0))))
    (check "wrapped row grows to fit the lines"
           (and row (~ (closure.engine:box-height row) 50d0)))))

(defun test-flex-wrap-reverse ()
  (let* ((html "<div id='row' style='display:flex;flex-wrap:wrap-reverse;width:100px;height:40px'>
                  <div id='a' style='width:60px;height:10px'></div>
                  <div id='b' style='width:60px;height:10px'></div>
                </div>
                <div id='auto' style='display:flex;flex-wrap:wrap-reverse;width:100px'>
                  <div id='c' style='width:60px;height:10px'></div>
                  <div id='d' style='width:60px;height:10px'></div>
                </div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (row (closure.engine:find-box root "row"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (auto (closure.engine:find-box root "auto"))
         (c (closure.engine:find-box root "c"))
         (d (closure.engine:find-box root "d")))
    (format t "~&  wrap-reverse: a ~A b ~A c ~A d ~A~%"
            (and a (closure.engine:box-y a))
            (and b (closure.engine:box-y b))
            (and c (closure.engine:box-y c))
            (and d (closure.engine:box-y d)))
    (check "wrap-reverse boxes" (and row a b auto c d))
    (check "first line sits at the bottom of the row"
           (and row a (~ (closure.engine:box-y a)
                         (+ (closure.engine:box-y row) 30d0))))
    (check "next line sits above the first"
           (and a b (~ (closure.engine:box-y b)
                       (- (closure.engine:box-y a) 10d0))))
    (check "auto-height first line stays at the cross end"
           (and auto c (~ (closure.engine:box-height auto) 20d0)
                (~ (closure.engine:box-y c)
                   (+ (closure.engine:box-y auto) 10d0))))
    (check "auto-height next line stays above it"
           (and c d (~ (closure.engine:box-y d) (closure.engine:box-y auto))))))

(defun test-flex-align-content ()
  (let* ((html "<div id='row' style='display:flex;flex-wrap:wrap;align-content:center;width:100px;height:40px'>
                  <div id='a' style='width:60px;height:10px'></div>
                  <div id='b' style='width:60px;height:10px'></div>
                </div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (row (closure.engine:find-box root "row"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b")))
    (format t "~&  align-content: a ~A b ~A row ~A~%"
            (and a (closure.engine:box-y a))
            (and b (closure.engine:box-y b))
            (and row (closure.engine:box-height row)))
    (check "centered flex lines" (and row a b))
    (check "the row keeps its height"
           (and row (~ (closure.engine:box-height row) 40d0)))
    (check "lines are centered in the extra space"
           (and row a b
                (~ (closure.engine:box-y a) (+ (closure.engine:box-y row) 10d0))
                (~ (closure.engine:box-y b) (+ (closure.engine:box-y a) 10d0))))
    (check "centered lines stay at the content edge"
           (and row a (~ (closure.engine:box-x a) (closure.engine:box-x row))))))

(defun test-flex-align-content-end ()
  (let* ((html "<div id='row' style='display:flex;flex-wrap:wrap;align-content:flex-end;width:100px;height:40px'>
                  <div id='a' style='width:60px;height:10px'></div>
                  <div id='b' style='width:60px;height:10px'></div>
                </div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (row (closure.engine:find-box root "row"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b")))
    (format t "~&  align-content end: a ~A b ~A row ~A~%"
            (and a (closure.engine:box-y a))
            (and b (closure.engine:box-y b))
            (and row (closure.engine:box-height row)))
    (check "end-aligned flex lines" (and row a b))
    (check "the end-aligned row keeps its height"
           (and row (~ (closure.engine:box-height row) 40d0)))
    (check "the last line sits on the cross end"
           (and row b (~ (closure.engine:box-y b)
                         (+ (closure.engine:box-y row) 30d0))))
    (check "the first line sits just above it"
           (and a b (~ (closure.engine:box-y a)
                       (- (closure.engine:box-y b) 10d0))))
    (check "end-aligned lines stay at the content edge"
           (and row a (~ (closure.engine:box-x a) (closure.engine:box-x row))))))

(defun test-flex-align-content-between ()
  (let* ((html "<div id='row' style='display:flex;flex-wrap:wrap;align-content:space-between;width:100px;height:50px'>
                  <div id='a' style='width:60px;height:10px'></div>
                  <div id='b' style='width:60px;height:10px'></div>
                  <div id='c' style='width:60px;height:10px'></div>
                </div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (row (closure.engine:find-box root "row"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c")))
    (format t "~&  align-content between: a ~A b ~A c ~A~%"
            (and a (closure.engine:box-y a))
            (and b (closure.engine:box-y b))
            (and c (closure.engine:box-y c)))
    (check "spaced flex lines" (and row a b c))
    (check "the first line stays at the start"
           (and row a (~ (closure.engine:box-y a) (closure.engine:box-y row))))
    (check "the spaced last line sits on the cross end"
           (and row c (~ (closure.engine:box-y c)
                         (+ (closure.engine:box-y row) 40d0))))
    (check "the extra space is split between the lines"
           (and a b c
                (~ (closure.engine:box-y b) (+ (closure.engine:box-y a) 20d0))
                (~ (closure.engine:box-y c) (+ (closure.engine:box-y b) 20d0))))))

(defun test-flex-align-content-around ()
  (let* ((html "<div id='row' style='display:flex;flex-wrap:wrap;align-content:space-around;width:100px;height:40px'>
                  <div id='a' style='width:60px;height:10px'></div>
                  <div id='b' style='width:60px;height:10px'></div>
                </div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (row (closure.engine:find-box root "row"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b")))
    (format t "~&  align-content around: a ~A b ~A~%"
            (and a (closure.engine:box-y a))
            (and b (closure.engine:box-y b)))
    (check "space-around flex lines" (and row a b))
    (check "each line has space above it"
           (and row a (~ (closure.engine:box-y a)
                         (+ (closure.engine:box-y row) 5d0))))
    (check "the gap between lines is twice the edge gap"
           (and a b
                (~ (- (closure.engine:box-y b)
                      (+ (closure.engine:box-y a) (closure.engine:box-height a)))
                   10d0)
                (~ (- (+ (closure.engine:box-y row) (closure.engine:box-height row))
                      (+ (closure.engine:box-y b) (closure.engine:box-height b)))
                   5d0)))))

(defun test-flex-align-content-evenly ()
  (let* ((html "<div id='row' style='display:flex;flex-wrap:wrap;align-content:space-evenly;width:100px;height:50px'>
                  <div id='a' style='width:60px;height:10px'></div>
                  <div id='b' style='width:60px;height:10px'></div>
                </div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (row (closure.engine:find-box root "row"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (gap (and row a b
                   (- (closure.engine:box-y a) (closure.engine:box-y row)))))
    (format t "~&  align-content evenly: a ~A b ~A gap ~A~%"
            (and a (closure.engine:box-y a))
            (and b (closure.engine:box-y b))
            gap)
    (check "space-evenly flex lines" (and row a b gap))
    (check "the leading gap matches the gap between lines"
           (and a b gap
                (~ (- (closure.engine:box-y b)
                      (+ (closure.engine:box-y a) (closure.engine:box-height a)))
                   gap)))
    (check "the trailing gap matches the others"
           (and row b gap
                (~ (- (+ (closure.engine:box-y row) (closure.engine:box-height row))
                      (+ (closure.engine:box-y b) (closure.engine:box-height b)))
                   gap)))))

(defun test-grid-and-calc ()
  (let* ((html "<style>:root { --g: 10px; }</style>
                <div id='g' style='display:grid;width:200px;grid-template-columns:100px 100px'>
                  <div id='c1' style='height:12px;background:#111'></div>
                  <div id='c2' style='height:12px;background:#222'></div>
                  <div id='c3' style='height:12px;background:#333'></div>
                </div>
                <div id='calc' style='width:calc(100% - 40px);height:8px;margin-left:var(--g);background:#00c'></div>")
         (root (closure.engine:layout-html html :width 200 :height 200 :stylesheet "body{margin:0}"))
         (g (closure.engine:find-box root "g"))
         (c1 (closure.engine:find-box root "c1"))
         (c2 (closure.engine:find-box root "c2"))
         (c3 (closure.engine:find-box root "c3"))
         (calc (closure.engine:find-box root "calc")))
    (check "grid boxes" (and g c1 c2 c3 calc))
    (check "grid col 1" (~ (closure.engine:box-x c1) (closure.engine:box-x g)))
    (check "grid col 2" (~ (closure.engine:box-x c2) (+ (closure.engine:box-x g) 100d0)))
    (check "grid row 2" (> (closure.engine:box-y c3) (closure.engine:box-y c1)))
    (check "calc width" (~ (closure.engine:box-width calc) 160d0))
    (check "var margin" (~ (closure.engine:box-x calc) 10d0))))

(defun test-absolute-and-media ()
  (let* ((html "<style>
                  @media (min-width: 500px) { #abs { left: 20px; } }
                  @media (max-width: 100px) { #abs { left: 1px; } }
                </style>
                <div id='rel' style='position:relative;width:200px;height:80px'>
                  <div id='abs' style='position:absolute;top:10px;width:6px;height:6px;background:#f00'></div>
                </div>")
         (root (closure.engine:layout-html html :width 800 :height 200 :stylesheet "body{margin:0}"))
         (rel (closure.engine:find-box root "rel"))
         (abs (closure.engine:find-box root "abs")))
    (check "abs exists" (and rel abs))
    (check "media min-width overrides left"
           (~ (closure.engine:box-x abs) (+ (closure.engine:box-x rel) 20d0)))
    (check "abs top" (~ (closure.engine:box-y abs) (+ (closure.engine:box-y rel) 10d0)))))

(defun test-paint-and-speed ()
  (let* ((page "<header style='display:flex;justify-content:space-between;background:#123;color:#fff;padding:12px'>
                  <strong>Closure</strong><span>engine</span>
                </header>
                <main style='display:grid;grid-template-columns:1fr 1fr 1fr;gap:12px;padding:12px'>
                  <article style='background:#eef;padding:8px'><h2>One</h2><p>Modern layout.</p></article>
                  <article style='background:#efe;padding:8px'><h2>Two</h2><p>Flex and grid.</p></article>
                  <article style='background:#fee;padding:8px'><h2>Three</h2><p>Painted with Cairo.</p></article>
                </main>")
         (png "/tmp/closure-engine-demo.png")
         (root (closure.engine:render-html page :width 900 :height 500 :output png
                                           :stylesheet "body{margin:0;font-family:sans-serif}")))
    (check "demo root" root)
    (check "png written" (probe-file png))
    (let ((ink (closure.engine::count-ink png)))
      (format t "~&  ink pixels: ~D~%" ink)
      (check "png has painted pixels" (> ink 100)))
    (let* ((n 1500)
           (html (with-output-to-string (s)
                   (write-string "<div style='display:flex;flex-wrap:wrap;width:800px'>" s)
                   (dotimes (i n)
                     (format s "<div style='width:40px;height:18px;margin:2px;background:#~X~X~X'></div>"
                             (mod i 10) (mod i 8) (mod i 6)))
                   (write-string "</div>" s)))
           (t0 (get-internal-real-time))
           (laid (closure.engine:layout-html html :width 800 :height 600 :stylesheet "body{margin:0}"))
           (dt (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
      (format t "~&  layout ~D flex items in ~,3Fs~%" n dt)
      (check "perf layout finished" laid)
      (check "perf under 8s" (< dt 8d0)))))

(defun test-javascript ()
  (let* ((html "<h1 id='title'>Old</h1>
                <p id='out'></p>
                <script>
                  var n = 2 + 3 * 4;
                  function shout(x) { return x + '!'; }
                  var el = document.getElementById('title');
                  el.textContent = shout('JS');
                  document.getElementById('out').innerHTML = '<div id=\"b\">n=' + n + '</div>';
                  console.log('ran');
                </script>")
         (root (closure.engine:layout-html html :width 400 :height 200 :stylesheet "body{margin:0}"))
         (title (closure.engine:find-box root "title"))
         (b (closure.engine:find-box root "b")))
    (check "js title box" title)
    (check "js rewrote text" (and title (search "JS" (closure.engine::dom-text-content (closure.engine::box-element title)))))
    (check "js innerHTML element" b)
    (check "js innerHTML text"
           (and b (search "n=14" (closure.engine::dom-text-content (closure.engine::box-element b)))))))

(defun write-solid-png (path w h r g b)
  (closure.engine::ensure-cairo)
  (let* ((surface (closure.engine::cairo-image-surface-create 0 w h))
         (cr (closure.engine::cairo-create surface)))
    (unwind-protect
         (progn
           (closure.engine::cairo-set-source-rgba cr r g b 1d0)
           (closure.engine::cairo-paint cr)
           (closure.engine::cairo-surface-flush surface)
           (closure.engine::cairo-surface-write-to-png surface (namestring path)))
      (closure.engine::cairo-destroy cr)
      (closure.engine::cairo-surface-destroy surface))))

(defun test-png-image ()
  (let* ((swatch "/tmp/closure-engine-swatch.png")
         (out "/tmp/closure-engine-img.png"))
    (write-solid-png swatch 8 8 0.8d0 0.1d0 0.1d0)
    (let* ((html (format nil "<img id='pic' src='~A'>" swatch))
           (root (closure.engine:layout-html html :width 200 :height 100
                                            :stylesheet "body{margin:0}"))
           (pic (closure.engine:find-box root "pic")))
      (check "img box" pic)
      (check "intrinsic width" (and pic (~ (closure.engine:box-width pic) 8d0)))
      (check "intrinsic height" (and pic (~ (closure.engine:box-height pic) 8d0))))
    (let* ((html (format nil "<img id='attr' src='file://~A' width='30' height='12'>" swatch))
           (root (closure.engine:layout-html html :width 200 :height 100
                                            :stylesheet "body{margin:0}"))
           (attr (closure.engine:find-box root "attr")))
      (check "attr img" attr)
      (check "html width" (and attr (~ (closure.engine:box-width attr) 30d0)))
      (check "html height" (and attr (~ (closure.engine:box-height attr) 12d0))))
    (let* ((html (format nil "<img id='css' src='~A' style='width:40px;height:10px'>" swatch))
           (root (closure.engine:render-html html :width 200 :height 80 :output out
                                            :stylesheet "body{margin:0}"))
           (css (closure.engine:find-box root "css")))
      (check "css img" css)
      (check "css width" (and css (~ (closure.engine:box-width css) 40d0)))
      (check "css height" (and css (~ (closure.engine:box-height css) 10d0)))
      (let ((ink (closure.engine::count-ink out)))
        (format t "~&  img ink pixels: ~D~%" ink)
        (check "img painted" (> ink 20))))))

(defun write-solid-gif (path w h r g b)
  (let* ((table (skippy:make-color-table))
         (index (skippy:add-color (skippy:rgb-color r g b) table))
         (stream (skippy:make-data-stream :color-table table :width w :height h))
         (image (skippy:make-image :width w :height h :data-stream stream
                                   :image-data (skippy:make-image-data w h :initial-element index))))
    (skippy:add-image image stream)
    (skippy:output-data-stream stream path)))

(defun write-solid-jpeg (path w h r g b)
  "Mint a baseline JPEG with pnmtojpeg. The renderer decodes it in Lisp."
  (let* ((header (format nil "P6~%~D ~D~%255~%" w h))
         (ppm (make-array (+ (length header) (* w h 3)) :element-type '(unsigned-byte 8))))
    (loop for ch across header
          for i from 0
          do (setf (aref ppm i) (char-code ch)))
    (loop for i from 0 below (* w h)
          do (setf (aref ppm (+ (length header) (* i 3))) r
                   (aref ppm (+ (length header) (* i 3) 1)) g
                   (aref ppm (+ (length header) (* i 3) 2)) b))
    (uiop:with-temporary-file (:pathname ppm-path)
      (with-open-file (out ppm-path :direction :output :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
        (write-sequence ppm out))
      (uiop:run-program (list "pnmtojpeg" (namestring ppm-path))
                        :output path
                        :if-output-exists :supersede))))

(defun png-rgb (path x y)
  (closure.engine::ensure-cairo)
  (let ((surface (closure.engine::cairo-image-surface-create-from-png (namestring path))))
    (unwind-protect
         (let* ((stride (closure.engine::cairo-image-surface-get-stride surface))
                (data (closure.engine::cairo-image-surface-get-data surface))
                (px (cffi:mem-ref data :uint32 (+ (* y stride) (* x 4)))))
           (values (ldb (byte 8 16) px)
                   (ldb (byte 8 8) px)
                   (ldb (byte 8 0) px)))
      (closure.engine::cairo-surface-destroy surface))))

(defun test-gif-and-jpeg ()
  (let ((gif "/tmp/closure-engine-green.gif")
        (jpg "/tmp/closure-engine-red.jpg")
        (gif-png "/tmp/closure-engine-gif.png"))
    (write-solid-gif gif 4 6 10 200 20)
    (write-solid-jpeg jpg 16 16 220 15 15)
    (let* ((html (format nil "<img id='g' src='~A'>" gif))
           (root (closure.engine:layout-html html :width 80 :height 40 :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "g")))
      (check "gif box" box)
      (check "gif width" (and box (~ (closure.engine:box-width box) 4d0)))
      (check "gif height" (and box (~ (closure.engine:box-height box) 6d0))))
    (let* ((html (format nil "<img id='j' src='~A'>" jpg))
           (root (closure.engine:layout-html html :width 80 :height 40 :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "j")))
      (check "jpeg box" box)
      (check "jpeg width" (and box (~ (closure.engine:box-width box) 16d0)))
      (check "jpeg height" (and box (~ (closure.engine:box-height box) 16d0))))
    (let* ((html (format nil "<img id='g' src='~A'><img id='j' src='~A'>" gif jpg))
           (root (closure.engine:render-html html :width 80 :height 40 :output gif-png
                                            :stylesheet "body{margin:0}"))
           (g (closure.engine:find-box root "g"))
           (j (closure.engine:find-box root "j")))
      (check "gif+jpeg laid out" (and g j))
      (multiple-value-bind (r g b)
          (png-rgb gif-png (round (closure.engine:box-x g)) (round (closure.engine:box-y g)))
        (format t "~&  gif pixel: ~D ~D ~D~%" r g b)
        (check "gif painted green" (and (> g 150) (< r 80) (< b 80))))
      (multiple-value-bind (r g b)
          (png-rgb gif-png
                   (+ (round (closure.engine:box-x j)) 4)
                   (+ (round (closure.engine:box-y j)) 4))
        (format t "~&  jpeg pixel: ~D ~D ~D~%" r g b)
        (check "jpeg painted red" (and (> r 150) (< g 80) (< b 80)))))))

(defun test-relative-and-http-image ()
  (let* ((dir "/tmp/closure-rel/")
         (png (concatenate 'string dir "dot.png")))
    (ensure-directories-exist dir)
    (write-solid-png png 5 7 0.1d0 0.2d0 0.9d0)
    (let* ((html "<img id='r' src='dot.png'>")
           (root (closure.engine:layout-html html :width 80 :height 40
                                            :stylesheet "body{margin:0}"
                                            :base "file:///tmp/closure-rel/index.html"))
           (box (closure.engine:find-box root "r")))
      (check "relative img" box)
      (check "relative width" (and box (~ (closure.engine:box-width box) 5d0)))
      (check "relative height" (and box (~ (closure.engine:box-height box) 7d0))))
    (let* ((html "<base href='file:///tmp/closure-rel/'><img id='b' src='dot.png'>")
           (root (closure.engine:layout-html html :width 80 :height 40
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "b")))
      (check "base href img" box)
      (check "base href width" (and box (~ (closure.engine:box-width box) 5d0))))
    (let* ((port 18765)
           (proc (uiop:launch-program
                  (list "python3" "-m" "http.server" (princ-to-string port) "--bind" "127.0.0.1")
                  :directory dir
                  :output #p"/tmp/closure-http.log"
                  :error-output :output)))
      (unwind-protect
           (progn
             (loop repeat 50
                   until (ignore-errors
                          (drakma:http-request (format nil "http://127.0.0.1:~D/dot.png" port)
                                               :force-binary t)
                          t)
                   do (sleep 0.05))
             (let* ((html "<img id='h' src='dot.png'>")
                    (root (closure.engine:render-html
                           html :width 80 :height 40 :stylesheet "body{margin:0}"
                           :base (format nil "http://127.0.0.1:~D/index.html" port)
                           :output "/tmp/closure-http-img.png"))
                    (box (closure.engine:find-box root "h")))
               (check "http img" box)
               (check "http width" (and box (~ (closure.engine:box-width box) 5d0)))
               (check "http height" (and box (~ (closure.engine:box-height box) 7d0)))
               (multiple-value-bind (r g b)
                   (png-rgb "/tmp/closure-http-img.png"
                            (round (+ (closure.engine:box-x box)
                                      (/ (closure.engine:box-width box) 2d0)))
                            (round (+ (closure.engine:box-y box)
                                      (/ (closure.engine:box-height box) 2d0))))
                 (format t "~&  http pixel: ~D ~D ~D~%" r g b)
                 (check "http painted blue" (and (> b 180) (< r 80) (< g 120))))))
        (uiop:terminate-process proc)
        (uiop:wait-process proc)))))

(defun write-split-png (path w h r1 g1 b1 r2 g2 b2)
  "Left half is the first color, right half the second. Channels are 0..1."
  (closure.engine::ensure-cairo)
  (let* ((surface (closure.engine::cairo-image-surface-create 0 w h))
         (cr (closure.engine::cairo-create surface))
         (mid (floor w 2)))
    (unwind-protect
         (progn
           (closure.engine::cairo-set-source-rgba cr r1 g1 b1 1d0)
           (closure.engine::cairo-rectangle cr 0d0 0d0 (float mid 1d0) (float h 1d0))
           (closure.engine::cairo-fill cr)
           (closure.engine::cairo-set-source-rgba cr r2 g2 b2 1d0)
           (closure.engine::cairo-rectangle cr (float mid 1d0) 0d0 (float (- w mid) 1d0) (float h 1d0))
           (closure.engine::cairo-fill cr)
           (closure.engine::cairo-surface-flush surface)
           (closure.engine::cairo-surface-write-to-png surface (namestring path)))
      (closure.engine::cairo-destroy cr)
      (closure.engine::cairo-surface-destroy surface))))

(defun test-background-no-repeat ()
  (let ((blue "/tmp/closure-engine-bg-once.png")
        (out "/tmp/closure-engine-bg-once-out.png"))
    (write-solid-png blue 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='tile' style='width:20px;height:12px;background-image:url(\"~A\")'></div>
                            <div id='once' style='width:20px;height:12px;background-image:url(\"~A\");background-repeat:no-repeat'></div>"
                         blue blue))
           (root (closure.engine:render-html html :width 40 :height 40 :output out
                                            :stylesheet "body{margin:0}"))
           (tile (closure.engine:find-box root "tile"))
           (once (closure.engine:find-box root "once")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (+ (round (closure.engine:box-x box)) dx)
                          (+ (round (closure.engine:box-y box)) dy))))
        (check "repeat boxes" (and tile once))
        (multiple-value-bind (r g b) (at tile 8 8)
          (format t "~&  tiled corner: ~D ~D ~D~%" r g b)
          (check "the default repeats the image" (and (> b 180) (< r 80))))
        (multiple-value-bind (r g b) (at once 1 1)
          (format t "~&  once origin: ~D ~D ~D~%" r g b)
          (check "no-repeat still paints the image" (and (> b 180) (< r 80))))
        (multiple-value-bind (r g b) (at once 8 8)
          (format t "~&  once corner: ~D ~D ~D~%" r g b)
          (check "no-repeat does not tile" (and (> r 240) (> g 240) (> b 240))))))))

(defun test-background-position-right ()
  (let ((blue "/tmp/closure-engine-bg-right.png")
        (out "/tmp/closure-engine-bg-right-out.png"))
    (write-solid-png blue 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='left' style='width:20px;height:12px;background-image:url(\"~A\");background-repeat:no-repeat'></div>
                            <div id='right' style='width:20px;height:12px;background-image:url(\"~A\");background-repeat:no-repeat;background-position:right'></div>"
                         blue blue))
           (root (closure.engine:render-html html :width 40 :height 40 :output out
                                            :stylesheet "body{margin:0}"))
           (left (closure.engine:find-box root "left"))
           (right (closure.engine:find-box root "right")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (+ (round (closure.engine:box-x box)) dx)
                          (+ (round (closure.engine:box-y box)) dy))))
        (check "position boxes" (and left right))
        (multiple-value-bind (r g b) (at left 1 1)
          (format t "~&  left origin: ~D ~D ~D~%" r g b)
          (check "the default sits on the left" (and (> b 180) (< r 80))))
        (multiple-value-bind (r g b) (at right 1 1)
          (format t "~&  right origin: ~D ~D ~D~%" r g b)
          (check "right leaves the left edge empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at right 17 1)
          (format t "~&  right edge: ~D ~D ~D~%" r g b)
          (check "right places the image on the right" (and (> b 180) (< r 80))))))))

(defun test-background-position-center ()
  (let ((blue "/tmp/closure-engine-bg-center.png")
        (out "/tmp/closure-engine-bg-center-out.png"))
    (write-solid-png blue 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='mid' style='width:20px;height:12px;background-image:url(\"~A\");background-repeat:no-repeat;background-position:center'></div>"
                         blue))
           (root (closure.engine:render-html html :width 40 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "mid")))
      (labels ((at (dx dy)
                 (png-rgb out
                          (+ (round (closure.engine:box-x box)) dx)
                          (+ (round (closure.engine:box-y box)) dy))))
        (check "center box" box)
        (multiple-value-bind (r g b) (at 1 1)
          (format t "~&  center left: ~D ~D ~D~%" r g b)
          (check "center leaves the left edge empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at 9 1)
          (format t "~&  center image: ~D ~D ~D~%" r g b)
          (check "center places the image in the middle" (and (> b 180) (< r 80))))
        (multiple-value-bind (r g b) (at 17 1)
          (format t "~&  center right: ~D ~D ~D~%" r g b)
          (check "center leaves the right edge empty" (and (> r 240) (> g 240) (> b 240))))))))

(defun test-background-position-bottom ()
  (let ((blue "/tmp/closure-engine-bg-bottom.png")
        (out "/tmp/closure-engine-bg-bottom-out.png"))
    (write-solid-png blue 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='top' style='width:20px;height:12px;background-image:url(\"~A\");background-repeat:no-repeat'></div>
                            <div id='low' style='width:20px;height:12px;background-image:url(\"~A\");background-repeat:no-repeat;background-position:bottom'></div>"
                         blue blue))
           (root (closure.engine:render-html html :width 40 :height 40 :output out
                                            :stylesheet "body{margin:0}"))
           (top (closure.engine:find-box root "top"))
           (low (closure.engine:find-box root "low")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (+ (round (closure.engine:box-x box)) dx)
                          (+ (round (closure.engine:box-y box)) dy))))
        (check "bottom boxes" (and top low))
        (multiple-value-bind (r g b) (at top 1 1)
          (format t "~&  top origin: ~D ~D ~D~%" r g b)
          (check "the default sits on the top" (and (> b 180) (< r 80))))
        (multiple-value-bind (r g b) (at low 1 1)
          (format t "~&  bottom top: ~D ~D ~D~%" r g b)
          (check "bottom leaves the top edge empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at low 1 9)
          (format t "~&  bottom edge: ~D ~D ~D~%" r g b)
          (check "bottom places the image along the bottom" (and (> b 180) (< r 80))))))))

(defun test-background-position-right-bottom ()
  (let ((blue "/tmp/closure-engine-bg-corner.png")
        (out "/tmp/closure-engine-bg-corner-out.png"))
    (write-solid-png blue 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='corner' style='width:20px;height:12px;background-image:url(\"~A\");background-repeat:no-repeat;background-position:right bottom'></div>"
                         blue))
           (root (closure.engine:render-html html :width 40 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "corner")))
      (labels ((at (dx dy)
                 (png-rgb out
                          (+ (round (closure.engine:box-x box)) dx)
                          (+ (round (closure.engine:box-y box)) dy))))
        (check "corner box" box)
        (multiple-value-bind (r g b) (at 1 1)
          (format t "~&  corner origin: ~D ~D ~D~%" r g b)
          (check "the top-left stays empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at 17 1)
          (format t "~&  corner top-right: ~D ~D ~D~%" r g b)
          (check "the top-right stays empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at 1 9)
          (format t "~&  corner bottom-left: ~D ~D ~D~%" r g b)
          (check "the bottom-left stays empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at 17 9)
          (format t "~&  corner image: ~D ~D ~D~%" r g b)
          (check "right bottom places the image in the corner" (and (> b 180) (< r 80))))))))

(defun test-background-size-contain ()
  (let ((blue "/tmp/closure-engine-bg-contain.png")
        (out "/tmp/closure-engine-bg-contain-out.png"))
    (write-solid-png blue 8 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='auto' style='width:16px;height:16px;background-image:url(\"~A\");background-repeat:no-repeat'></div>
                            <div id='fit' style='width:16px;height:16px;background-image:url(\"~A\");background-repeat:no-repeat;background-size:contain'></div>"
                         blue blue))
           (root (closure.engine:render-html html :width 40 :height 50 :output out
                                            :stylesheet "body{margin:0}"))
           (auto (closure.engine:find-box root "auto"))
           (fit (closure.engine:find-box root "fit")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (+ (round (closure.engine:box-x box)) dx)
                          (+ (round (closure.engine:box-y box)) dy))))
        (check "contain boxes" (and auto fit))
        (multiple-value-bind (r g b) (at auto 12 1)
          (format t "~&  auto past width: ~D ~D ~D~%" r g b)
          (check "the intrinsic image stops at its own width" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at fit 12 1)
          (format t "~&  contain wide: ~D ~D ~D~%" r g b)
          (check "contain scales the image to the box width" (and (> b 180) (< r 80))))
        (multiple-value-bind (r g b) (at fit 1 12)
          (format t "~&  contain below: ~D ~D ~D~%" r g b)
          (check "contain leaves the extra side empty" (and (> r 240) (> g 240) (> b 240))))))))

(defun test-background-size-cover ()
  (let ((split "/tmp/closure-engine-bg-cover.png")
        (out "/tmp/closure-engine-bg-cover-out.png"))
    (write-split-png split 8 4 0.9d0 0.1d0 0.1d0 0.1d0 0.8d0 0.1d0)
    (let* ((html (format nil "<div id='cov' style='width:16px;height:16px;background-image:url(\"~A\");background-repeat:no-repeat;background-size:cover'></div>"
                         split))
           (root (closure.engine:render-html html :width 40 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "cov")))
      (labels ((at (dx dy)
                 (png-rgb out
                          (+ (round (closure.engine:box-x box)) dx)
                          (+ (round (closure.engine:box-y box)) dy))))
        (check "cover box" box)
        (multiple-value-bind (r g b) (at 1 14)
          (format t "~&  cover bottom: ~D ~D ~D~%" r g b)
          (check "cover fills the side that contain would leave empty"
                 (and (> r 180) (< g 80) (< b 80))))
        (multiple-value-bind (r g b) (at 12 1)
          (format t "~&  cover far side: ~D ~D ~D~%" r g b)
          (check "cover crops the part that does not fit"
                 (and (> r 180) (< g 80) (< b 80))))))))

(defun test-background-size-lengths ()
  (let ((blue "/tmp/closure-engine-bg-size.png")
        (out "/tmp/closure-engine-bg-size-out.png"))
    (write-solid-png blue 16 8 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='sized' style='width:24px;height:16px;background-image:url(\"~A\");background-repeat:no-repeat;background-size:8px 4px'></div>"
                         blue))
           (root (closure.engine:render-html html :width 40 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "sized")))
      (labels ((at (dx dy)
                 (png-rgb out
                          (+ (round (closure.engine:box-x box)) dx)
                          (+ (round (closure.engine:box-y box)) dy))))
        (check "sized box" box)
        (multiple-value-bind (r g b) (at 6 1)
          (format t "~&  sized inside: ~D ~D ~D~%" r g b)
          (check "the given size still paints the image" (and (> b 180) (< r 80))))
        (multiple-value-bind (r g b) (at 10 1)
          (format t "~&  sized past width: ~D ~D ~D~%" r g b)
          (check "the image stops at the given width" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at 1 6)
          (format t "~&  sized past height: ~D ~D ~D~%" r g b)
          (check "the image stops at the given height" (and (> r 240) (> g 240) (> b 240))))))))

(defun test-background-size-percent ()
  (let ((blue "/tmp/closure-engine-bg-pct.png")
        (out "/tmp/closure-engine-bg-pct-out.png"))
    (write-solid-png blue 16 16 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='half' style='width:20px;height:20px;background-image:url(\"~A\");background-repeat:no-repeat;background-size:50%'></div>"
                         blue))
           (root (closure.engine:render-html html :width 40 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "half")))
      (labels ((at (dx dy)
                 (png-rgb out
                          (+ (round (closure.engine:box-x box)) dx)
                          (+ (round (closure.engine:box-y box)) dy))))
        (check "percent box" box)
        (multiple-value-bind (r g b) (at 8 1)
          (format t "~&  percent inside: ~D ~D ~D~%" r g b)
          (check "half the box still paints the image" (and (> b 180) (< r 80))))
        (multiple-value-bind (r g b) (at 12 1)
          (format t "~&  percent past width: ~D ~D ~D~%" r g b)
          (check "the image stops at half the box width" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at 1 12)
          (format t "~&  percent past height: ~D ~D ~D~%" r g b)
          (check "the image stops at half the box height" (and (> r 240) (> g 240) (> b 240))))))))

(defun test-background-position-percent ()
  (let ((blue "/tmp/closure-engine-bg-pos-pct.png")
        (out "/tmp/closure-engine-bg-pos-pct-out.png"))
    (write-solid-png blue 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='mid' style='width:20px;height:12px;background-image:url(\"~A\");background-repeat:no-repeat;background-position:50%'></div>"
                         blue))
           (root (closure.engine:render-html html :width 40 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "mid")))
      (labels ((at (dx dy)
                 (png-rgb out
                          (+ (round (closure.engine:box-x box)) dx)
                          (+ (round (closure.engine:box-y box)) dy))))
        (check "percent position box" box)
        (multiple-value-bind (r g b) (at 1 1)
          (format t "~&  percent left: ~D ~D ~D~%" r g b)
          (check "50% leaves the left edge empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at 9 1)
          (format t "~&  percent middle: ~D ~D ~D~%" r g b)
          (check "50% places the image halfway across" (and (> b 180) (< r 80))))
        (multiple-value-bind (r g b) (at 17 1)
          (format t "~&  percent right: ~D ~D ~D~%" r g b)
          (check "50% leaves the right edge empty" (and (> r 240) (> g 240) (> b 240))))))))

(defun test-text-shadow ()
  (let* ((html "<span id='s' style='text-shadow:2px 2px #cc0000'>x</span>")
         (out "/tmp/closure-engine-text-shadow.png")
         (root (closure.engine:render-html html :width 80 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:24px}"))
         (box (closure.engine:find-box root "s"))
         (dark nil)
         (red nil))
    (check "shadow text box" box)
    (loop for y from 0 below 40 do
      (loop for x from 0 below 80 do
        (multiple-value-bind (r g b) (png-rgb out x y)
          (cond ((and (< r 40) (< g 40) (< b 40)) (push (cons x y) dark))
                ((and (> r 180) (< g 80) (< b 80)) (push (cons x y) red))))))
    (format t "~&  text shadow dark ~D red ~D~%" (length dark) (length red))
    (check "the glyph is painted" dark)
    (check "the shadow is painted" red)
    (when (and dark red)
      (let ((dark-x (reduce #'min dark :key #'car))
            (dark-y (reduce #'min dark :key #'cdr))
            (red-x (reduce #'min red :key #'car))
            (red-y (reduce #'min red :key #'cdr)))
        (format t "~&  glyph ~D,~D shadow ~D,~D~%" dark-x dark-y red-x red-y)
        (check "the shadow sits to the right" (> red-x dark-x))
        (check "the shadow sits below" (> red-y dark-y))))))

(defun test-text-shadow-two ()
  (let* ((html "<span id='s' style='text-shadow:2px 2px #cc0000, 6px 6px #0000cc'>x</span>")
         (out "/tmp/closure-engine-text-shadow-two.png")
         (root (closure.engine:render-html html :width 80 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:24px}"))
         (box (closure.engine:find-box root "s"))
         (red nil)
         (blue nil))
    (check "two-shadow text box" box)
    (loop for y from 0 below 40 do
      (loop for x from 0 below 80 do
        (multiple-value-bind (r g b) (png-rgb out x y)
          (cond ((and (> r 180) (< g 80) (< b 80)) (push (cons x y) red))
                ((and (< r 80) (< g 80) (> b 180)) (push (cons x y) blue))))))
    (format t "~&  two shadows red ~D blue ~D~%" (length red) (length blue))
    (check "the first shadow is painted" red)
    (check "the second shadow is painted" blue)
    (when (and red blue)
      (let ((red-x (reduce #'min red :key #'car))
            (red-y (reduce #'min red :key #'cdr))
            (blue-x (reduce #'min blue :key #'car))
            (blue-y (reduce #'min blue :key #'cdr)))
        (format t "~&  red ~D,~D blue ~D,~D~%" red-x red-y blue-x blue-y)
        (check "the second shadow sits further right" (> blue-x red-x))
        (check "the second shadow sits further down" (> blue-y red-y))))))

(defun test-linear-gradient ()
  (let* ((html "<div id='g' style='width:20px;height:20px;background:linear-gradient(#cc0000,#0000cc)'></div>")
         (out "/tmp/closure-engine-gradient.png")
         (root (closure.engine:render-html html :width 40 :height 30 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "g")))
    (labels ((at (dx dy)
               (png-rgb out
                        (+ (round (closure.engine:box-x box)) dx)
                        (+ (round (closure.engine:box-y box)) dy))))
      (check "gradient box" box)
      (multiple-value-bind (r g b) (at 4 1)
        (format t "~&  gradient top: ~D ~D ~D~%" r g b)
        (check "the gradient starts red" (and (> r 160) (< g 40) (< b 80))))
      (multiple-value-bind (r g b) (at 4 18)
        (format t "~&  gradient bottom: ~D ~D ~D~%" r g b)
        (check "the gradient ends blue" (and (< r 80) (< g 40) (> b 160))))
      (multiple-value-bind (r g b) (at 4 10)
        (format t "~&  gradient middle: ~D ~D ~D~%" r g b)
        (check "the middle mixes the two colors" (and (> r 40) (> b 40) (< g 40)))))))

(defun test-linear-gradient-right ()
  (let* ((html "<div id='g' style='width:20px;height:20px;background:linear-gradient(to right,#cc0000,#0000cc)'></div>")
         (out "/tmp/closure-engine-gradient-right.png")
         (root (closure.engine:render-html html :width 40 :height 30 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "g")))
    (labels ((at (dx dy)
               (png-rgb out
                        (+ (round (closure.engine:box-x box)) dx)
                        (+ (round (closure.engine:box-y box)) dy))))
      (check "horizontal gradient box" box)
      (multiple-value-bind (r g b) (at 1 10)
        (format t "~&  gradient left: ~D ~D ~D~%" r g b)
        (check "the gradient starts red on the left" (and (> r 160) (< g 40) (< b 80))))
      (multiple-value-bind (r g b) (at 18 10)
        (format t "~&  gradient right: ~D ~D ~D~%" r g b)
        (check "the gradient ends blue on the right" (and (< r 80) (< g 40) (> b 160))))
      (multiple-value-bind (r g b) (at 1 18)
        (format t "~&  gradient lower left: ~D ~D ~D~%" r g b)
        (check "the left edge stays red from top to bottom" (and (> r 160) (< g 40) (< b 80)))))))

(defun test-linear-gradient-angle ()
  (let* ((html "<div id='g' style='width:20px;height:20px;background:linear-gradient(45deg,#cc0000,#0000cc)'></div>")
         (out "/tmp/closure-engine-gradient-angle.png")
         (root (closure.engine:render-html html :width 40 :height 30 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "g")))
    (labels ((at (dx dy)
               (png-rgb out
                        (+ (round (closure.engine:box-x box)) dx)
                        (+ (round (closure.engine:box-y box)) dy))))
      (check "angled gradient box" box)
      (multiple-value-bind (r g b) (at 1 18)
        (format t "~&  gradient start: ~D ~D ~D~%" r g b)
        (check "45deg starts red at the bottom-left" (and (> r 160) (< g 40) (< b 80))))
      (multiple-value-bind (r g b) (at 18 1)
        (format t "~&  gradient end: ~D ~D ~D~%" r g b)
        (check "45deg ends blue at the top-right" (and (< r 80) (< g 40) (> b 160))))
      (multiple-value-bind (r g b) (at 1 1)
        (format t "~&  gradient corner: ~D ~D ~D~%" r g b)
        (check "the other corner mixes the colors" (and (> r 40) (> b 40) (< g 40)))))))

(defun test-linear-gradient-stop ()
  (let* ((html "<div id='g' style='width:20px;height:40px;background:linear-gradient(#cc0000 20%,#0000cc)'></div>")
         (out "/tmp/closure-engine-gradient-stop.png")
         (root (closure.engine:render-html html :width 40 :height 50 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "g")))
    (labels ((at (dx dy)
               (png-rgb out
                        (+ (round (closure.engine:box-x box)) dx)
                        (+ (round (closure.engine:box-y box)) dy))))
      (check "stopped gradient box" box)
      (multiple-value-bind (r g b) (at 4 6)
        (format t "~&  gradient hold: ~D ~D ~D~%" r g b)
        (check "the first color holds through the stop" (and (> r 180) (< g 30) (< b 20))))
      (multiple-value-bind (r g b) (at 4 24)
        (format t "~&  gradient after: ~D ~D ~D~%" r g b)
        (check "the blend is underway after the stop" (and (> r 40) (> b 40))))
      (multiple-value-bind (r g b) (at 4 38)
        (format t "~&  gradient end: ~D ~D ~D~%" r g b)
        (check "the gradient still ends on the last color" (and (< r 80) (< g 40) (> b 160)))))))

(defun test-radial-gradient ()
  (let* ((html "<div id='g' style='width:20px;height:20px;background:radial-gradient(#cc0000,#0000cc)'></div>")
         (out "/tmp/closure-engine-radial.png")
         (root (closure.engine:render-html html :width 40 :height 30 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "g")))
    (labels ((at (dx dy)
               (png-rgb out
                        (+ (round (closure.engine:box-x box)) dx)
                        (+ (round (closure.engine:box-y box)) dy))))
      (check "radial gradient box" box)
      (multiple-value-bind (r g b) (at 10 10)
        (format t "~&  radial center: ~D ~D ~D~%" r g b)
        (check "the center is the first color" (and (> r 160) (< g 40) (< b 80))))
      (multiple-value-bind (r g b) (at 1 1)
        (format t "~&  radial corner: ~D ~D ~D~%" r g b)
        (check "the corner is the last color" (and (< r 80) (< g 40) (> b 160))))
      (multiple-value-bind (r g b) (at 10 1)
        (format t "~&  radial edge: ~D ~D ~D~%" r g b)
        (check "the edge is farther along than the center" (and (> b r) (< g 40)))))))

(defun test-radial-gradient-closest-side ()
  (let* ((html "<div id='g' style='width:20px;height:20px;background:radial-gradient(circle closest-side,#cc0000,#0000cc)'></div>")
         (out "/tmp/closure-engine-radial-side.png")
         (root (closure.engine:render-html html :width 40 :height 30 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "g")))
    (labels ((at (dx dy)
               (png-rgb out
                        (+ (round (closure.engine:box-x box)) dx)
                        (+ (round (closure.engine:box-y box)) dy))))
      (check "closest-side box" box)
      (multiple-value-bind (r g b) (at 10 10)
        (format t "~&  closest center: ~D ~D ~D~%" r g b)
        (check "the center stays the first color" (and (> r 160) (< g 40) (< b 80))))
      (multiple-value-bind (r g b) (at 10 1)
        (format t "~&  closest edge: ~D ~D ~D~%" r g b)
        (check "the nearest edge is the last color" (and (< r 80) (< g 40) (> b 160)))))))

(defun test-radial-gradient-at-corner ()
  (let* ((html "<div id='g' style='width:20px;height:20px;background:radial-gradient(circle at 0 0,#cc0000,#0000cc)'></div>")
         (out "/tmp/closure-engine-radial-at.png")
         (root (closure.engine:render-html html :width 40 :height 30 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "g")))
    (labels ((at (dx dy)
               (png-rgb out
                        (+ (round (closure.engine:box-x box)) dx)
                        (+ (round (closure.engine:box-y box)) dy))))
      (check "corner radial box" box)
      (multiple-value-bind (r g b) (at 1 1)
        (format t "~&  radial corner start: ~D ~D ~D~%" r g b)
        (check "the top-left is the first color" (and (> r 160) (< g 40) (< b 80))))
      (multiple-value-bind (r g b) (at 18 18)
        (format t "~&  radial far corner: ~D ~D ~D~%" r g b)
        (check "the opposite corner is the last color" (and (< r 80) (< g 40) (> b 160))))
      (multiple-value-bind (r g b) (at 10 10)
        (format t "~&  radial middle: ~D ~D ~D~%" r g b)
        (check "the middle of the box is along the blend" (and (> b 40) (< g 40)))))))

(defun test-repeating-linear-gradient ()
  (let* ((html "<div id='g' style='width:20px;height:40px;background:repeating-linear-gradient(#cc0000,#0000cc 50%)'></div>")
         (out "/tmp/closure-engine-repeat-gradient.png")
         (root (closure.engine:render-html html :width 40 :height 50 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "g")))
    (labels ((at (dy)
               (png-rgb out
                        (+ (round (closure.engine:box-x box)) 4)
                        (+ (round (closure.engine:box-y box)) dy))))
      (check "repeating gradient box" box)
      (multiple-value-bind (r g b) (at 1)
        (format t "~&  repeat start: ~D ~D ~D~%" r g b)
        (check "the first cycle starts red" (and (> r 160) (< g 40) (< b 80))))
      (multiple-value-bind (r g b) (at 18)
        (format t "~&  repeat middle: ~D ~D ~D~%" r g b)
        (check "the first cycle ends blue" (and (< r 80) (< g 40) (> b 160))))
      (multiple-value-bind (r g b) (at 22)
        (format t "~&  repeat again: ~D ~D ~D~%" r g b)
        (check "the second cycle starts red" (and (> r 160) (< g 40) (< b 80))))
      (multiple-value-bind (r g b) (at 38)
        (format t "~&  repeat end: ~D ~D ~D~%" r g b)
        (check "the second cycle ends blue" (and (< r 80) (< g 40) (> b 160)))))))

(defun test-background-and-object-fit ()
  (let ((blue "/tmp/closure-engine-bg.png")
        (split "/tmp/closure-engine-split.png")
        (out "/tmp/closure-engine-fit.png"))
    (write-solid-png blue 4 4 0.1d0 0.2d0 0.9d0)
    (write-split-png split 8 4 0.9d0 0.1d0 0.1d0 0.1d0 0.8d0 0.1d0)
    (let* ((html (format nil "<div id='bg' style='width:20px;height:12px;background-image:url(\"~A\")'></div>" blue))
           (root (closure.engine:render-html html :width 40 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "bg")))
      (check "background box" box)
      (multiple-value-bind (r g b)
          (png-rgb out
                   (+ (round (closure.engine:box-x box)) 1)
                   (+ (round (closure.engine:box-y box)) 1))
        (format t "~&  background pixel: ~D ~D ~D~%" r g b)
        (check "background image painted" (and (> b 180) (< r 80)))))
    (let* ((html (format nil "<img id='c' src='~A' style='width:8px;height:8px;object-fit:contain'>" split))
           (root (closure.engine:render-html html :width 20 :height 20 :output out
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "c"))
           (x (round (closure.engine:box-x box)))
           (y (round (closure.engine:box-y box))))
      (check "contain box" box)
      (multiple-value-bind (r g b) (png-rgb out x y)
        (format t "~&  contain corner: ~D ~D ~D~%" r g b)
        (check "contain letterbox" (and (> r 240) (> g 240) (> b 240))))
      (multiple-value-bind (r g b) (png-rgb out x (+ y 3))
        (format t "~&  contain left: ~D ~D ~D~%" r g b)
        (check "contain shows image" (> r 180))))
    (let* ((html (format nil "<img id='v' src='~A' style='width:8px;height:8px;object-fit:cover'>" split))
           (root (closure.engine:render-html html :width 20 :height 20 :output out
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "v"))
           (x (round (closure.engine:box-x box)))
           (y (round (closure.engine:box-y box))))
      (multiple-value-bind (r g b) (png-rgb out x y)
        (format t "~&  cover corner: ~D ~D ~D~%" r g b)
        (check "cover fills corner" (> r 180))))))

(defun text-ys (box)
  (let ((out nil))
    (labels ((walk (b)
               (when b
                 (when (and (eq (closure.engine::box-kind b) :text)
                            (closure.engine::box-text b))
                   (push (cons (closure.engine::box-text b) (closure.engine::box-y b)) out))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk box))
    out))

(defun test-dir-optgroup ()
  (let* ((root (closure.engine:layout-html
                "<dir id='d'><li id='i'>Item</li></dir>
                 <select id='s'><optgroup label='Group'><option>Blue</option></optgroup></select>
                 <p id='p' style='width:40px'>super<wbr>cali</p>
                 <noscript id='n'>No script</noscript>"
                :width 400 :height 160
                :stylesheet "body{margin:0} dir{margin:0}"))
         (dir (closure.engine:find-box root "d"))
         (item (closure.engine:find-box root "i"))
         (sel (closure.engine:find-box root "s"))
         (para (closure.engine:find-box root "p"))
         (noscript (closure.engine:find-box root "n")))
    (check "directory and optgroup" (and dir item sel para (not noscript)))
    (check "a directory indents its item by 40px"
           (and dir item (~ (- (closure.engine:box-x item) (closure.engine:box-x dir)) 40d0)))
    (check "a select grows to its bold group label"
           (and sel (~ (closure.engine:box-width sel) 65.22d0 0.5d0)
                (~ (closure.engine:box-height sel) 19d0)
                (> (closure.engine:box-width sel) 55d0)))
    (check "wbr wraps a narrow line into two"
           (and para (~ (closure.engine:box-height para) 36d0)))))

(defun test-tfoot-menu ()
  (let* ((root (closure.engine:layout-html
                "<table id='t'><tfoot><tr><td id='f'>Foot</td></tr></tfoot><tbody><tr><td id='b'>Body</td></tr></tbody></table>
                 <menu id='m'><li id='i'>Item</li></menu>
                 <hgroup id='g'><h2 id='h'>Title</h2><p>Sub</p></hgroup>"
                :width 400 :height 200
                :stylesheet "body{margin:0} menu{margin:0}"))
         (foot (closure.engine:find-box root "f"))
         (body (closure.engine:find-box root "b"))
         (item (closure.engine:find-box root "i"))
         (menu (closure.engine:find-box root "m"))
         (group (closure.engine:find-box root "g"))
         (heading (closure.engine:find-box root "h")))
    (check "footer, menu, and group" (and foot body item menu group heading))
    (check "a footer row is painted after the body"
           (and foot body (> (closure.engine:box-y foot) (closure.engine:box-y body))))
    (check "a menu indents its item by 40px"
           (and menu item (~ (- (closure.engine:box-x item) (closure.engine:box-x menu)) 40d0)))
    (check "a heading group contains its heading"
           (and group heading
                (~ (closure.engine:box-x heading) (closure.engine:box-x group))
                (>= (closure.engine:box-height group) (closure.engine:box-height heading))))))

(defun test-slot-columns ()
  (let* ((root (closure.engine:layout-html
                "<table id='t'><colgroup id='cg'><col id='c1'><col id='c2'></colgroup><tr><td id='a'>One</td><td id='b'>Two</td></tr></table>
                 <p>host <slot id='sl'>fallback</slot> end</p>
                 <datalist id='dl'><option>Rome</option></datalist>"
                :width 400 :height 80
                :stylesheet "body{margin:0}"))
         (a (closure.engine:find-box root "a"))
         (c1 (closure.engine:find-box root "c1"))
         (cg (closure.engine:find-box root "cg"))
         (dl (closure.engine:find-box root "dl"))
         (fallback nil))
    (labels ((walk (b)
               (when b
                 (when (and (eq (closure.engine::box-kind b) :text)
                            (string= (or (closure.engine::box-text b) "") "fallback"))
                   (setf fallback b))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (check "columns and slot" (and a c1 cg fallback (not dl)))
    (check "a col covers its cell"
           (and a c1
                (~ (closure.engine:box-x c1) (closure.engine:box-x a))
                (~ (closure.engine:box-width c1) (closure.engine:box-width a))))
    (check "a colgroup covers every column"
           (and cg c1 (>= (closure.engine:box-width cg) (closure.engine:box-width c1))))
    (check "a slot's fallback is ordinary text"
           (and fallback (> (closure.engine:box-width fallback) 20d0)))))

(defun test-picture-map ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'><picture><img id='im' src='missing.png' alt='s' width='20' height='20'></picture> tail</p>
                 <p id='q'>go <map id='m'><area shape='rect' coords='0,0,10,10'></map>on</p>"
                :width 400 :height 80
                :stylesheet "body{margin:0} p{margin:0}"))
         (p (closure.engine:find-box root "p"))
         (im (closure.engine:find-box root "im"))
         (go nil)
         (on nil))
    (labels ((walk (b)
               (when b
                 (when (eq (closure.engine::box-kind b) :text)
                   (cond ((string= (or (closure.engine::box-text b) "") "go")
                          (setf go b))
                         ((string= (or (closure.engine::box-text b) "") "on")
                          (setf on b))))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (check "picture and map" (and p im go on))
    (check "the picture's image is 20 by 20 and the line is 24 tall"
           (and im p
                (~ (closure.engine:box-width im) 20d0)
                (~ (closure.engine:box-height im) 20d0)
                (~ (closure.engine:box-height p) 24d0)))
    (check "a map does not separate the words around it"
           (and go on
                (~ (closure.engine:box-x on)
                   (+ (closure.engine:box-x go) (closure.engine:box-width go) 4d0)
                   0.2d0)))))

(defun test-dialog-search ()
  (let* ((root (closure.engine:layout-html
                "<dialog id='c'>Dialog</dialog><p id='b'>Before</p><dialog id='d' open>Dialog</dialog><search id='s'>Search</search><p><output id='o'>42</output></p>"
                :width 400 :height 240
                :stylesheet "body{margin:0} p{margin:0}"))
         (closed (closure.engine:find-box root "c"))
         (before (closure.engine:find-box root "b"))
         (open (closure.engine:find-box root "d"))
         (search (closure.engine:find-box root "s"))
         (output (closure.engine:find-box root "o")))
    (check "dialog and search" (and (not closed) open search output before))
    (check "an open dialog sits at the static position and is centered"
           (and open search before
                (~ (closure.engine:box-y before) 0d0)
                (~ (closure.engine:box-height before) 18d0)
                (~ (closure.engine:box-y open) 18d0)
                (~ (closure.engine:box-y search) 18d0)
                (~ (closure.engine:box-height open) 56d0)
                (~ (closure.engine:box-x open)
                   (/ (- 400d0 (closure.engine:box-width open)) 2d0)
                   0.5d0)))
    (let ((word nil))
      (labels ((walk (b)
                 (when b
                   (when (and (eq (closure.engine::box-kind b) :text)
                              (string= (closure.engine::box-text b) "Dialog"))
                     (setf word b))
                   (dolist (c (closure.engine::box-children b)) (walk c)))))
        (walk root))
      (check "the dialog text moves into the centered box"
             (and open word
                  (>= (closure.engine:box-x word) (closure.engine:box-x open))
                  (<= (+ (closure.engine:box-x word) (closure.engine:box-width word))
                      (+ (closure.engine:box-x open) (closure.engine:box-width open))))))
    (check "search is a full-width block"
           (and search (~ (closure.engine:box-width search) 400d0)
                (~ (closure.engine:box-height search) 18d0)))
    (check "output is an inline word"
           (and output (~ (closure.engine:box-width output) 16d0 0.5d0)))))

(defun test-iframe-border ()
  (let ((path "/tmp/closure-engine-iframe.png"))
    (closure.engine:render-html
     "<iframe></iframe>"
     :width 320 :height 170
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 4 1)
      (check "an iframe border is gray"
             (and (< (abs (- r 154)) 40)
                  (< (abs (- g 154)) 40)
                  (< (abs (- b 154)) 40))))
    (multiple-value-bind (r g b) (png-rgb path 20 152)
      (check "the iframe bottom edge is the light inset gray"
             (and (< (abs (- r 238)) 20)
                  (< (abs (- g 238)) 20)
                  (< (abs (- b 238)) 20))))))

(defun test-solid-border-rows ()
  (let ((path "/tmp/closure-engine-solid-border.png"))
    (closure.engine:render-html
     "<div style='margin-top:0.2px;border:3px solid #000;width:40px;height:10px'></div>"
     :width 80 :height 30
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 10 0)
      (declare (ignore g b))
      (check "a 3px border starts on a solid pixel" (< r 20)))
    (multiple-value-bind (r g b) (png-rgb path 10 2)
      (declare (ignore g b))
      (check "a 3px border fills its third pixel" (< r 20)))
    (multiple-value-bind (r g b) (png-rgb path 10 3)
      (declare (ignore g b))
      (check "a 3px border does not spill into the next row" (> r 250)))))

(defun test-iframe-object ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'><iframe id='f'></iframe> <object id='o'></object></p><p><bdi id='b'>bdi</bdi> <data id='d'>data</data></p>"
                :width 1024 :height 220
                :stylesheet "body{margin:0} p{margin:0}"))
         (frame (closure.engine:find-box root "f"))
         (object (closure.engine:find-box root "o"))
         (bdi (closure.engine:find-box root "b"))
         (data (closure.engine:find-box root "d")))
    (check "iframe and object" (and frame object bdi data))
    (check "an iframe is 300 by 150 inside a 2px border"
           (and frame
                (~ (closure.engine:box-width frame) 304d0)
                (~ (closure.engine:box-height frame) 154d0)))
    (check "an object is 300 by 150 and shares the iframe baseline"
           (and frame object
                (~ (closure.engine:box-width object) 300d0)
                (~ (closure.engine:box-height object) 150d0)
                (~ (closure.engine:box-x object) 308d0)
                (~ (- (closure.engine:box-y object) (closure.engine:box-y frame)) 4d0)))
    (check "bdi and data are ordinary inline words"
           (and bdi data
                (~ (closure.engine:box-width bdi) 20.45d0 0.2d0)
                (~ (closure.engine:box-width data) 26.66d0 0.2d0)))))

(defun test-form-metrics ()
  (let* ((root (closure.engine:layout-html
                "<button id='b'>OK</button><input id='n' value='Hi'><select id='s'><option selected>Blue</option></select><textarea id='t'>note</textarea>"
                :width 800 :height 80
                :stylesheet "body{margin:0}"))
         (b (closure.engine:find-box root "b"))
         (n (closure.engine:find-box root "n"))
         (s (closure.engine:find-box root "s"))
         (tt (closure.engine:find-box root "t")))
    (check "form metric boxes" (and b n s tt))
    (check "a button is about 35 by 21"
           (and b (~ (closure.engine:box-width b) 35.27d0 0.15d0)
                (~ (closure.engine:box-height b) 21d0)))
    (check "a text input is 177 by 21"
           (and n (~ (closure.engine:box-width n) 177d0)
                (~ (closure.engine:box-height n) 21d0)))
    (check "a select fits its label"
           (and s (~ (closure.engine:box-width s) 49d0 0.6d0)
                (~ (closure.engine:box-height s) 19d0)))
    (check "a textarea is 181 by 40"
           (and tt (~ (closure.engine:box-width tt) 181d0)
                (~ (closure.engine:box-height tt) 40d0)))))

(defun test-form-controls ()
  (let* ((html "<input id='n' value='old'>
                <button id='b'>OK</button>
                <select id='s'><option>Red</option><option selected>Blue</option></select>
                <textarea id='t'>note</textarea>
                <div id='out'></div>
                <script>
                  var n = document.getElementById('n');
                  n.value = 'Hi';
                  document.getElementById('out').textContent =
                    n.value + '/' + document.getElementById('s').value + '/' +
                    document.getElementById('t').value;
                  document.getElementById('t').value = 'next';
                </script>")
         (out "/tmp/closure-engine-form.png")
         (root (closure.engine:render-html html :width 400 :height 200 :output out
                                          :stylesheet "body{margin:0}"))
         (n (closure.engine:find-box root "n"))
         (b (closure.engine:find-box root "b"))
         (s (closure.engine:find-box root "s"))
         (tt (closure.engine:find-box root "t"))
         (shown (closure.engine:find-box root "out")))
    (check "input box" n)
    (check "input sized" (and n (> (closure.engine:box-width n) 100d0)
                              (< (closure.engine:box-width n) 180d0)
                              (> (closure.engine:box-height n) 18d0)
                              (< (closure.engine:box-height n) 40d0)))
    (check "button box" b)
    (check "button shrinks" (and b (< (closure.engine:box-width b) 80d0)
                                 (> (closure.engine:box-height b) 10d0)))
    (check "select box" s)
    (check "textarea box" (and tt (> (closure.engine:box-height tt) 30d0)))
    (check "script read values" (and shown (search "Hi/Blue/note"
                                                   (closure.engine::dom-text-content
                                                    (closure.engine::box-element shown)))))
    (let ((ink (closure.engine::count-ink out)))
      (format t "~&  form ink pixels: ~D~%" ink)
      (check "controls painted" (> ink 40)))))

(defun test-inline-image-baseline ()
  (let ((png "/tmp/closure-inline-img.png"))
    (write-solid-png png 8 8 0.8d0 0.1d0 0.1d0)
    (let* ((html (format nil "<p id='p'>Hi <img id='pic' src='~A' width='12' height='12'> there</p>" png))
           (root (closure.engine:layout-html html :width 300 :height 80
                                            :stylesheet "body{margin:0;font-size:16px}"))
           (pic (closure.engine:find-box root "pic"))
           (texts nil))
      (labels ((walk (b)
                 (when b
                   (when (and (eq (closure.engine::box-kind b) :text)
                              (closure.engine::box-text b)
                              (plusp (length (string-trim '(#\space) (closure.engine::box-text b)))))
                     (push b texts))
                   (dolist (c (closure.engine::box-children b)) (walk c)))))
        (walk root))
      (let ((hi (find "Hi" texts :key #'closure.engine::box-text :test #'string=))
            (there (find "there" texts :key #'closure.engine::box-text :test #'string=)))
        (check "inline image" (and pic hi there))
        (check "image sits between the words"
               (and pic hi there
                    (< (closure.engine:box-x hi) (closure.engine:box-x pic))
                    (< (closure.engine:box-x pic) (closure.engine:box-x there))))
        (check "image bottom meets the text baseline"
               (and pic hi
                    (~ (+ (closure.engine:box-y pic) (closure.engine:box-height pic))
                       (+ (closure.engine:box-y hi) (closure.engine::box-baseline hi)))))))))

(defun test-nested-inline ()
  (let* ((html "<p>Hello <span>kind <b id='b'>bold</b></span> world</p>")
         (out "/tmp/closure-engine-nested-inline.png")
         (root (closure.engine:render-html html :width 400 :height 40 :output out
                                          :stylesheet "body{margin:0} p{margin:0;font-size:16px}"))
         (hello nil) (kind nil) (world nil)
         (bold (closure.engine:find-box root "b"))
         (bold-adv (closure.engine::text-advance "bold" 16d0 "serif" :bold :normal))
         (norm-adv (closure.engine::text-advance "bold" 16d0 "serif" :normal :normal)))
    (labels ((walk (b)
               (when b
                 (when (eq (closure.engine::box-kind b) :text)
                   (cond
                     ((string= (or (closure.engine::box-text b) "") "Hello") (setf hello b))
                     ((string= (or (closure.engine::box-text b) "") "kind") (setf kind b))
                     ((string= (or (closure.engine::box-text b) "") "world") (setf world b))))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (check "nested words are present" (and hello kind bold world))
    (check "nested words stay in order"
           (and hello kind bold world
                (< (closure.engine:box-x hello) (closure.engine:box-x kind))
                (< (closure.engine:box-x kind) (closure.engine:box-x bold))
                (< (closure.engine:box-x bold) (closure.engine:box-x world))))
    (check "bold word uses a bold font"
           (and bold
                (eq (closure.engine::computed-font-weight (closure.engine::box-style bold)) :bold)
                (> bold-adv norm-adv)
                (~ (closure.engine:box-width bold) bold-adv)))
    (let ((ink (closure.engine::count-ink out)))
      (format t "~&  nested inline ink pixels: ~D~%" ink)
      (check "nested inline text is painted" (> ink 10)))))

(defun test-link-underline ()
  (let* ((html "<a id='go' href='next.html'>link</a><span id='plain'>link</span>")
         (out "/tmp/closure-engine-underline.png")
         (root (closure.engine:render-html html :width 240 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (link (closure.engine:find-box root "go"))
         (plain (closure.engine:find-box root "plain")))
    (check "link text box" link)
    (check "plain text box" plain)
    (labels ((blue-below (box)
               (when box
                 (let ((y0 (floor (+ (closure.engine:box-y box)
                                     (closure.engine::box-baseline box)
                                     1d0)))
                       (x (floor (+ (closure.engine:box-x box) 2d0))))
                   (loop for y from y0 to (+ y0 4)
                         thereis (and (< y 40)
                                      (multiple-value-bind (r g b) (png-rgb out x y)
                                        (and (> b 180) (> b r) (> b g)))))))))
      (format t "~&  link underline: ~A plain underline: ~A~%"
              (blue-below link) (blue-below plain))
      (check "link is underlined in its color" (blue-below link))
      (check "plain text is not underlined" (not (blue-below plain))))))

(defun test-link-underline-nested ()
  (let* ((html "<a href='next.html'>see <b id='b' style='color:#cc0000'>this</b></a>")
         (out "/tmp/closure-engine-underline-nested.png")
         (root (closure.engine:render-html html :width 240 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (bold (closure.engine:find-box root "b")))
    (check "nested link word" bold)
    (when bold
      (let ((top (floor (+ (closure.engine:box-y bold) 2d0)))
            (y0 (floor (+ (closure.engine:box-y bold)
                          (closure.engine::box-baseline bold)
                          1d0)))
            (x (floor (+ (closure.engine:box-x bold) 2d0))))
        (labels ((at (y)
                   (png-rgb out x y))
                 (red (y)
                   (multiple-value-bind (r g b) (at y)
                     (and (> r 150) (> r g) (> r b))))
                 (blue (y)
                   (multiple-value-bind (r g b) (at y)
                     (and (> b 180) (> b r) (> b g)))))
          (format t "~&  nested glyph red: ~A underline blue: ~A~%"
                  (loop for y from top below y0 thereis (red y))
                  (loop for y from y0 to (+ y0 4) thereis (and (< y 40) (blue y))))
          (check "nested word keeps its own color"
                 (loop for y from top below y0 thereis (red y)))
          (check "underline under the nested word stays the link color"
                 (loop for y from y0 to (+ y0 4) thereis (and (< y 40) (blue y)))))))))

(defun test-image-alt ()
  (let* ((html "<img id='gone' src='/tmp/closure-does-not-exist.png' alt='missing'>")
         (out "/tmp/closure-engine-alt.png")
         (root (closure.engine:render-html html :width 200 :height 40 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "gone"))
         (adv (closure.engine::text-advance "missing" 16d0 "serif" :normal :normal)))
    (check "broken image box" box)
    (check "alt sizes the box" (and box (~ (closure.engine:box-width box) adv)))
    (check "alt text is stored" (and box (string= (closure.engine::box-text box) "missing")))
    (let ((ink (closure.engine::count-ink out)))
      (format t "~&  alt ink pixels: ~D~%" ink)
      (check "alt text is painted" (> ink 10)))))

(defun test-br ()
  (let* ((html "<p id='p'>one<br>two</p>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0} p{margin:0}"))
         (one nil)
         (two nil))
    (labels ((walk (b)
               (when b
                 (when (eq (closure.engine::box-kind b) :text)
                   (cond
                     ((string= (or (closure.engine::box-text b) "") "one") (setf one b))
                     ((string= (or (closure.engine::box-text b) "") "two") (setf two b))))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (check "br splits the words" (and one two))
    (check "br puts the next word below"
           (and one two (> (closure.engine:box-y two) (+ (closure.engine:box-y one) 4d0))))
    (check "br starts the next line at the same edge"
           (and one two (~ (closure.engine:box-x two) (closure.engine:box-x one))))))

(defun test-image-alt-clip ()
  (let* ((html "<img id='gone' src='/tmp/closure-does-not-exist.png' alt='WWWWWWWWWWWW' width='24' height='16'>")
         (out "/tmp/closure-engine-alt-clip.png")
         (root (closure.engine:render-html html :width 200 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (box (closure.engine:find-box root "gone")))
    (check "sized broken image" box)
    (check "html width keeps the box" (and box (~ (closure.engine:box-width box) 24d0)))
    (check "alt ink stays inside the box"
           (loop for y from 0 below 16
                 thereis (loop for x from 0 below 24
                               thereis (multiple-value-bind (r g b) (png-rgb out x y)
                                         (or (< r 250) (< g 250) (< b 250))))))
    (multiple-value-bind (r g b) (png-rgb out 40 8)
      (format t "~&  past alt pixel: ~D ~D ~D~%" r g b)
      (check "alt does not spill past the box" (and (> r 250) (> g 250) (> b 250))))))

(defun test-inline-image-middle ()
  (let ((png "/tmp/closure-inline-middle.png"))
    (write-solid-png png 8 8 0.1d0 0.2d0 0.8d0)
    (let* ((html (format nil "<p>Hi <img id='mid' src='~A' width='20' height='20' style='vertical-align:middle'> there</p>" png))
           (root (closure.engine:layout-html html :width 300 :height 80
                                            :stylesheet "body{margin:0;font-size:16px}"))
           (pic (closure.engine:find-box root "mid"))
           (hi nil))
      (labels ((walk (b)
                 (when b
                   (when (and (eq (closure.engine::box-kind b) :text)
                              (string= (or (closure.engine::box-text b) "") "Hi"))
                     (setf hi b))
                   (dolist (c (closure.engine::box-children b)) (walk c)))))
        (walk root))
      (check "middle image" (and pic hi))
      (when (and pic hi)
        (let* ((baseline (+ (closure.engine:box-y hi) (closure.engine::box-baseline hi)))
               (center (+ (closure.engine:box-y pic) (/ (closure.engine:box-height pic) 2d0))))
          (format t "~&  image center ~,1F baseline ~,1F~%" center baseline)
          (check "middle centers above the baseline"
                 (~ (- baseline center) 4d0)))))))

(defun test-inline-layout ()
  (let* ((wrap (closure.engine:layout-html
                "<div id='w' style='width:20px'>hello world</div>"
                :width 200 :height 80 :stylesheet "body{margin:0}"))
         (nowrap (closure.engine:layout-html
                  "<div id='n' style='width:20px;white-space:nowrap'>hello world</div>"
                  :width 200 :height 80 :stylesheet "body{margin:0}"))
         (pre (closure.engine:layout-html
               "<div id='p' style='white-space:pre'>a
b</div>"
               :width 200 :height 80 :stylesheet "body{margin:0}"))
         (super (closure.engine:layout-html
                 "<div id='s'>A<span style='vertical-align:super'>2</span></div>"
                 :width 200 :height 80 :stylesheet "body{margin:0}"))
         (w (closure.engine:find-box wrap "w"))
         (n (closure.engine:find-box nowrap "n"))
         (p (closure.engine:find-box pre "p"))
         (ys (text-ys super))
         (ay (cdr (assoc "A" ys :test #'string=)))
         (two (cdr (assoc "2" ys :test #'string=))))
    (check "wrap box" w)
    (check "words wrap" (and w (> (closure.engine:box-height w) 30d0)))
    (check "nowrap stays one line" (and n (< (closure.engine:box-height n) 28d0)))
    (check "pre keeps the newline" (and p (> (closure.engine:box-height p) 30d0)))
    (check "super is above the baseline"
           (and ay two (< two ay)))))

(defun test-sub-sup ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'>A<sup id='up'>2</sup><sub id='down'>2</sub></p>"
                :width 200 :height 80
                :stylesheet "body{margin:0} p{margin:0}"))
         (p (closure.engine:find-box root "p"))
         (ay (cdr (assoc "A" (text-ys root) :test #'string=)))
         (up (closure.engine:find-box root "up"))
         (down (closure.engine:find-box root "down")))
    (format t "~&  sub offset ~A~%" (and p down (- (closure.engine:box-y down) (closure.engine:box-y p))))
    (check "sup and sub boxes" (and up down ay p))
    (check "sup sits above the baseline"
           (and up ay (< (closure.engine:box-y up) ay)))
    (check "sub sits below the baseline"
           (and down ay (> (closure.engine:box-y down) ay)))
    (check "the subscript is lowered by size/5 plus a pixel"
           (and p down (~ (- (closure.engine:box-y down) (closure.engine:box-y p)) 10.52d0 0.05d0)))
    (check "the subscript line matches a browser"
           (and p (~ (closure.engine:box-height p) 26.52d0 0.02d0)))))

(defun test-mark ()
  (let* ((html "<mark id='m'>O</mark><mark><b id='b'>O</b></mark><span id='p'>O</span>")
         (out "/tmp/closure-engine-mark.png")
         (root (closure.engine:render-html html :width 200 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (marked (closure.engine:find-box root "m"))
         (nested (closure.engine:find-box root "b"))
         (plain (closure.engine:find-box root "p")))
    (labels ((yellow (box)
               (when box
                 (let ((x (floor (+ (closure.engine:box-x box)
                                    (/ (closure.engine:box-width box) 2d0))))
                       (y (floor (+ (closure.engine:box-y box)
                                    (closure.engine::box-baseline box)
                                    -7))))
                   (multiple-value-bind (r g b) (png-rgb out x y)
                     (and (> r 220) (> g 220) (< b 40)))))))
      (check "mark boxes" (and marked nested plain))
      (format t "~&  mark: ~A nested: ~A plain: ~A~%"
              (yellow marked) (yellow nested) (yellow plain))
      (check "mark paints yellow behind the text" (yellow marked))
      (check "mark paints yellow behind nested text" (yellow nested))
      (check "plain text has no highlight" (not (yellow plain))))))

(defun test-line-through ()
  (let* ((html "<s id='s'>O</s><del id='d'>O</del><span id='p'>O</span>")
         (out "/tmp/closure-engine-strike.png")
         (root (closure.engine:render-html html :width 200 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (struck (closure.engine:find-box root "s"))
         (deleted (closure.engine:find-box root "d"))
         (plain (closure.engine:find-box root "p")))
    (labels ((dark (box dy)
               (when box
                 (let ((x (floor (+ (closure.engine:box-x box)
                                    (/ (closure.engine:box-width box) 2d0))))
                       (y (floor (+ (closure.engine:box-y box)
                                    (closure.engine::box-baseline box)
                                    dy))))
                   (and (<= 0 y 39)
                        (multiple-value-bind (r g b) (png-rgb out x y)
                          (< (+ r g b) 500)))))))
      (check "strike boxes" (and struck deleted plain))
      (format t "~&  strike s: ~A del: ~A plain: ~A~%"
              (dark struck -5) (dark deleted -5) (dark plain -5))
      (check "s draws a line through the text" (dark struck -5))
      (check "del draws a line through the text" (dark deleted -5))
      (check "plain letter is not struck" (not (dark plain -5))))))

(defun test-list-markers ()
  (let* ((html "<ol><li id='a'>One</li><li id='b'>Two</li></ol>
                <ul><li id='c'>Dot</li><li id='d'>Dot</li></ul>")
         (out "/tmp/closure-engine-lists.png")
         (root (closure.engine:render-html html :width 220 :height 160 :output out
                                          :stylesheet "body{margin:0}"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c"))
         (d (closure.engine:find-box root "d")))
    (check "ordered markers" (and a b
                                  (string= (closure.engine::box-marker a) "1.")
                                  (string= (closure.engine::box-marker b) "2.")))
    (check "bullet marker" (and c d
                                (eq (closure.engine::box-marker c) :disc)
                                (eq (closure.engine::box-marker d) :disc)))
    (labels ((ink (box dx dy)
               (when box
                 (let ((x (floor (+ (closure.engine:box-x box) dx)))
                       (y (floor (+ (closure.engine:box-y box) dy))))
                   (and (<= 0 x 219) (<= 0 y 159)
                        (multiple-value-bind (r g b) (png-rgb out x y)
                          (< (+ r g b) 600))))))
             (mask (box)
               (loop for dy from 2 below 16
                     nconc (loop for dx from -22 below -4
                                 collect (if (ink box dx dy) 1 0)))))
      (let ((ma (mask a)) (mb (mask b)) (mc (mask c)) (md (mask d)))
        (format t "~&  list ink a ~D b ~D c ~D~%"
                (reduce #'+ ma) (reduce #'+ mb) (reduce #'+ mc))
        (check "ordered numbers are painted" (and (some #'plusp ma) (some #'plusp mb)))
        (check "item numbers differ" (not (equal ma mb)))
        (check "bullets are painted" (and (some #'plusp mc) (some #'plusp md)))
        (check "bullets match each other"
               (< (abs (- (reduce #'+ mc) (reduce #'+ md))) 12))
        (check "a number is not a bullet" (not (equal ma mc)))))))

(defun test-small ()
  (let* ((root (closure.engine:layout-html
                "<p>A<small id='s'>wide</small><span id='n'>wide</span></p>"
                :width 300 :height 40
                :stylesheet "body{margin:0} p{margin:0;font-size:16px}"))
         (small (closure.engine:find-box root "s"))
         (normal (closure.engine:find-box root "n")))
    (check "small and normal words" (and small normal))
    (check "small is narrower than the same word"
           (and small normal
                (< (closure.engine:box-width small) (closure.engine:box-width normal))))
    (check "small uses a smaller font"
           (and small
                (~ (closure.engine::computed-font-size (closure.engine::box-style small))
                   (/ 16d0 1.2d0))))
    (check "small text is 15px tall"
           (and small (~ (closure.engine:box-height small) 15d0)))))

(defun test-big ()
  (let* ((root (closure.engine:layout-html
                "<p>A<big id='b'>wide</big><span id='n'>wide</span></p>"
                :width 400 :height 40
                :stylesheet "body{margin:0} p{margin:0;font-size:16px}"))
         (big (closure.engine:find-box root "b"))
         (normal (closure.engine:find-box root "n")))
    (check "big and normal words" (and big normal))
    (check "big is wider than the same word"
           (and big normal
                (> (closure.engine:box-width big) (closure.engine:box-width normal))))
    (check "big uses a larger font"
           (and big
                (~ (closure.engine::computed-font-size (closure.engine::box-style big))
                   (* 16d0 1.2d0))))))

(defun test-u-ins ()
  (let* ((html "<u id='u'>O</u><ins id='i'>O</ins><span id='p'>O</span>")
         (out "/tmp/closure-engine-u.png")
         (root (closure.engine:render-html html :width 200 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (under (closure.engine:find-box root "u"))
         (inserted (closure.engine:find-box root "i"))
         (plain (closure.engine:find-box root "p")))
    (labels ((dark (box dy)
               (when box
                 (let ((x (floor (+ (closure.engine:box-x box)
                                    (/ (closure.engine:box-width box) 2d0))))
                       (y (floor (+ (closure.engine:box-y box)
                                    (closure.engine::box-baseline box)
                                    dy))))
                   (and (<= 0 y 39)
                        (multiple-value-bind (r g b) (png-rgb out x y)
                          (< (+ r g b) 500)))))))
      (check "underline boxes" (and under inserted plain))
      (format t "~&  underline u: ~A ins: ~A plain: ~A~%"
              (dark under 2) (dark inserted 2) (dark plain 2))
      (check "u underlines its text" (dark under 2))
      (check "ins underlines its text" (dark inserted 2))
      (check "plain text is not underlined" (not (dark plain 2))))))

(defun test-overline ()
  (let* ((html "<span id='o' style='text-decoration:overline'>x</span><span id='p'>x</span>")
         (out "/tmp/closure-engine-overline.png")
         (root (closure.engine:render-html html :width 120 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (over (closure.engine:find-box root "o"))
         (plain (closure.engine:find-box root "p")))
    (labels ((at (box dy)
               (when box
                 (png-rgb out
                          (floor (+ (closure.engine:box-x box)
                                    (/ (closure.engine:box-width box) 2d0)))
                          (floor (+ (closure.engine:box-y box)
                                    (closure.engine::box-baseline box)
                                    dy))))))
      (check "overline boxes" (and over plain))
      (let* ((ascent (round (closure.engine::font-ascent
                             16d0
                             (closure.engine::computed-font-family
                              (closure.engine::box-style over))
                             :normal :normal)))
             (sample (- 1 ascent)))
          (multiple-value-bind (r g b) (at over sample)
            (format t "~&  overline: ~D ~D ~D at ~D~%" r g b sample)
            (check "overline draws above the text" (and (< r 80) (< g 80) (< b 80))))
          (multiple-value-bind (r g b) (at plain sample)
            (format t "~&  plain above: ~D ~D ~D~%" r g b)
            (check "plain text has no overline" (and (> r 240) (> g 240) (> b 240))))))))

(defun test-underline-and-overline ()
  (let* ((html "<span id='b' style='text-decoration:underline overline'>x</span><span id='p'>x</span>")
         (out "/tmp/closure-engine-both-lines.png")
         (root (closure.engine:render-html html :width 120 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (both (closure.engine:find-box root "b"))
         (plain (closure.engine:find-box root "p")))
    (labels ((at (box dy)
               (when box
                 (png-rgb out
                          (floor (+ (closure.engine:box-x box)
                                    (/ (closure.engine:box-width box) 2d0)))
                          (floor (+ (closure.engine:box-y box)
                                    (closure.engine::box-baseline box)
                                    dy))))))
      (check "both decoration boxes" (and both plain))
      (let* ((ascent (round (closure.engine::font-ascent
                             16d0
                             (closure.engine::computed-font-family
                              (closure.engine::box-style both))
                             :normal :normal)))
             (above (- 1 ascent)))
        (multiple-value-bind (r g b) (at both above)
          (format t "~&  both overline: ~D ~D ~D~%" r g b)
          (check "both decorations draw the overline" (and (< r 80) (< g 80) (< b 80))))
        (multiple-value-bind (r g b) (at both 2)
          (format t "~&  both underline: ~D ~D ~D~%" r g b)
          (check "both decorations draw the underline" (and (< r 80) (< g 80) (< b 80))))
        (multiple-value-bind (r g b) (at plain above)
          (format t "~&  plain above both: ~D ~D ~D~%" r g b)
          (check "plain text has neither line above" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at plain 2)
          (format t "~&  plain below both: ~D ~D ~D~%" r g b)
          (check "plain text has neither line below" (and (> r 240) (> g 240) (> b 240))))))))

(defun test-underline-and-line-through ()
  (let* ((html "<span id='b' style='text-decoration:underline line-through'>O</span><span id='p'>O</span>")
         (out "/tmp/closure-engine-under-strike.png")
         (root (closure.engine:render-html html :width 120 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (both (closure.engine:find-box root "b"))
         (plain (closure.engine:find-box root "p")))
    (labels ((dark (box dy)
               (when box
                 (let ((x (floor (+ (closure.engine:box-x box)
                                    (/ (closure.engine:box-width box) 2d0))))
                       (y (floor (+ (closure.engine:box-y box)
                                    (closure.engine::box-baseline box)
                                    dy))))
                   (multiple-value-bind (r g b) (png-rgb out x y)
                     (and (< (+ r g b) 500)))))))
      (check "underline and strike boxes" (and both plain))
      (format t "~&  under+strike line ~A under ~A plain-line ~A plain-under ~A~%"
              (dark both -5) (dark both 2) (dark plain -5) (dark plain 2))
      (check "the line through the text is drawn" (dark both -5))
      (check "the underline is drawn" (dark both 2))
      (check "plain text is not struck" (not (dark plain -5)))
      (check "plain text is not underlined" (not (dark plain 2))))))

(defun test-text-decoration-color ()
  (let* ((html "<span id='u' style='text-decoration:underline;text-decoration-color:#cc0000'>O</span>")
         (out "/tmp/closure-engine-deco-color.png")
         (root (closure.engine:render-html html :width 80 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (box (closure.engine:find-box root "u"))
         (red nil)
         (black nil))
    (check "colored decoration box" box)
    (loop for y from 0 below 40 do
      (loop for x from 0 below 80 do
        (multiple-value-bind (r g b) (png-rgb out x y)
          (cond ((and (> r 180) (< g 80) (< b 80)) (push y red))
                ((and (< r 40) (< g 40) (< b 40)) (push y black))))))
    (format t "~&  decoration red ~D black ~D~%" (length red) (length black))
    (check "the decoration is red" red)
    (check "the glyph stays black" black)
    (when (and red black)
      (format t "~&  red y ~D black y ~D~%" (reduce #'min red) (reduce #'min black))
      (check "the red line sits below the glyph" (> (reduce #'min red) (reduce #'min black))))))

(defun test-text-underline-offset ()
  (let* ((html "<u id='a'>O</u><u id='b' style='text-underline-offset:6px'>O</u>")
         (out "/tmp/closure-engine-underline-offset.png")
         (root (closure.engine:render-html html :width 120 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (plain (closure.engine:find-box root "a"))
         (moved (closure.engine:find-box root "b")))
    (labels ((dark (box dy)
               (when box
                 (let ((x (floor (+ (closure.engine:box-x box)
                                    (/ (closure.engine:box-width box) 2d0))))
                       (y (floor (+ (closure.engine:box-y box)
                                    (closure.engine::box-baseline box)
                                    dy))))
                   (and (<= 0 y 39)
                        (multiple-value-bind (r g b) (png-rgb out x y)
                          (< (+ r g b) 500)))))))
      (check "offset underline boxes" (and plain moved))
      (format t "~&  default at 2 ~A at 8 ~A offset at 2 ~A at 8 ~A~%"
              (dark plain 2) (dark plain 8) (dark moved 2) (dark moved 8))
      (check "the default underline stays near the baseline" (dark plain 2))
      (check "the default underline is not far below" (not (dark plain 8)))
      (check "the offset underline leaves the default place" (not (dark moved 2)))
      (check "the offset underline sits further below" (dark moved 8)))))

(defun test-text-decoration-thickness ()
  (let* ((html "<u id='thin'>O</u><u id='thick' style='text-decoration-thickness:4px'>O</u>")
         (out "/tmp/closure-engine-deco-thick.png")
         (root (closure.engine:render-html html :width 120 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (thin (closure.engine:find-box root "thin"))
         (thick (closure.engine:find-box root "thick")))
    (labels ((rows (box)
               (loop for dy from 0 to 8
                     count (and box
                                (let ((x (floor (+ (closure.engine:box-x box)
                                                   (/ (closure.engine:box-width box) 2d0))))
                                      (y (floor (+ (closure.engine:box-y box)
                                                   (closure.engine::box-baseline box)
                                                   dy))))
                                  (and (<= 0 y 39)
                                       (multiple-value-bind (r g b) (png-rgb out x y)
                                         (< (+ r g b) 500))))))))
      (check "thickness boxes" (and thin thick))
      (let ((a (rows thin))
            (b (rows thick)))
        (format t "~&  thin rows ~D thick rows ~D~%" a b)
        (check "the default line is a hairline" (and (>= a 1) (<= a 2)))
        (check "a 4px decoration covers more rows" (> b a))))))

(defun test-text-decoration-dashed ()
  (let* ((html "<u id='solid'>MMMMMMMMMM</u><u id='dash' style='text-decoration-style:dashed'>MMMMMMMMMM</u>")
         (out "/tmp/closure-engine-deco-dash.png")
         (root (closure.engine:render-html html :width 400 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (solid (closure.engine:find-box root "solid"))
         (dash (closure.engine:find-box root "dash")))
    (labels ((along (box)
               (let ((dark 0) (light 0)
                     (y (floor (+ (closure.engine:box-y box)
                                  (closure.engine::box-baseline box) 2)))
                     (x0 (round (closure.engine:box-x box)))
                     (x1 (round (+ (closure.engine:box-x box) (closure.engine:box-width box)))))
                 (loop for x from x0 below x1
                       do (multiple-value-bind (r g b) (png-rgb out x y)
                            (if (< (+ r g b) 500) (incf dark) (incf light))))
                 (values dark light))))
      (check "dashed decoration boxes" (and solid dash))
      (multiple-value-bind (sd sl) (along solid)
        (multiple-value-bind (dd dl) (along dash)
          (format t "~&  solid dark ~D light ~D dash dark ~D light ~D~%" sd sl dd dl)
          (check "a solid underline is continuous" (and (> sd 8) (< sl 3)))
          (check "a dashed underline has gaps" (and (> dd 3) (> dl 3)))
          (check "a dashed underline has less ink than a solid one" (< dd sd)))))))

(defun test-text-decoration-wavy ()
  (let* ((html "<u id='solid'>MMMMMMMMMM</u><u id='wavy' style='text-decoration-style:wavy'>MMMMMMMMMM</u>")
         (out "/tmp/closure-engine-deco-wavy.png")
         (root (closure.engine:render-html html :width 400 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (solid (closure.engine:find-box root "solid"))
         (wavy (closure.engine:find-box root "wavy")))
    (labels ((row-dark (box dy)
               (let ((dark 0)
                     (y (floor (+ (closure.engine:box-y box)
                                  (closure.engine::box-baseline box) dy)))
                     (x0 (round (closure.engine:box-x box)))
                     (x1 (round (+ (closure.engine:box-x box)
                                   (closure.engine:box-width box)))))
                 (loop for x from x0 below x1
                       do (multiple-value-bind (r g b) (png-rgb out x y)
                            (when (< (+ r g b) 500) (incf dark))))
                 dark)))
      (check "wavy decoration boxes" (and solid wavy))
      (let ((solid-mid (row-dark solid 2))
            (solid-low (row-dark solid 6))
            (wavy-mid (row-dark wavy 2))
            (wavy-low (row-dark wavy 6)))
        (format t "~&  solid mid ~D low ~D wavy mid ~D low ~D~%"
                solid-mid solid-low wavy-mid wavy-low)
        (check "a solid underline stays on its row" (and (> solid-mid 8) (< solid-low 3)))
        (check "a wavy underline leaves the center row" (and (> wavy-mid 3) (< wavy-mid solid-mid)))
        (check "a wavy underline dips below the solid line" (> wavy-low 3))))))

(defun test-text-decoration-double ()
  (let* ((html "<u id='solid'>MMMMMMMMMM</u><u id='dbl' style='text-decoration-style:double'>MMMMMMMMMM</u>")
         (out "/tmp/closure-engine-deco-double.png")
         (root (closure.engine:render-html html :width 400 :height 48 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (solid (closure.engine:find-box root "solid"))
         (dbl (closure.engine:find-box root "dbl")))
    (labels ((row-dark (box dy)
               (let ((dark 0)
                     (y (floor (+ (closure.engine:box-y box)
                                  (closure.engine::box-baseline box) dy)))
                     (x0 (round (closure.engine:box-x box)))
                     (x1 (round (+ (closure.engine:box-x box)
                                   (closure.engine:box-width box)))))
                 (loop for x from x0 below x1
                       do (multiple-value-bind (r g b) (png-rgb out x y)
                            (when (< (+ r g b) 500) (incf dark))))
                 dark)))
      (check "double decoration boxes" (and solid dbl))
      (let ((solid-a (row-dark solid 2))
            (solid-b (row-dark solid 6))
            (double-a (row-dark dbl 2))
            (double-gap (row-dark dbl 4))
            (double-b (row-dark dbl 6)))
        (format t "~&  solid ~D/~D double ~D gap ~D second ~D~%"
                solid-a solid-b double-a double-gap double-b)
        (check "a solid underline is a single line" (and (> solid-a 8) (< solid-b 3)))
        (check "a double underline paints the first line" (> double-a 8))
        (check "a double underline paints a second line" (> double-b 8))
        (check "a gap separates the two underlines" (< double-gap 3))))))

(defun test-text-decoration-dotted ()
  (let* ((html "<u id='solid'>MMMMMMMMMM</u><u id='dot' style='text-decoration-style:dotted'>MMMMMMMMMM</u>")
         (out "/tmp/closure-engine-deco-dot.png")
         (root (closure.engine:render-html html :width 400 :height 40 :output out
                                          :stylesheet "body{margin:0;font-size:16px}"))
         (solid (closure.engine:find-box root "solid"))
         (dot (closure.engine:find-box root "dot")))
    (labels ((along (box)
               (let ((dark 0) (light 0)
                     (y (floor (+ (closure.engine:box-y box)
                                  (closure.engine::box-baseline box) 2)))
                     (x0 (round (closure.engine:box-x box)))
                     (x1 (round (+ (closure.engine:box-x box)
                                   (closure.engine:box-width box)))))
                 (loop for x from x0 below x1
                       do (multiple-value-bind (r g b) (png-rgb out x y)
                            (if (< (+ r g b) 500) (incf dark) (incf light))))
                 (values dark light))))
      (check "dotted decoration boxes" (and solid dot))
      (multiple-value-bind (sd sl) (along solid)
        (multiple-value-bind (dd dl) (along dot)
          (format t "~&  solid dark ~D light ~D dot dark ~D light ~D~%" sd sl dd dl)
          (check "a solid underline is continuous" (and (> sd 8) (< sl 3)))
          (check "a dotted underline has gaps" (and (> dd 3) (> dl 3)))
          (check "a dotted underline has less ink than a solid one" (< dd sd)))))))

(defun test-outline ()
  (let* ((html "<div id='a' style='margin-top:8px;width:40px;height:20px;outline:4px solid #cc0000'></div>
                <div id='b' style='width:40px;height:10px;background:#000'></div>")
         (out "/tmp/closure-engine-outline.png")
         (root (closure.engine:render-html html :width 80 :height 60 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "a"))
         (next (closure.engine:find-box root "b")))
    (check "outline box" (and box next))
    (check "outline does not change the width"
           (and box (~ (closure.engine:box-width box) 40d0)))
    (check "outline does not change the height"
           (and box (~ (closure.engine:box-height box) 20d0)))
    (check "outline does not push the next box"
           (and box next (~ (closure.engine:box-y next)
                            (+ (closure.engine:box-y box) (closure.engine:box-height box)))))
    (when box
      (multiple-value-bind (r g b)
          (png-rgb out 8 (floor (- (closure.engine:box-y box) 2d0)))
        (format t "~&  outline pixel: ~D ~D ~D~%" r g b)
        (check "outline is painted outside the box"
               (and (> r 180) (< g 40) (< b 40)))))))

(defun test-outline-dashed ()
  (let* ((html "<div id='solid' style='margin:8px;width:80px;height:20px;outline:4px solid #cc0000'></div>
                <div id='dash' style='margin:8px;width:80px;height:20px;outline:4px dashed #cc0000'></div>")
         (out "/tmp/closure-engine-outline-dash.png")
         (root (closure.engine:render-html html :width 120 :height 80 :output out
                                          :stylesheet "body{margin:0}"))
         (solid (closure.engine:find-box root "solid"))
         (dash (closure.engine:find-box root "dash")))
    (labels ((along (box)
               (let ((red 0) (gap 0)
                     (y (floor (- (closure.engine:box-y box) 2d0)))
                     (x0 (round (closure.engine:box-x box)))
                     (x1 (round (+ (closure.engine:box-x box)
                                   (closure.engine:box-width box)))))
                 (loop for x from x0 below x1
                       do (multiple-value-bind (r g b) (png-rgb out x y)
                            (if (and (> r 180) (< g 80) (< b 80))
                                (incf red)
                                (incf gap))))
                 (values red gap))))
      (check "dashed outline boxes" (and solid dash))
      (check "a dashed outline does not change the width"
             (and dash (~ (closure.engine:box-width dash) 80d0)))
      (multiple-value-bind (sr sg) (along solid)
        (multiple-value-bind (dr dg) (along dash)
          (format t "~&  solid red ~D gap ~D dash red ~D gap ~D~%" sr sg dr dg)
          (check "a solid outline is continuous" (and (> sr 40) (< sg 3)))
          (check "a dashed outline has gaps" (and (> dr 8) (> dg 8)))
          (check "a dashed outline has less ink than a solid one" (< dr sr)))))))

(defun test-outline-dotted ()
  (let* ((html "<div id='solid' style='margin:8px;width:80px;height:20px;outline:4px solid #cc0000'></div>
                <div id='dot' style='margin:8px;width:80px;height:20px;outline:4px dotted #cc0000'></div>")
         (out "/tmp/closure-engine-outline-dot.png")
         (root (closure.engine:render-html html :width 120 :height 80 :output out
                                          :stylesheet "body{margin:0}"))
         (solid (closure.engine:find-box root "solid"))
         (dot (closure.engine:find-box root "dot")))
    (labels ((along (box)
               (let ((red 0) (gap 0)
                     (y (floor (- (closure.engine:box-y box) 2d0)))
                     (x0 (round (closure.engine:box-x box)))
                     (x1 (round (+ (closure.engine:box-x box)
                                   (closure.engine:box-width box)))))
                 (loop for x from x0 below x1
                       do (multiple-value-bind (r g b) (png-rgb out x y)
                            (if (and (> r 180) (< g 80) (< b 80))
                                (incf red)
                                (incf gap))))
                 (values red gap))))
      (check "dotted outline boxes" (and solid dot))
      (check "a dotted outline does not change the width"
             (and dot (~ (closure.engine:box-width dot) 80d0)))
      (multiple-value-bind (sr sg) (along solid)
        (multiple-value-bind (dr dg) (along dot)
          (format t "~&  solid red ~D gap ~D dot red ~D gap ~D~%" sr sg dr dg)
          (check "a solid outline is continuous" (and (> sr 40) (< sg 3)))
          (check "a dotted outline has gaps" (and (> dr 8) (> dg 8)))
          (check "a dotted outline has less ink than a solid one" (< dr sr)))))))

(defun test-outline-double ()
  (let* ((html "<div id='solid' style='margin:16px;width:80px;height:20px;outline:6px solid #cc0000'></div>
                <div id='dbl' style='margin:16px;width:80px;height:20px;outline:6px double #cc0000'></div>")
         (out "/tmp/closure-engine-outline-double.png")
         (root (closure.engine:render-html html :width 120 :height 120 :output out
                                          :stylesheet "body{margin:0}"))
         (solid (closure.engine:find-box root "solid"))
         (dbl (closure.engine:find-box root "dbl")))
    (labels ((reds (box dy)
               (let ((red 0)
                     (y (floor (+ (closure.engine:box-y box) dy)))
                     (x0 (round (closure.engine:box-x box)))
                     (x1 (round (+ (closure.engine:box-x box)
                                   (closure.engine:box-width box)))))
                 (loop for x from x0 below x1
                       do (multiple-value-bind (r g b) (png-rgb out x y)
                            (when (and (> r 180) (< g 80) (< b 80))
                              (incf red))))
                 red)))
      (check "double outline boxes" (and solid dbl))
      (check "a double outline does not change the width"
             (and dbl (~ (closure.engine:box-width dbl) 80d0)))
      (let ((solid-near (reds solid -1))
            (solid-mid (reds solid -3))
            (solid-far (reds solid -5))
            (dbl-near (reds dbl -1))
            (dbl-mid (reds dbl -3))
            (dbl-far (reds dbl -5)))
        (format t "~&  solid ~D/~D/~D double ~D gap ~D outer ~D~%"
                solid-near solid-mid solid-far dbl-near dbl-mid dbl-far)
        (check "a solid outline fills its band" (and (> solid-near 40) (> solid-mid 40) (> solid-far 40)))
        (check "a double outline paints the inner line" (> dbl-near 40))
        (check "a double outline paints the outer line" (> dbl-far 40))
        (check "a gap separates the two outlines" (< dbl-mid 8))))))

(defun test-border-double ()
  (let* ((html "<div id='solid' style='margin:8px;width:80px;height:20px;border:6px solid #cc0000'></div>
                <div id='dbl' style='margin:8px;width:80px;height:20px;border:6px double #cc0000'></div>")
         (out "/tmp/closure-engine-border-double.png")
         (root (closure.engine:render-html html :width 140 :height 120 :output out
                                          :stylesheet "body{margin:0}"))
         (solid (closure.engine:find-box root "solid"))
         (dbl (closure.engine:find-box root "dbl")))
    (labels ((reds (box dy)
               (let ((red 0)
                     (y (floor (+ (closure.engine:box-y box) dy)))
                     (x0 (+ (round (closure.engine:box-x box)) 12))
                     (x1 (- (round (+ (closure.engine:box-x box)
                                      (closure.engine:box-width box)))
                            12)))
                 (loop for x from x0 below x1
                       do (multiple-value-bind (r g b) (png-rgb out x y)
                            (when (and (> r 180) (< g 80) (< b 80))
                              (incf red))))
                 red)))
      (check "double border boxes" (and solid dbl))
      (check "a double border keeps the same box width"
             (and solid dbl
                  (~ (closure.engine:box-width solid) (closure.engine:box-width dbl))))
      (let ((solid-outer (reds solid 1))
            (solid-mid (reds solid 3))
            (solid-inner (reds solid 5))
            (dbl-outer (reds dbl 1))
            (dbl-mid (reds dbl 3))
            (dbl-inner (reds dbl 5)))
        (format t "~&  solid ~D/~D/~D double ~D gap ~D inner ~D~%"
                solid-outer solid-mid solid-inner dbl-outer dbl-mid dbl-inner)
        (check "a solid border fills its band" (and (> solid-outer 40) (> solid-mid 40) (> solid-inner 40)))
        (check "a double border paints the outer edge" (> dbl-outer 40))
        (check "a double border paints the inner edge" (> dbl-inner 40))
        (check "a gap separates the two border edges" (< dbl-mid 8))))))

(defun test-border-groove ()
  (let* ((html "<div id='g' style='margin:8px;width:40px;height:20px;border:6px groove #808080'></div>")
         (out "/tmp/closure-engine-border-groove.png")
         (root (closure.engine:render-html html :width 80 :height 60 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "g")))
    (labels ((at (x y)
               (multiple-value-bind (r g b) (png-rgb out x y)
                 (format t "~&  ~D,~D ~D ~D ~D~%" x y r g b)
                 (+ r g b))))
      (check "groove border box" box)
      (when box
        (let* ((x (round (closure.engine:box-x box)))
               (y (round (closure.engine:box-y box)))
               (w (round (closure.engine:box-width box)))
               (h (round (closure.engine:box-height box)))
               (top (at (+ x (floor w 2)) (+ y 1)))
               (bottom (at (+ x (floor w 2)) (+ y h -2)))
               (left (at (+ x 1) (+ y (floor h 2))))
               (right (at (+ x w -2) (+ y (floor h 2)))))
          (format t "~&  groove top ~D bottom ~D left ~D right ~D~%" top bottom left right)
          (check "the top edge is darker than the bottom" (< top bottom))
          (check "the left edge is darker than the right" (< left right)))))))

(defun test-border-ridge ()
  (let* ((html "<div id='r' style='margin:8px;width:40px;height:20px;border:6px ridge #808080'></div>")
         (out "/tmp/closure-engine-border-ridge.png")
         (root (closure.engine:render-html html :width 80 :height 60 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "r")))
    (labels ((at (x y)
               (multiple-value-bind (r g b) (png-rgb out x y)
                 (+ r g b))))
      (check "ridge border box" box)
      (when box
        (let* ((x (round (closure.engine:box-x box)))
               (y (round (closure.engine:box-y box)))
               (w (round (closure.engine:box-width box)))
               (h (round (closure.engine:box-height box)))
               (top (at (+ x (floor w 2)) (+ y 1)))
               (bottom (at (+ x (floor w 2)) (+ y h -2)))
               (left (at (+ x 1) (+ y (floor h 2))))
               (right (at (+ x w -2) (+ y (floor h 2)))))
          (format t "~&  ridge top ~D bottom ~D left ~D right ~D~%" top bottom left right)
          (check "the top edge is lighter than the bottom" (> top bottom))
          (check "the left edge is lighter than the right" (> left right)))))))

(defun test-border-inset ()
  (let* ((html "<div id='n' style='margin:8px;width:40px;height:20px;border:6px inset #808080'></div>")
         (out "/tmp/closure-engine-border-inset.png")
         (root (closure.engine:render-html html :width 80 :height 60 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "n")))
    (labels ((at (x y)
               (multiple-value-bind (r g b) (png-rgb out x y)
                 (+ r g b))))
      (check "inset border box" box)
      (when box
        (let* ((x (round (closure.engine:box-x box)))
               (y (round (closure.engine:box-y box)))
               (w (round (closure.engine:box-width box)))
               (h (round (closure.engine:box-height box)))
               (top (at (+ x (floor w 2)) (+ y 1)))
               (bottom (at (+ x (floor w 2)) (+ y h -2)))
               (left (at (+ x 1) (+ y (floor h 2))))
               (right (at (+ x w -2) (+ y (floor h 2)))))
          (format t "~&  inset top ~D bottom ~D left ~D right ~D~%" top bottom left right)
          (check "the top edge is darker than the bottom" (< top bottom))
          (check "the left edge is darker than the right" (< left right)))))))

(defun test-border-outset ()
  (let* ((html "<div id='o' style='margin:8px;width:40px;height:20px;border:6px outset #808080'></div>")
         (out "/tmp/closure-engine-border-outset.png")
         (root (closure.engine:render-html html :width 80 :height 60 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "o")))
    (labels ((at (x y)
               (multiple-value-bind (r g b) (png-rgb out x y)
                 (+ r g b))))
      (check "outset border box" box)
      (when box
        (let* ((x (round (closure.engine:box-x box)))
               (y (round (closure.engine:box-y box)))
               (w (round (closure.engine:box-width box)))
               (h (round (closure.engine:box-height box)))
               (top (at (+ x (floor w 2)) (+ y 1)))
               (bottom (at (+ x (floor w 2)) (+ y h -2)))
               (left (at (+ x 1) (+ y (floor h 2))))
               (right (at (+ x w -2) (+ y (floor h 2)))))
          (format t "~&  outset top ~D bottom ~D left ~D right ~D~%" top bottom left right)
          (check "the top edge is lighter than the bottom" (> top bottom))
          (check "the left edge is lighter than the right" (> left right)))))))

(defun test-float-left ()
  (let ((png "/tmp/closure-float-left.png"))
    (write-solid-png png 20 20 0.2d0 0.4d0 0.8d0)
    (let* ((html (format nil "<img id='pic' src='~A' style='float:left' width='20' height='20'>Hi" png))
           (root (closure.engine:layout-html html :width 200 :height 40
                                            :stylesheet "body{margin:0}"))
           (pic (closure.engine:find-box root "pic"))
           (hi nil))
      (labels ((walk (b)
                 (when b
                   (when (and (eq (closure.engine::box-kind b) :text)
                              (string= (or (closure.engine::box-text b) "") "Hi"))
                     (setf hi b))
                   (dolist (c (closure.engine::box-children b)) (walk c)))))
        (walk root))
      (check "floated image" (and pic hi))
      (check "float sits at the left"
             (and pic (~ (closure.engine:box-x pic) 0d0)))
      (check "text starts to the right of the float"
             (and pic hi (>= (closure.engine:box-x hi)
                             (+ (closure.engine:box-x pic) (closure.engine:box-width pic) -0.5d0))))
      (check "text stays beside the float"
             (and pic hi (< (closure.engine:box-y hi)
                            (+ (closure.engine:box-y pic) (closure.engine:box-height pic))))))))

(defun test-float-right ()
  (let ((png "/tmp/closure-float-right.png"))
    (write-solid-png png 40 20 0.8d0 0.2d0 0.2d0)
    (let* ((html (format nil "<div id='box' style='width:200px'><img id='pic' src='~A' style='float:right' width='40' height='20'>Hi</div>" png))
           (root (closure.engine:layout-html html :width 240 :height 40
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "box"))
           (pic (closure.engine:find-box root "pic"))
           (hi nil))
      (labels ((walk (b)
                 (when b
                   (when (and (eq (closure.engine::box-kind b) :text)
                              (string= (or (closure.engine::box-text b) "") "Hi"))
                     (setf hi b))
                   (dolist (c (closure.engine::box-children b)) (walk c)))))
        (walk root))
      (check "right float and text" (and pic hi box))
      (check "float sits at the right"
             (and pic box (~ (closure.engine:box-x pic)
                             (- (+ (closure.engine:box-x box) (closure.engine:box-width box))
                                (closure.engine:box-width pic)))))
      (check "text stays left of the float"
             (and pic hi (< (+ (closure.engine:box-x hi) (closure.engine:box-width hi))
                            (+ (closure.engine:box-x pic) 1d0))))
      (check "text stays beside the right float"
             (and pic hi (< (closure.engine:box-y hi)
                            (+ (closure.engine:box-y pic) (closure.engine:box-height pic))))))))

(defun test-clear-both ()
  (let ((png "/tmp/closure-clear-both.png"))
    (write-solid-png png 40 30 0.2d0 0.5d0 0.3d0)
    (let* ((html (format nil "<img id='pic' src='~A' style='float:left' width='40' height='30'><div id='c' style='clear:both'>After</div>" png))
           (root (closure.engine:layout-html html :width 200 :height 80
                                            :stylesheet "body{margin:0} div{margin:0}"))
           (pic (closure.engine:find-box root "pic"))
           (cleared (closure.engine:find-box root "c")))
      (check "clear and float" (and pic cleared))
      (check "cleared block starts below the float"
             (and pic cleared
                  (>= (closure.engine:box-y cleared)
                      (+ (closure.engine:box-y pic) (closure.engine:box-height pic) -0.5d0))))
      (check "cleared block uses the content edge"
             (and cleared (~ (closure.engine:box-x cleared) 0d0))))))

(defun test-relative-text ()
  (let* ((html "<div id='a' style='position:relative;left:12px;top:8px'>Hi</div>")
         (root (closure.engine:layout-html html :width 200 :height 40
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "a"))
         (hi nil))
    (labels ((walk (b)
               (when b
                 (when (and (eq (closure.engine::box-kind b) :text)
                            (string= (or (closure.engine::box-text b) "") "Hi"))
                   (setf hi b))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (check "relative text" (and box hi))
    (check "text moves with the box"
           (and box hi (~ (closure.engine:box-x hi) (closure.engine:box-x box))
                (~ (closure.engine:box-y hi) (closure.engine:box-y box))))
    (check "box and text share the relative shift"
           (and box (~ (closure.engine:box-x box) 12d0)
                (~ (closure.engine:box-y box) 8d0)))))

(defun test-parent-child-margin ()
  (let* ((html "<div id='p'><div id='c' style='margin-top:20px;height:10px'></div></div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (parent (closure.engine:find-box root "p"))
         (child (closure.engine:find-box root "c")))
    (check "parent and child" (and parent child))
    (check "child starts at the parent top"
           (and parent child (~ (closure.engine:box-y child) (closure.engine:box-y parent))))
    (check "collapsed margin is outside the parent"
           (and parent (~ (closure.engine:box-y parent) 20d0)))))

(defun test-parent-child-bottom-margin ()
  (labels ((measure (html)
             (let* ((root (closure.engine:layout-html html :width 200 :height 80
                                                     :stylesheet "body{margin:0}"))
                    (parent (closure.engine:find-box root "p"))
                    (next (closure.engine:find-box root "n")))
               (values (and parent (closure.engine:box-height parent))
                       (and parent next
                            (- (closure.engine:box-y next)
                               (+ (closure.engine:box-y parent)
                                  (closure.engine:box-height parent))))))))
    (multiple-value-bind (h gap)
        (measure "<div id='p'><div id='c' style='margin-bottom:20px;height:10px'></div></div><div id='n' style='height:10px'></div>")
      (format t "~&  parent bottom: height ~A gap ~A~%" h gap)
      (check "parent height is the child height" (and h (~ h 10d0)))
      (check "child bottom margin sits outside the parent" (and gap (~ gap 20d0))))
    (multiple-value-bind (h gap)
        (measure "<div id='p' style='margin-bottom:10px'><div id='c' style='margin-bottom:30px;height:10px'></div></div><div id='n' style='height:10px'></div>")
      (format t "~&  larger bottom: height ~A gap ~A~%" h gap)
      (check "parent height stays the child height" (and h (~ h 10d0)))
      (check "the larger bottom margin wins" (and gap (~ gap 30d0))))))

(defun test-margin-separation ()
  (labels ((place (html)
             (let* ((root (closure.engine:layout-html html :width 200 :height 80
                                                     :stylesheet "body{margin:0}"))
                    (parent (closure.engine:find-box root "p"))
                    (child (closure.engine:find-box root "c")))
               (values parent child))))
    (multiple-value-bind (parent child)
        (place "<div id='p' style='padding-top:8px'><div id='c' style='margin-top:20px;height:10px'></div></div>")
      (format t "~&  padding separation: parent ~A child ~A height ~A~%"
              (and parent (closure.engine:box-y parent))
              (and child (closure.engine:box-y child))
              (and parent (closure.engine:box-height parent)))
      (check "padding keeps the parent at the origin"
             (and parent (~ (closure.engine:box-y parent) 0d0)))
      (check "padding keeps the child margin inside"
             (and parent child
                  (~ (closure.engine:box-y child)
                     (+ (closure.engine:box-y parent) 28d0))))
      (check "padding height includes the child margin"
             (and parent (~ (closure.engine:box-height parent) 38d0))))
    (multiple-value-bind (parent child)
        (place "<div id='p' style='border-top:4px solid #000'><div id='c' style='margin-top:20px;height:10px'></div></div>")
      (format t "~&  border separation: parent ~A child ~A height ~A~%"
              (and parent (closure.engine:box-y parent))
              (and child (closure.engine:box-y child))
              (and parent (closure.engine:box-height parent)))
      (check "border keeps the parent at the origin"
             (and parent (~ (closure.engine:box-y parent) 0d0)))
      (check "border keeps the child margin inside"
             (and parent child
                  (~ (closure.engine:box-y child)
                     (+ (closure.engine:box-y parent) 24d0))))
      (check "border height includes the child margin"
             (and parent (~ (closure.engine:box-height parent) 34d0))))))

(defun test-margin-bottom-separation ()
  (labels ((measure (html)
             (let* ((root (closure.engine:layout-html html :width 200 :height 120
                                                     :stylesheet "body{margin:0}"))
                    (parent (closure.engine:find-box root "p"))
                    (next (closure.engine:find-box root "n")))
               (values (and parent (closure.engine:box-height parent))
                       (and parent next
                            (- (closure.engine:box-y next)
                               (+ (closure.engine:box-y parent)
                                  (closure.engine:box-height parent))))))))
    (multiple-value-bind (h gap)
        (measure "<div id='p' style='padding-bottom:8px'><div id='c' style='margin-bottom:20px;height:10px'></div></div><div id='n' style='height:10px'></div>")
      (format t "~&  padding bottom separation: height ~A gap ~A~%" h gap)
      (check "padding-bottom includes the child margin" (and h (~ h 38d0)))
      (check "padding-bottom leaves no extra gap" (and gap (~ gap 0d0))))
    (multiple-value-bind (h gap)
        (measure "<div id='p' style='border-bottom:4px solid #000'><div id='c' style='margin-bottom:20px;height:10px'></div></div><div id='n' style='height:10px'></div>")
      (format t "~&  border bottom separation: height ~A gap ~A~%" h gap)
      (check "border-bottom includes the child margin" (and h (~ h 34d0)))
      (check "border-bottom leaves no extra gap" (and gap (~ gap 0d0))))))

(defun test-margin-collapse ()
  (labels ((gap (html)
             (let* ((root (closure.engine:layout-html html :width 200 :height 80
                                                     :stylesheet "body{margin:0}"))
                    (a (closure.engine:find-box root "a"))
                    (b (closure.engine:find-box root "b")))
               (and a b (- (closure.engine:box-y b)
                           (+ (closure.engine:box-y a) (closure.engine:box-height a)))))))
    (let ((equal (gap "<div id='a' style='height:10px;margin-bottom:20px'></div><div id='b' style='height:10px;margin-top:20px'></div>"))
          (larger (gap "<div id='a' style='height:10px;margin-bottom:10px'></div><div id='b' style='height:10px;margin-top:30px'></div>")))
      (format t "~&  collapsed gaps: equal ~A larger ~A~%" equal larger)
      (check "equal margins collapse to 20" (and equal (~ equal 20d0)))
      (check "the larger margin wins" (and larger (~ larger 30d0))))))

(defun test-margin-left-auto ()
  (let* ((html "<div id='p' style='width:200px'><div id='c' style='width:40px;height:10px;margin-left:auto'></div></div>")
         (root (closure.engine:layout-html html :width 300 :height 40
                                          :stylesheet "body{margin:0}"))
         (parent (closure.engine:find-box root "p"))
         (child (closure.engine:find-box root "c")))
    (check "right-aligned boxes" (and parent child))
    (check "child keeps its width" (and child (~ (closure.engine:box-width child) 40d0)))
    (check "margin-left auto sits on the right"
           (and parent child
                (~ (+ (closure.engine:box-x child) (closure.engine:box-width child))
                   (+ (closure.engine:box-x parent) (closure.engine:box-width parent)))))))

(defun test-auto-margin ()
  (let* ((html "<div id='p' style='width:200px'><div id='c' style='width:40px;height:10px;margin-left:auto;margin-right:auto'></div></div>")
         (root (closure.engine:layout-html html :width 300 :height 40
                                          :stylesheet "body{margin:0}"))
         (parent (closure.engine:find-box root "p"))
         (child (closure.engine:find-box root "c")))
    (check "auto margin boxes" (and parent child))
    (check "child keeps its width" (and child (~ (closure.engine:box-width child) 40d0)))
    (check "auto margins center the child"
           (and parent child
                (~ (closure.engine:box-x child)
                   (+ (closure.engine:box-x parent)
                      (/ (- (closure.engine:box-width parent) (closure.engine:box-width child)) 2d0)))))))

(defun test-relative ()
  (let* ((html "<div id='a' style='position:relative;left:12px;top:8px;width:40px;height:20px'></div>
                <div id='b' style='width:40px;height:10px'></div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b")))
    (check "relative boxes" (and a b))
    (check "relative shifts right" (and a (~ (closure.engine:box-x a) 12d0)))
    (check "relative shifts down" (and a (~ (closure.engine:box-y a) 8d0)))
    (check "next box keeps the static place"
           (and b (~ (closure.engine:box-x b) 0d0)
                (~ (closure.engine:box-y b) 20d0)))))

(defun test-float-below ()
  (let ((png "/tmp/closure-float-below.png"))
    (write-solid-png png 40 10 0.2d0 0.4d0 0.8d0)
    (let* ((html (format nil "<img id='pic' src='~A' style='float:left' width='40' height='10'>One<br>Two" png))
           (root (closure.engine:layout-html html :width 200 :height 80
                                            :stylesheet "body{margin:0}"))
           (pic (closure.engine:find-box root "pic"))
           (one nil)
           (two nil))
      (labels ((walk (b)
                 (when b
                   (when (eq (closure.engine::box-kind b) :text)
                     (cond
                       ((string= (or (closure.engine::box-text b) "") "One") (setf one b))
                       ((string= (or (closure.engine::box-text b) "") "Two") (setf two b))))
                   (dolist (c (closure.engine::box-children b)) (walk c)))))
        (walk root))
      (check "float and both lines" (and pic one two))
      (check "first line stays beside the float"
             (and pic one (>= (closure.engine:box-x one)
                              (+ (closure.engine:box-x pic) (closure.engine:box-width pic) -0.5d0))))
      (check "next line is below the float"
             (and pic two (>= (closure.engine:box-y two)
                              (+ (closure.engine:box-y pic) (closure.engine:box-height pic) -0.5d0))))
      (check "next line returns to the content edge"
             (and two (~ (closure.engine:box-x two) 0d0))))))

(defun test-two-shadows ()
  (let* ((html "<div id='a' style='width:20px;height:12px;background:#fff;box-shadow:20px 0 #cc0000, 0 16px #0000cc'></div>")
         (out "/tmp/closure-engine-two-shadows.png")
         (root (closure.engine:render-html html :width 60 :height 40 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "a")))
    (check "two-shadow box" box)
    (check "two shadows do not change the box"
           (and box (~ (closure.engine:box-width box) 20d0)
                (~ (closure.engine:box-height box) 12d0)))
    (multiple-value-bind (r g b) (png-rgb out 24 4)
      (format t "~&  first shadow pixel: ~D ~D ~D~%" r g b)
      (check "first shadow is painted" (and (> r 180) (< g 40) (< b 40))))
    (multiple-value-bind (r g b) (png-rgb out 4 20)
      (format t "~&  second shadow pixel: ~D ~D ~D~%" r g b)
      (check "second shadow is painted" (and (< r 40) (< g 40) (> b 180))))))

(defun test-inset-shadow ()
  (let* ((html "<div id='a' style='width:40px;height:24px;background:#fff;box-shadow:inset 6px 4px #cc0000'></div>
                <div id='b' style='width:40px;height:10px;background:#000'></div>")
         (out "/tmp/closure-engine-inset.png")
         (root (closure.engine:render-html html :width 80 :height 50 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "a"))
         (next (closure.engine:find-box root "b")))
    (check "inset box" (and box next))
    (check "inset shadow does not change the box"
           (and box (~ (closure.engine:box-width box) 40d0)
                (~ (closure.engine:box-height box) 24d0)))
    (check "inset shadow does not push the next box"
           (and box next (~ (closure.engine:box-y next)
                            (+ (closure.engine:box-y box) (closure.engine:box-height box)))))
    (multiple-value-bind (r g b) (png-rgb out 2 12)
      (format t "~&  inset edge pixel: ~D ~D ~D~%" r g b)
      (check "inset shadow paints inside the edge" (and (> r 180) (< g 40) (< b 40))))
    (multiple-value-bind (r g b) (png-rgb out 20 12)
      (format t "~&  inset center pixel: ~D ~D ~D~%" r g b)
      (check "inset shadow leaves the center" (and (> r 250) (> g 250) (> b 250))))))

(defun test-box-shadow ()
  (let* ((html "<div id='a' style='width:30px;height:20px;background:#fff;box-shadow:10px 8px #cc0000'></div>
                <div id='b' style='width:30px;height:10px;background:#000'></div>")
         (out "/tmp/closure-engine-shadow.png")
         (root (closure.engine:render-html html :width 80 :height 50 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "a"))
         (next (closure.engine:find-box root "b")))
    (check "shadow box" (and box next))
    (check "shadow does not change the box"
           (and box (~ (closure.engine:box-width box) 30d0)
                (~ (closure.engine:box-height box) 20d0)))
    (check "shadow does not push the next box"
           (and box next (~ (closure.engine:box-y next)
                            (+ (closure.engine:box-y box) (closure.engine:box-height box)))))
    (multiple-value-bind (r g b) (png-rgb out 36 14)
      (format t "~&  shadow pixel: ~D ~D ~D~%" r g b)
      (check "shadow is painted at the offset" (and (> r 180) (< g 40) (< b 40))))
    (multiple-value-bind (r g b) (png-rgb out 4 4)
      (check "the box covers its own shadow" (and (> r 250) (> g 250) (> b 250))))))

(defun test-outline-offset ()
  (let* ((html "<div id='a' style='margin-top:16px;width:40px;height:20px;outline:4px solid #cc0000;outline-offset:6px'></div>
                <div id='b' style='width:40px;height:10px;background:#000'></div>")
         (out "/tmp/closure-engine-outline-offset.png")
         (root (closure.engine:render-html html :width 80 :height 80 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "a"))
         (next (closure.engine:find-box root "b")))
    (check "offset outline box" (and box next))
    (check "offset does not change the box"
           (and box (~ (closure.engine:box-width box) 40d0)
                (~ (closure.engine:box-height box) 20d0)))
    (check "offset does not push the next box"
           (and box next (~ (closure.engine:box-y next)
                            (+ (closure.engine:box-y box) (closure.engine:box-height box)))))
    (when box
      (let ((y (floor (closure.engine:box-y box))))
        (multiple-value-bind (r g b) (png-rgb out 8 (- y 8))
          (format t "~&  offset outline pixel: ~D ~D ~D~%" r g b)
          (check "outline sits outside the offset" (and (> r 180) (< g 40) (< b 40))))
        (multiple-value-bind (r g b) (png-rgb out 8 (- y 2))
          (format t "~&  offset gap pixel: ~D ~D ~D~%" r g b)
          (check "offset leaves a gap at the border" (and (> r 250) (> g 250) (> b 250))))))))

(defun test-visibility-hidden ()
  (let* ((html "<div id='h' style='visibility:hidden;background:#000;width:40px;height:20px'></div>
                <div id='v' style='background:#000;width:40px;height:20px'></div>")
         (out "/tmp/closure-engine-hidden.png")
         (root (closure.engine:render-html html :width 80 :height 60 :output out
                                          :stylesheet "body{margin:0}"))
         (hidden (closure.engine:find-box root "h"))
         (visible (closure.engine:find-box root "v")))
    (check "hidden box keeps its size"
           (and hidden
                (~ (closure.engine:box-width hidden) 40d0)
                (~ (closure.engine:box-height hidden) 20d0)))
    (check "hidden box keeps its place"
           (and hidden visible (> (closure.engine:box-y visible) (closure.engine:box-y hidden))))
    (when hidden
      (multiple-value-bind (r g b) (png-rgb out 4 (floor (+ (closure.engine:box-y hidden) 4d0)))
        (format t "~&  hidden pixel: ~D ~D ~D~%" r g b)
        (check "hidden box paints nothing" (and (> r 250) (> g 250) (> b 250)))))
    (when visible
      (multiple-value-bind (r g b) (png-rgb out 4 (floor (+ (closure.engine:box-y visible) 4d0)))
        (format t "~&  visible pixel: ~D ~D ~D~%" r g b)
        (check "visible box still paints" (and (< r 20) (< g 20) (< b 20)))))))

(defun test-text-transform ()
  (let* ((root (closure.engine:layout-html
                "<span id='u' style='text-transform:uppercase'>ab</span><span id='l' style='text-transform:lowercase'>AB</span>"
                :width 200 :height 40
                :stylesheet "body{margin:0;font-size:16px}"))
         (up (closure.engine:find-box root "u"))
         (down (closure.engine:find-box root "l")))
    (check "transform boxes" (and up down))
    (check "uppercase draws capitals"
           (and up (string= (closure.engine::box-text up) "AB")))
    (check "lowercase draws small letters"
           (and down (string= (closure.engine::box-text down) "ab")))
    (check "uppercase is measured as capitals"
           (and up (~ (closure.engine:box-width up)
                      (closure.engine::text-advance "AB" 16d0 "serif" :normal :normal))))))

(defun test-word-spacing ()
  (let* ((root (closure.engine:layout-html
                "<p><span>AA BB</span> <span style='word-spacing:10px'>AA BB</span></p>"
                :width 500 :height 40
                :stylesheet "body{margin:0} p{margin:0;font-size:16px}"))
         (gaps nil)
         (words nil))
    (labels ((walk (b)
               (when b
                 (when (eq (closure.engine::box-kind b) :text)
                   (cond
                     ((string= (or (closure.engine::box-text b) "") "AA")
                      (push (closure.engine:box-width b) words))
                     ((string= (or (closure.engine::box-text b) "") " ")
                      (push (cons (or (closure.engine::computed-word-spacing
                                       (closure.engine::box-style b))
                                      0d0)
                                  (closure.engine:box-width b))
                            gaps))))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (let ((plain (find-if (lambda (g) (~ (car g) 0d0 0.1d0)) gaps))
          (wide (find-if (lambda (g) (~ (car g) 10d0)) gaps)))
      (check "word-spacing gaps" (and plain wide (cdr words)))
      (check "letters stay packed"
             (and (cdr words) (~ (first words) (second words))))
      (check "word-spacing widens the gap"
             (and plain wide (~ (- (cdr wide) (cdr plain)) 10d0))))))

(defun test-letter-spacing ()
  (let* ((root (closure.engine:layout-html
                "<span id='a'>MMMM</span><span id='b' style='letter-spacing:4px'>MMMM</span>"
                :width 400 :height 40
                :stylesheet "body{margin:0;font-size:16px}"))
         (plain (closure.engine:find-box root "a"))
         (spaced (closure.engine:find-box root "b")))
    (check "letter-spacing boxes" (and plain spaced))
    (check "letter-spacing widens the word"
           (and plain spaced
                (~ (- (closure.engine:box-width spaced) (closure.engine:box-width plain))
                   12d0)))))

(defun test-text-indent ()
  (let* ((root (closure.engine:layout-html
                "<div style='text-indent:24px'>One<br>Two</div>"
                :width 200 :height 80
                :stylesheet "body{margin:0}"))
         (one nil)
         (two nil))
    (labels ((walk (b)
               (when b
                 (when (eq (closure.engine::box-kind b) :text)
                   (cond
                     ((string= (or (closure.engine::box-text b) "") "One") (setf one b))
                     ((string= (or (closure.engine::box-text b) "") "Two") (setf two b))))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (check "indented lines" (and one two))
    (check "first line is indented"
           (and one (~ (closure.engine:box-x one) 24d0)))
    (check "following line stays at the content edge"
           (and two (~ (closure.engine:box-x two) 0d0)))
    (check "following line is below the first"
           (and one two (> (closure.engine:box-y two) (closure.engine:box-y one))))))

(defun test-address ()
  (let* ((root (closure.engine:layout-html
                "<address>Mail</address><p>Mail</p>"
                :width 200 :height 80
                :stylesheet "body{margin:0} address,p{margin:0;font-size:16px}"))
         (italic nil)
         (roman nil))
    (labels ((walk (b)
               (when b
                 (when (and (eq (closure.engine::box-kind b) :text)
                            (string= (or (closure.engine::box-text b) "") "Mail"))
                   (if (eq (closure.engine::computed-font-style (closure.engine::box-style b)) :italic)
                       (setf italic b)
                       (setf roman b)))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (check "address and paragraph words" (and italic roman))
    (check "address is italic"
           (and italic roman
                (eq (closure.engine::computed-font-style (closure.engine::box-style roman)) :normal)
                (~ (closure.engine:box-width italic)
                   (closure.engine::text-advance "Mail" 16d0 "serif" :normal :italic))))
    (labels ((find-tag (b)
               (when b
                 (when (string= (or (closure.engine::box-tag b) "") "address")
                   (return-from find-tag b))
                 (dolist (c (closure.engine::box-children b))
                   (let ((hit (find-tag c)))
                     (when hit (return-from find-tag hit)))))))
      (let ((block (find-tag root)))
        (check "address spans the content width"
               (and block (~ (closure.engine:box-width block) 200d0)))
        (check "address has no extra indent"
               (and block (~ (closure.engine:box-x block) 0d0)))))))

(defun test-ruby-rtc ()
  (let* ((root (closure.engine:layout-html
                "<p>X<ruby id='r'>WWW<rtc id='c'><rt>note</rt></rtc></ruby>Y</p>"
                :width 800 :height 80
                :stylesheet "body{margin:0} p{margin:0}"))
         (ruby (closure.engine:find-box root "r"))
         (rtc (closure.engine:find-box root "c")))
    (check "an rtc follows the base at full size"
           (and ruby rtc
                (~ (closure.engine:box-x ruby) 11.56d0 0.2d0)
                (~ (closure.engine:box-y ruby) 0d0)
                (~ (closure.engine:box-width ruby) 72.86d0 0.4d0)
                (~ (closure.engine:box-height ruby) 17d0)
                (~ (closure.engine:box-x rtc) 56.88d0 0.3d0)
                (~ (closure.engine:box-y rtc) 0d0)
                (~ (closure.engine:box-width rtc) 27.55d0 0.3d0)
                (~ (closure.engine:box-height rtc) 17d0)
                (~ (closure.engine::computed-font-size (closure.engine::box-style rtc)) 16d0)))))

(defun test-ruby-rb ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'>X<ruby id='r'><rb id='b'>WWW</rb><rt id='t'>note</rt></ruby>Y</p>"
                :width 800 :height 80
                :stylesheet "body{margin:0} p{margin:0}"))
         (rb (closure.engine:find-box root "b"))
         (ruby (closure.engine:find-box root "r")))
    (check "an rb box covers the ruby base"
           (and rb ruby
                (~ (closure.engine:box-x rb) 11.56d0 0.2d0)
                (~ (closure.engine:box-y rb) 8d0)
                (~ (closure.engine:box-width rb) 45.31d0 0.2d0)
                (~ (closure.engine:box-height rb) 17d0)
                (~ (closure.engine:box-x rb) (closure.engine:box-x ruby) 0.2d0)))))

(defun test-ruby ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'>X<ruby id='r'>字<rt id='t'>annotation</rt></ruby>Y</p>"
                :width 400 :height 80
                :stylesheet "body{margin:0} p{margin:0}"))
         (p (closure.engine:find-box root "p"))
         (ruby (closure.engine:find-box root "r"))
         (rt (closure.engine:find-box root "t"))
         (base nil))
    (labels ((walk (b)
               (when b
                 (when (and (eq (closure.engine::box-kind b) :text)
                            (string= (or (closure.engine::box-text b) "") "字"))
                   (setf base b))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (check "ruby boxes" (and p ruby rt base))
    (check "the annotation is half the base size"
           (and rt (~ (closure.engine::computed-font-size (closure.engine::box-style rt)) 8d0 0.1d0)))
    (check "the annotation sits above the base"
           (and rt base (< (closure.engine:box-y rt) (closure.engine:box-y base))))
    (check "a ruby line is taller than a normal line"
           (and p (> (closure.engine:box-height p) 20d0)
                (~ (closure.engine:box-height p) 26d0 0.6d0)))
    (check "a long annotation overhangs the ruby box"
           (and ruby rt (> (closure.engine:box-width rt) (closure.engine:box-width ruby))
                (< (closure.engine:box-x rt) (closure.engine:box-x ruby))))))

(defun test-table-size-attr ()
  (let* ((html "<table id='wide' width='180'><tr><td id='wc'>A</td></tr></table>
                <table id='half' width='50%'><tr><td>B</td></tr></table>
                <table id='cell'><tr><td id='c' width='80'>C</td></tr></table>
                <table id='row'><tr height='40'><td id='r'>D</td></tr></table>
                <table id='grown' height='50'><tr><td id='g'>E</td></tr></table>
                <table id='cols'><col width='60'><tr><td id='k'>F</td></tr></table>
                <table id='css' width='180' style='width:90px'><tr><td>G</td></tr></table>
                <hr id='rule' width='120'>")
         (root (closure.engine:layout-html html :width 200
                                          :stylesheet "body{margin:0}"))
         (wide (closure.engine:find-box root "wide"))
         (cell (closure.engine:find-box root "wc"))
         (half (closure.engine:find-box root "half"))
         (c (closure.engine:find-box root "c"))
         (r (closure.engine:find-box root "r"))
         (grown (closure.engine:find-box root "grown"))
         (g (closure.engine:find-box root "g"))
         (k (closure.engine:find-box root "k"))
         (css (closure.engine:find-box root "css"))
         (rule (closure.engine:find-box root "rule")))
    (check "sized tables" (and wide cell half c r grown g k css rule))
    (check "table width is the attribute"
           (and wide (~ (closure.engine:box-width wide) 180d0)))
    (check "the cell inside that table keeps the spacing"
           (and cell (~ (closure.engine:box-width cell) 176d0)))
    (check "a percentage table width uses the viewport"
           (and half (~ (closure.engine:box-width half) 100d0)))
    (check "a cell width is the content width"
           (and c (~ (closure.engine:box-width c) 82d0)))
    (check "a row height is the attribute"
           (and r (~ (closure.engine:box-height r) 40d0)))
    (check "a table height stretches the row"
           (and grown g
                (~ (closure.engine:box-height grown) 50d0)
                (~ (closure.engine:box-height g) 46d0)))
    (check "a col width is the column"
           (and k (~ (closure.engine:box-width k) 60d0)))
    (check "an author width wins over the attribute"
           (and css (~ (closure.engine:box-width css) 90d0)))
    (check "an hr width is the attribute"
           (and rule (~ (closure.engine:box-width rule) 122d0)))))

(defun test-valign-attr ()
  (let* ((html "<table>
                  <tr><td id='top' style='height:40px'>A</td></tr>
                  <tr><td id='mid' valign='middle' style='height:40px'>B</td></tr>
                  <tr valign='bottom'><td id='bot' style='height:40px'>C</td></tr>
                </table>")
         (root (closure.engine:layout-html html :width 200
                                          :stylesheet "body{margin:0}"))
         (top (closure.engine:find-box root "top"))
         (mid (closure.engine:find-box root "mid"))
         (bot (closure.engine:find-box root "bot")))
    (labels ((text-offset (cell)
               (labels ((find-text (b)
                          (when b
                            (if (eq (closure.engine::box-kind b) :text)
                                b
                                (some #'find-text (closure.engine::box-children b))))))
                 (let ((hit (find-text cell)))
                   (and hit (- (closure.engine:box-y hit) (closure.engine:box-y cell)))))))
      (let ((top-off (text-offset top))
            (mid-off (text-offset mid))
            (bot-off (text-offset bot)))
        (check "valign offsets" (and top-off mid-off bot-off))
        (check "valign=middle sits below the top"
               (and top-off mid-off (> mid-off (+ top-off 4d0))))
        (check "valign=bottom sits below the middle"
               (and mid-off bot-off (> bot-off (+ mid-off 4d0))))))))

(defun test-align-attr ()
  (let* ((html "<div id='d' align='center' style='width:200px'><span id='w'>Hi</span></div>
                <table id='t' align='center'><tr><td id='c' align='right' style='width:80px'>X</td></tr></table>
                <img id='i' align='right' src='file:///home/jmercouris/Source/closure/screenshots/square.png' width='20' height='20'>")
         (root (closure.engine:layout-html html :width 200
                                          :stylesheet "body{margin:0}"))
         (word (closure.engine:find-box root "w"))
         (table (closure.engine:find-box root "t"))
         (cell (closure.engine:find-box root "c"))
         (image (closure.engine:find-box root "i")))
    (check "align boxes" (and word table cell image))
    (check "align=center centers the line"
           (and word (> (closure.engine:box-x word) 60d0)))
    (check "align=center centers the table"
           (and table (> (closure.engine:box-x table) 20d0)
                (< (+ (closure.engine:box-x table) (closure.engine:box-width table)) 180d0)))
    (check "align=right puts the cell text on the right"
           (and cell
                (labels ((find-x (b)
                           (when b
                             (if (and (eq (closure.engine::box-kind b) :text)
                                      (string= (or (closure.engine::box-text b) "") "X"))
                                 b
                                 (some #'find-x (closure.engine::box-children b))))))
                  (let ((hit (find-x cell)))
                    (and hit (> (closure.engine:box-x hit)
                                (+ (closure.engine:box-x cell) 40d0)))))))
    (check "align=right floats an image"
           (and image (eq (closure.engine::computed-float
                           (closure.engine::box-style image))
                          :right)))))

(defun test-bgcolor ()
  (let* ((html "<table><tr><td id='c' bgcolor='#c00' style='width:40px;height:20px'>X</td>
                      <td id='s' bgcolor='#c00' style='width:40px;height:20px;background:#00c'>Y</td></tr></table>")
         (path "/tmp/closure-engine-bgcolor.png")
         (root (closure.engine:render-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"
                                          :output path))
         (cell (closure.engine:find-box root "c"))
         (styled (closure.engine:find-box root "s")))
    (check "bgcolor boxes" (and cell styled))
    (multiple-value-bind (r g b)
        (png-rgb path
                 (round (+ (closure.engine:box-x cell) 30))
                 (round (+ (closure.engine:box-y cell) 8)))
      (check "a cell bgcolor paints the cell"
             (and (> r 180) (< g 40) (< b 40))))
    (multiple-value-bind (r g b)
        (png-rgb path
                 (round (+ (closure.engine:box-x styled) 30))
                 (round (+ (closure.engine:box-y styled) 8)))
      (check "an author background wins over bgcolor"
             (and (< r 40) (< g 40) (> b 180))))))

(defun test-table-cell-attrs ()
  (let* ((html "<table id='t' cellspacing='10' cellpadding='8'><tr><td id='a'>One</td></tr></table>")
         (root (closure.engine:layout-html html :width 200
                                          :stylesheet "body{margin:0}"))
         (table (closure.engine:find-box root "t"))
         (cell (closure.engine:find-box root "a")))
    (check "cellspacing and cellpadding match Chromium"
           (and table cell
                (~ (closure.engine:box-width table) 62.65625d0 1d0)
                (~ (closure.engine:box-height table) 54d0 1d0)
                (~ (closure.engine:box-x cell) 10d0)
                (~ (closure.engine:box-y cell) 10d0)
                (~ (closure.engine:box-width cell) 42.65625d0 1d0)
                (~ (closure.engine:box-height cell) 34d0 1d0)))))

(defun test-table-bordercolor ()
  (let* ((html "<table id='t' border='1' bordercolor='#c00'><tr><td id='a'>A</td></tr></table>
                <table id='css' border='1' bordercolor='#c00' style='border-top-color:#00c;border-right-color:#00c;border-bottom-color:#00c;border-left-color:#00c'><tr><td id='e'>E</td></tr></table>
                <table id='plain' bordercolor='red'><tr><td id='f'>F</td></tr></table>")
         (path "/tmp/closure-engine-bordercolor.png")
         (root (closure.engine:render-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"
                                          :output path))
         (table (closure.engine:find-box root "t"))
         (cell (closure.engine:find-box root "a"))
         (css (closure.engine:find-box root "css"))
         (authored (closure.engine:find-box root "e"))
         (plain (closure.engine:find-box root "plain")))
    (check "bordercolor tables" (and table cell css authored plain))
    (let ((st (closure.engine::box-style table))
          (cs (closure.engine::box-style cell)))
      (check "bordercolor makes the frame and the cell solid red"
             (and st cs
                  (eq (closure.engine::computed-bts st) :solid)
                  (eq (closure.engine::computed-bts cs) :solid)
                  (> (first (closure.engine::computed-btc st)) 0.7d0)
                  (< (second (closure.engine::computed-btc st)) 0.1d0)
                  (> (first (closure.engine::computed-btc cs)) 0.7d0)
                  (< (second (closure.engine::computed-btc cs)) 0.1d0))))
    (let ((st (closure.engine::box-style css))
          (cs (closure.engine::box-style authored)))
      (check "an author border color wins over bordercolor"
             (and st cs
                  (< (first (closure.engine::computed-btc st)) 0.1d0)
                  (> (third (closure.engine::computed-btc st)) 0.7d0)
                  (< (first (closure.engine::computed-btc cs)) 0.1d0)
                  (> (third (closure.engine::computed-btc cs)) 0.7d0))))
    (let ((st (closure.engine::box-style plain)))
      (check "bordercolor without a border draws no frame"
             (and st (eq (closure.engine::computed-bts st) :none))))
    (let ((x (round (+ (closure.engine:box-x table) 2d0)))
          (y (round (closure.engine:box-y table))))
      (multiple-value-bind (r g b) (png-rgb path x y)
        (check "the bordercolor frame is painted red"
               (and (> r 180) (< g 40) (< b 40)))))))

(defun test-table-frame ()
  (let* ((html "<table id='above' frame='above' border='1'><tr><td id='a'>A</td></tr></table>
                <table id='sides' frame='hsides'><tr><td id='h'>H</td></tr></table>
                <table id='left' frame='lhs' border='1'><tr><td>L</td></tr></table>
                <table id='empty' frame='void' border='1'><tr><td id='v'>V</td></tr></table>
                <table id='vert' frame='vsides' border='2'><tr><td>S</td></tr></table>
                <table id='box' frame='box'><tr><td>B</td></tr></table>
                <table id='css' frame='above' style='border:2px solid #c00'><tr><td>C</td></tr></table>")
         (path "/tmp/closure-engine-table-frame.png")
         (root (closure.engine:render-html html :width 200 :height 200
                                          :stylesheet "body{margin:0}"
                                          :output path))
         (above (closure.engine:find-box root "above"))
         (a (closure.engine:find-box root "a"))
         (sides (closure.engine:find-box root "sides"))
         (h (closure.engine:find-box root "h"))
         (left (closure.engine:find-box root "left"))
         (empty (closure.engine:find-box root "empty"))
         (v (closure.engine:find-box root "v"))
         (vert (closure.engine:find-box root "vert"))
         (box (closure.engine:find-box root "box"))
         (css (closure.engine:find-box root "css")))
    (check "frame tables" (and above a sides h left empty v vert box css))
    (let ((st (closure.engine::box-style above))
          (cell (closure.engine::box-style a)))
      (check "frame=above draws only the top"
             (and st (~ (closure.engine::computed-bt st) 1d0 0.01d0)
                  (eq (closure.engine::computed-bts st) :solid)
                  (~ (closure.engine::computed-br st) 0d0 0.01d0)
                  (eq (closure.engine::computed-brs st) :hidden)
                  cell (eq (closure.engine::computed-bts cell) :inset))))
    (let ((st (closure.engine::box-style sides))
          (cell (closure.engine::box-style h)))
      (check "frame=hsides draws the top and bottom"
             (and st (~ (closure.engine::computed-bt st) 1d0 0.01d0)
                  (~ (closure.engine::computed-bb st) 1d0 0.01d0)
                  (~ (closure.engine::computed-bl st) 0d0 0.01d0)
                  cell (eq (closure.engine::computed-bts cell) :none))))
    (let ((st (closure.engine::box-style left)))
      (check "frame=lhs draws only the left"
             (and st (~ (closure.engine::computed-bl st) 1d0 0.01d0)
                  (eq (closure.engine::computed-bls st) :solid)
                  (~ (closure.engine::computed-bt st) 0d0 0.01d0))))
    (let ((st (closure.engine::box-style empty))
          (cell (closure.engine::box-style v)))
      (check "frame=void hides the frame and keeps the cell borders"
             (and st (~ (closure.engine::computed-bt st) 0d0 0.01d0)
                  (eq (closure.engine::computed-bts st) :hidden)
                  cell (eq (closure.engine::computed-bts cell) :inset))))
    (let ((st (closure.engine::box-style vert)))
      (check "frame=vsides uses the border width"
             (and st (~ (closure.engine::computed-bl st) 2d0 0.01d0)
                  (~ (closure.engine::computed-br st) 2d0 0.01d0)
                  (~ (closure.engine::computed-bt st) 0d0 0.01d0))))
    (let ((st (closure.engine::box-style box)))
      (check "frame=box draws every side"
             (and st (~ (closure.engine::computed-bt st) 1d0 0.01d0)
                  (~ (closure.engine::computed-bl st) 1d0 0.01d0)
                  (eq (closure.engine::computed-bts st) :solid))))
    (let ((st (closure.engine::box-style css)))
      (check "an author border wins over frame"
             (and st (~ (closure.engine::computed-bt st) 2d0 0.01d0)
                  (~ (closure.engine::computed-br st) 2d0 0.01d0)
                  (> (first (closure.engine::computed-btc st)) 0.7d0))))
    (let ((x (round (+ (closure.engine:box-x above) 2d0)))
          (y (round (closure.engine:box-y above))))
      (multiple-value-bind (r g b) (png-rgb path x y)
        (check "the top frame is painted" (and (< (abs (- r 128)) 40)
                                               (< (abs (- g 128)) 40)
                                               (< (abs (- b 128)) 40)))))))

(defun test-table-rules ()
  (let* ((html "<table id='rows' rules='rows' border='1'>
                  <tr><td id='a'>A</td><td id='b'>B</td></tr>
                  <tr><td id='c'>C</td></tr>
                </table>
                <table id='cols' rules='cols'><tr><td id='d'>D</td><td>E</td></tr></table>
                <table id='all' rules='all'><tr><td id='e'>F</td></tr></table>
                <table id='none' rules='none' border='1'><tr><td id='n'>N</td></tr></table>
                <table id='grp' rules='groups' border='1'>
                  <colgroup><col><col></colgroup>
                  <thead><tr><td id='h'>H</td><td id='i'>I</td></tr></thead>
                  <tbody><tr><td id='body'>B</td><td>C</td></tr></tbody>
                </table>
                <table id='css' rules='rows'><tr><td id='red' style='border:1px solid #c00'>R</td></tr></table>")
         (path "/tmp/closure-engine-table-rules.png")
         (root (closure.engine:render-html html :width 200 :height 160
                                          :stylesheet "body{margin:0}"
                                          :output path))
         (rows (closure.engine:find-box root "rows"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (d (closure.engine:find-box root "d"))
         (e (closure.engine:find-box root "e"))
         (n (closure.engine:find-box root "n"))
         (none (closure.engine:find-box root "none"))
         (h (closure.engine:find-box root "h"))
         (i (closure.engine:find-box root "i"))
         (red (closure.engine:find-box root "red")))
    (check "rules tables" (and rows a b d e n none h i red))
    (check "rules collapses the table"
           (and rows (eq (closure.engine::computed-border-collapse (closure.engine::box-style rows))
                         :collapse)))
    (let ((st (closure.engine::box-style a)))
      (check "rules=rows draws the horizontal borders"
             (and st (~ (closure.engine::computed-bt st) 1d0 0.01d0)
                  (~ (closure.engine::computed-bb st) 1d0 0.01d0)
                  (eq (closure.engine::computed-bts st) :solid)))
      (check "rules=rows leaves the vertical borders off"
             (and st (~ (closure.engine::computed-bl st) 0d0 0.01d0)
                  (~ (closure.engine::computed-br st) 0d0 0.01d0)
                  (eq (closure.engine::computed-bls st) :none))))
    (let ((st (closure.engine::box-style d)))
      (check "rules=cols draws the vertical borders"
             (and st (~ (closure.engine::computed-bl st) 1d0 0.01d0)
                  (~ (closure.engine::computed-br st) 1d0 0.01d0)
                  (~ (closure.engine::computed-bt st) 0d0 0.01d0))))
    (let ((st (closure.engine::box-style e)))
      (check "rules=all draws every side"
             (and st (~ (closure.engine::computed-bt st) 1d0 0.01d0)
                  (~ (closure.engine::computed-bl st) 1d0 0.01d0))))
    (let ((st (closure.engine::box-style n))
          (frame (closure.engine::box-style none)))
      (check "rules=none clears the cell borders and keeps the frame"
             (and st frame
                  (~ (closure.engine::computed-bt st) 0d0 0.01d0)
                  (eq (closure.engine::computed-bts frame) :outset))))
    (let ((hs (closure.engine::box-style h))
          (is (closure.engine::box-style i)))
      (check "rules=groups lines the header and the column group"
             (and hs is
                  (~ (closure.engine::computed-bb hs) 1d0 0.01d0)
                  (~ (closure.engine::computed-br hs) 0d0 0.01d0)
                  (~ (closure.engine::computed-br is) 1d0 0.01d0))))
    (let ((st (closure.engine::box-style red)))
      (check "an author border wins over rules"
             (and st (eq (closure.engine::computed-bts st) :solid)
                  (> (first (closure.engine::computed-btc st)) 0.7d0)
                  (< (second (closure.engine::computed-btc st)) 0.2d0))))
    (let ((x (round (+ (closure.engine:box-x a) 4d0)))
          (y (round (+ (closure.engine:box-y a) (closure.engine:box-height a) -1d0)))
          (gap-x (round (+ (closure.engine:box-x a) (closure.engine:box-width a))))
          (gap-y (round (+ (closure.engine:box-y a) 4d0))))
      (multiple-value-bind (r g b) (png-rgb path x y)
        (format t "~&  row rule pixel ~A,~A ~A ~A ~A~%" x y r g b)
        (check "the row rule is painted" (and (< r 180) (< g 180) (< b 180))))
      (multiple-value-bind (r g b) (png-rgb path gap-x gap-y)
        (format t "~&  column gap pixel ~A,~A ~A ~A ~A~%" gap-x gap-y r g b)
        (check "rules=rows does not paint a column rule"
               (not (and (< (abs (- r 128)) 24)
                         (< (abs (- g 128)) 24)
                         (< (abs (- b 128)) 24))))))))

(defun test-table-border-attr ()
  (let* ((html "<table id='t' border='1'><tr><td id='a'>One</td><td>Two</td></tr></table>")
         (path "/tmp/closure-engine-table-border.png")
         (root (closure.engine:render-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"
                                          :output path))
         (table (closure.engine:find-box root "t"))
         (cell (closure.engine:find-box root "a")))
    (check "a border attribute sizes the table like Chromium"
           (and table cell
                (~ (closure.engine:box-width table) 70.875d0 0.5d0)
                (~ (closure.engine:box-height table) 28d0)
                (~ (closure.engine:box-x cell) 3d0)
                (~ (closure.engine:box-y cell) 3d0)
                (~ (closure.engine:box-width cell) 30.65625d0 0.5d0)
                (~ (closure.engine:box-height cell) 22d0)))
    (multiple-value-bind (r g b) (png-rgb path 0 0)
      (check "the table frame is painted"
             (and (< r 230) (< g 230) (< b 230))))
    (multiple-value-bind (r g b) (png-rgb path 3 3)
      (check "the cell border is darker than the frame"
             (and (< r 80) (< g 80) (< b 80))))
    (multiple-value-bind (r g b) (png-rgb path 10 10)
      (check "the cell interior stays empty"
             (and (> r 240) (> g 240) (> b 240))))))

(defun test-frameset ()
  (let* ((root (closure.engine:layout-html
                "<frameset id='fs' cols='100,*'><frame id='a'></frame><frame id='b'></frame><noframes id='nf'>fallback</noframes></frameset>"
                :width 800 :height 400))
         (set (closure.engine:find-box root "fs"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (fallback (closure.engine:find-box root "nf"))
         (stars (closure.engine:layout-html
                 "<frameset cols='2*,*'><frame id='a'></frame><frame id='b'></frame></frameset>"
                 :width 800 :height 300))
         (sa (closure.engine:find-box stars "a"))
         (sb (closure.engine:find-box stars "b"))
         (nested (closure.engine:layout-html
                  "<frameset cols='100,*' border='0'><frame id='a'></frame><frameset rows='80,*' border='0'><frame id='r1'></frame><frame id='r2'></frame></frameset></frameset>"
                  :width 800 :height 400))
         (na (closure.engine:find-box nested "a"))
         (r1 (closure.engine:find-box nested "r1"))
         (r2 (closure.engine:find-box nested "r2")))
    (check "frameset, frames, and no fallback" (and set a b (not fallback) sa sb na r1 r2))
    (check "the default border leaves a 6px gap and the star takes the rest"
           (and set a b
                (~ (closure.engine:box-width set) 800d0)
                (~ (closure.engine:box-height set) 400d0)
                (~ (closure.engine:box-width a) 100d0)
                (~ (closure.engine:box-height a) 400d0)
                (~ (closure.engine:box-x b) 106d0)
                (~ (closure.engine:box-width b) 694d0)))
    (check "star columns split the space that remains after the gap"
           (and sa sb
                (~ (closure.engine:box-width sa) 529d0)
                (~ (closure.engine:box-x sb) 535d0)
                (~ (closure.engine:box-width sb) 265d0)))
    (let ((path "/tmp/closure-engine-frameset.png"))
      (closure.engine:render-html
       "<frameset cols='100,*'><frame></frame><frame></frame></frameset>"
       :width 200 :height 80
       :output path)
      (multiple-value-bind (r g b) (png-rgb path 100 20)
        (check "the frameset border starts dark gray"
               (and (< (abs (- r 170)) 12)
                    (< (abs (- g 170)) 12)
                    (< (abs (- b 170)) 12))))
      (multiple-value-bind (r g b) (png-rgb path 102 20)
        (check "the frameset border is gray"
               (and (< (abs (- r 208)) 12)
                    (< (abs (- g 208)) 12)
                    (< (abs (- b 208)) 12))))
      (multiple-value-bind (r g b) (png-rgb path 105 20)
        (check "the frameset border ends black"
               (and (< r 20) (< g 20) (< b 20))))
      (multiple-value-bind (r g b) (png-rgb path 90 20)
        (check "a frame stays empty beside the border"
               (and (> r 240) (> g 240) (> b 240)))))
    (check "a nested frameset fills its cell, and border 0 has no gap"
           (and na r1 r2
                (~ (closure.engine:box-width na) 100d0)
                (~ (closure.engine:box-x r1) 100d0)
                (~ (closure.engine:box-width r1) 700d0)
                (~ (closure.engine:box-height r1) 80d0)
                (~ (closure.engine:box-y r2) 80d0)
                (~ (closure.engine:box-height r2) 320d0)))))

(defun test-embed-paint ()
  (let ((path "/tmp/closure-engine-embed.png"))
    (closure.engine:render-html
     "<embed src='square.png' width='40' height='20'>"
     :width 80 :height 40
     :stylesheet "body{margin:0}"
     :base "file:///home/jmercouris/Source/closure/screenshots/page.html"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 10 10)
      (check "an embed paints its image" (and (> r 180) (< g 40) (< b 40))))))

(defun test-embed-size ()
  (let* ((root (closure.engine:layout-html
                "<embed id='e0' width='40' height='20'><embed id='e2' src='square.png'><embed id='e3' src='square.png' width='40' height='20'><iframe id='f0'></iframe><iframe id='f1' width='80' height='40'></iframe><iframe id='f2' width='80' height='40'></iframe><object id='o1' width='70' height='25'></object>"
                :width 1024 :height 200
                :stylesheet "body{margin:0} iframe#f2{width:50px;height:30px}"
                :base "file:///home/jmercouris/Source/closure/screenshots/page.html"))
         (empty (closure.engine:find-box root "e0"))
         (intrinsic (closure.engine:find-box root "e2"))
         (sized (closure.engine:find-box root "e3"))
         (frame (closure.engine:find-box root "f1"))
         (styled (closure.engine:find-box root "f2"))
         (object (closure.engine:find-box root "o1")))
    (check "embed and sized frames" (and intrinsic sized frame styled object))
    (check "an embed without a resource takes no space"
           (not empty))
    (check "an embed uses its image, or the width and height attributes"
           (and intrinsic sized
                (~ (closure.engine:box-width intrinsic) 20d0)
                (~ (closure.engine:box-height intrinsic) 20d0)
                (~ (closure.engine:box-width sized) 40d0)
                (~ (closure.engine:box-height sized) 20d0)))
    (check "iframe and object attributes beat the default size, and author CSS wins"
           (and frame styled object
                (~ (closure.engine:box-width frame) 84d0)
                (~ (closure.engine:box-height frame) 44d0)
                (~ (closure.engine:box-width styled) 54d0)
                (~ (closure.engine:box-height styled) 34d0)
                (~ (closure.engine:box-width object) 70d0)
                (~ (closure.engine:box-height object) 25d0)))))

(defun test-obsolete-tail ()
  (let* ((root (closure.engine:layout-html
                "<p id='t'><tt id='tt'>tt</tt> <blink id='bl'>blink</blink></p><p id='g'><image id='im' src='square.png' width='20' height='20'></image></p><isindex id='is' prompt='Find'><p id='a'>after</p><plaintext id='pl'>one
two</plaintext></body></html>"
                :width 1024 :height 300
                :stylesheet "body{margin:0}"
                :base "file:///home/jmercouris/Source/closure/screenshots/page.html"))
         (tt (closure.engine:find-box root "tt"))
         (blink (closure.engine:find-box root "bl"))
         (image (closure.engine:find-box root "im"))
         (line (closure.engine:find-box root "g"))
         (index (closure.engine:find-box root "is"))
         (after (closure.engine:find-box root "a"))
         (plain (closure.engine:find-box root "pl")))
    (check "tt, blink, image, and plaintext" (and tt blink image line after plain))
    (check "tt is 13px monospace and blink is an ordinary word"
           (and tt blink
                (~ (closure.engine::computed-font-size (closure.engine::box-style tt)) 13d0)
                (~ (closure.engine:box-width tt) 15.61d0 0.1d0)
                (~ (closure.engine:box-y tt) 17d0)
                (~ (closure.engine:box-width blink) 32.89d0 0.1d0)
                (~ (closure.engine:box-x blink) 19.61d0 0.1d0)))
    (check "image is an img and the line is 24 tall"
           (and image line
                (string= (closure.engine::box-tag image) "img")
                (~ (closure.engine:box-width image) 20d0)
                (~ (closure.engine:box-height image) 20d0)
                (~ (closure.engine:box-height line) 24d0)))
    (check "isindex adds no search form, and plaintext is two preformatted lines"
           (and (not index) after plain
                (~ (closure.engine:box-y after) 90d0)
                (~ (closure.engine:box-height after) 18d0)
                (~ (closure.engine:box-y plain) 124d0)
                (~ (closure.engine:box-height plain) 34d0)))))

(defun test-listing-abbr ()
  (let* ((root (closure.engine:layout-html
                "<p><abbr id='a' title='B'>abbr</abbr> <acronym id='c' title='A'>acro</acronym> <abbr id='b'>no</abbr></p><listing id='ls'>one
two</listing><xmp id='xm'>one
two</xmp><p id='n' style='width:40px'><nobr id='nb'>nobr text</nobr></p>"
                :width 1024 :height 400
                :stylesheet "body{margin:0}"))
         (abbr (closure.engine:find-box root "a"))
         (acro (closure.engine:find-box root "c"))
         (bare (closure.engine:find-box root "b"))
         (listing (closure.engine:find-box root "ls"))
         (xmp (closure.engine:find-box root "xm"))
         (narrow (closure.engine:find-box root "n"))
         (nobr-right (labels ((edge (b)
                                (let ((here (if (and b
                                                     (string= (or (closure.engine::box-tag b) "") "nobr")
                                                     (closure.engine::box-text b))
                                                (+ (closure.engine:box-x b)
                                                   (closure.engine:box-width b))
                                                0d0)))
                                  (dolist (c (and b (closure.engine::box-children b)))
                                    (setf here (max here (edge c))))
                                  here)))
                       (edge root))))
    (check "listing, xmp, abbr, and nobr" (and abbr acro bare listing xmp narrow))
    (check "a titled abbr and acronym are dotted underlines"
           (and abbr acro bare
                (eq (closure.engine::computed-text-decoration (closure.engine::box-style abbr))
                    :underline)
                (eq (closure.engine::computed-text-decoration-style (closure.engine::box-style abbr))
                    :dotted)
                (eq (closure.engine::computed-text-decoration-style (closure.engine::box-style acro))
                    :dotted)
                (eq (closure.engine::computed-text-decoration (closure.engine::box-style bare))
                    :none)
                (~ (closure.engine:box-width abbr) 28.44d0 0.1d0)
                (~ (closure.engine:box-width acro) 27.53d0 0.1d0)))
    (check "listing and xmp are two monospace lines with a 13px margin"
           (and listing xmp
                (~ (closure.engine:box-y listing) 50d0)
                (~ (closure.engine:box-height listing) 34d0)
                (~ (closure.engine:box-y xmp) 97d0)
                (~ (closure.engine:box-height xmp) 34d0)
                (~ (closure.engine::computed-font-size (closure.engine::box-style listing)) 13d0)))
    (check "nobr stays on one line and overflows a narrow paragraph"
           (and narrow
                (~ (closure.engine:box-height narrow) 18d0)
                (~ nobr-right 57.33d0 0.1d0)))))

(defun test-font-marquee ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'><strike id='s'>strike</strike></p><p id='g'><font id='f' size='7' color='red'>font</font> <font id='r' size='+2'>plus</font> <font id='m' size='3' face='monospace'>mono</font></p><p id='q'><marquee id='mq'>scroll</marquee></p><p><font id='o' size='7' style='font-size:12px'>x</font></p>"
                :width 1024 :height 300
                :stylesheet "body{margin:0} p{margin:0}"))
         (strike (closure.engine:find-box root "s"))
         (para (closure.engine:find-box root "g"))
         (font (closure.engine:find-box root "f"))
         (plus (closure.engine:find-box root "r"))
         (mono (closure.engine:find-box root "m"))
         (line (closure.engine:find-box root "q"))
         (marquee (closure.engine:find-box root "mq"))
         (over (closure.engine:find-box root "o"))
         (color (and font (closure.engine::computed-color (closure.engine::box-style font)))))
    (check "font, strike, and marquee" (and strike para font plus mono line marquee over))
    (check "strike is struck through at the same width as a browser"
           (and strike
                (eq (closure.engine::computed-text-decoration (closure.engine::box-style strike))
                    :line-through)
                (~ (closure.engine:box-width strike) 35.55d0 0.1d0)
                (~ (closure.engine:box-height strike) 17d0)))
    (check "font size 7 is 48px red and the line is 55 tall"
           (and font para color
                (~ (closure.engine::computed-font-size (closure.engine::box-style font)) 48d0)
                (~ (first color) 1d0 0.01d0)
                (~ (second color) 0d0 0.01d0)
                (~ (closure.engine:box-width font) 77.33d0 0.1d0)
                (~ (closure.engine:box-height font) 53d0)
                (~ (closure.engine:box-y font) 19d0)
                (~ (closure.engine:box-height para) 55d0)))
    (check "a relative font size and a monospace face use the browser tables"
           (and plus mono
                (~ (closure.engine::computed-font-size (closure.engine::box-style plus)) 24d0)
                (~ (closure.engine:box-y plus) 41d0)
                (~ (closure.engine::computed-font-size (closure.engine::box-style mono)) 13d0)
                (~ (closure.engine:box-width mono) 31.2d0 0.1d0)))
    (check "an author font-size beats the size attribute"
           (and over (~ (closure.engine::computed-font-size (closure.engine::box-style over)) 12d0)))
    (check "a marquee fills the line and the line is 22 tall"
           (and marquee line
                (~ (closure.engine:box-width marquee) 1024d0)
                (~ (closure.engine:box-height marquee) 18d0)
                (~ (closure.engine:box-height line) 22d0)))))

(defun test-replaced-widgets ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'><video id='v'></video> <canvas id='c'></canvas> <progress id='g'></progress> <meter id='m'></meter></p><audio id='a'></audio>"
                :width 1024 :height 220
                :stylesheet "body{margin:0} p{margin:0}"))
         (video (closure.engine:find-box root "v"))
         (canvas (closure.engine:find-box root "c"))
         (progress (closure.engine:find-box root "g"))
         (meter (closure.engine:find-box root "m"))
         (audio (closure.engine:find-box root "a"))
         (para (closure.engine:find-box root "p")))
    (check "replaced widgets" (and video canvas progress meter para))
    (check "the video line is 154px, the progress line gap does not stretch it"
           (and para (~ (closure.engine:box-height para) 154d0 0.2d0)))
    (check "video and canvas are 300 by 150"
           (and video canvas
                (~ (closure.engine:box-width video) 300d0)
                (~ (closure.engine:box-height video) 150d0)
                (~ (closure.engine:box-width canvas) 300d0)
                (~ (closure.engine:box-height canvas) 150d0)
                (~ (closure.engine:box-x canvas) 304d0)))
    (check "progress is 160 by 16 and sits 0.2em below the video baseline"
           (and progress video
                (~ (closure.engine:box-width progress) 160d0)
                (~ (closure.engine:box-height progress) 16d0)
                (~ (- (closure.engine:box-y progress) (closure.engine:box-y video)) 137.2d0 0.2d0)))
    (check "meter is 80 by 16 on that same line"
           (and meter progress
                (~ (closure.engine:box-width meter) 80d0)
                (~ (closure.engine:box-height meter) 16d0)
                (~ (closure.engine:box-y meter) (closure.engine:box-y progress) 0.2d0)))
    (check "audio is not shown"
           (not audio))))

(defun test-check-line ()
  (let* ((root (closure.engine:layout-html
                "<p id='p' style='margin:0'><input id='c' type='checkbox'><input id='s' type='submit' value='Go'></p>"
                :width 400 :height 80
                :stylesheet "body{margin:0}"))
         (para (closure.engine:find-box root "p"))
         (check (closure.engine:find-box root "c"))
         (submit (closure.engine:find-box root "s")))
    (check "checkbox line and controls" (and para check submit))
    (check "a checkbox margin stretches the line to 22px"
           (and para (~ (closure.engine:box-height para) 22d0)))
    (check "the checkbox sits 3px below the line"
           (and para check
                (~ (- (closure.engine:box-y check) (closure.engine:box-y para)) 3d0)))
    (check "the submit sits 1px below that line"
           (and para submit
                (~ (- (closure.engine:box-y submit) (closure.engine:box-y para)) 1d0 0.2d0)))))

(defun test-check-paint ()
  (let ((path "/tmp/closure-engine-checks.png"))
    (closure.engine:render-html
     "<input type='checkbox' checked><input type='checkbox'><input type='radio' checked>"
     :width 40 :height 50
     :stylesheet "body{margin:0} input{display:block}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 6 6)
      (declare (ignore g))
      (check "a checked checkbox is blue" (and (< r 40) (> b 200))))
    (multiple-value-bind (r g b) (png-rgb path 10 22)
      (check "an unchecked checkbox is white inside" (and (> r 250) (> g 250) (> b 250))))
    (multiple-value-bind (r g b) (png-rgb path 11 38)
      (declare (ignore g))
      (check "a checked radio is blue" (and (< r 40) (> b 200))))))

(defun test-select-arrow ()
  (let ((path "/tmp/closure-engine-select.png"))
    (closure.engine:render-html
     "<select><option>Blue</option></select>"
     :width 80 :height 30
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 39 10)
      (check "the select paints a dark arrow" (and (< r 40) (< g 40) (< b 40))))))

(defun test-button-border ()
  (let ((path "/tmp/closure-engine-button-border.png"))
    (closure.engine:render-html
     "<button>OK</button>"
     :width 80 :height 30
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 8 0)
      (check "a button edge is the gray hairline"
             (and (< (abs (- r 118)) 16)
                  (< (abs (- g 118)) 16)
                  (< (abs (- b 118)) 16))))
    (multiple-value-bind (r g b) (png-rgb path 8 1)
      (check "the button interior stays the button gray"
             (and (> r 220) (> g 220) (> b 220) (< r 250))))
    (multiple-value-bind (r g b) (png-rgb path 8 20)
      (check "the button bottom edge is the same hairline"
             (and (< (abs (- r 118)) 16)
                  (< (abs (- g 118)) 16)
                  (< (abs (- b 118)) 16))))
    (multiple-value-bind (r g b) (png-rgb path 0 0)
      (check "the button corner is rounded"
             (and (> r 200) (> g 200) (> b 200))))
    (closure.engine:render-html
     "<button>OK</button>"
     :width 200 :height 30
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 120 8)
      (check "the button background stops at the button"
             (and (> r 250) (> g 250) (> b 250))))))

(defun test-textarea-grip ()
  (let ((path "/tmp/closure-engine-textarea-grip.png"))
    (closure.engine:render-html
     "<textarea>note</textarea>"
     :width 200 :height 80
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 178 31)
      (check "a textarea paints the upper grip mark"
             (and (< (abs (- r 102)) 12)
                  (< (abs (- g 102)) 12)
                  (< (abs (- b 102)) 12))))
    (multiple-value-bind (r g b) (png-rgb path 172 37)
      (check "a textarea paints the lower grip mark"
             (and (< (abs (- r 102)) 12)
                  (< (abs (- g 102)) 12)
                  (< (abs (- b 102)) 12))))
    (multiple-value-bind (r g b) (png-rgb path 176 37)
      (check "a textarea paints the second grip mark"
             (and (< (abs (- r 102)) 12)
                  (< (abs (- g 102)) 12)
                  (< (abs (- b 102)) 12))))
    (multiple-value-bind (r g b) (png-rgb path 170 30)
      (check "the grip leaves the field white beside it"
             (and (> r 250) (> g 250) (> b 250))))))

(defun test-password-paint ()
  (let ((path "/tmp/closure-engine-password.png"))
    (closure.engine:render-html
     "<input type='password' value='secret'>"
     :width 200 :height 40
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 6 9)
      (check "a password paints a disc for each character"
             (and (< r 20) (< g 20) (< b 20))))
    (multiple-value-bind (r g b) (png-rgb path 11 9)
      (check "the next password disc is painted"
             (and (< r 20) (< g 20) (< b 20))))
    (multiple-value-bind (r g b) (png-rgb path 8 9)
      (check "password discs stay separate"
             (and (> r 240) (> g 240) (> b 240))))
    (closure.engine:render-html
     "<input type='password'>"
     :width 200 :height 40
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 6 9)
      (check "an empty password stays blank"
             (and (> r 240) (> g 240) (> b 240))))))

(defun test-image-input ()
  (let* ((path "/tmp/closure-engine-image-input.png")
         (root (closure.engine:render-html
                "<input id='i' type='image' src='file:///home/jmercouris/Source/closure/screenshots/square.png' alt='go'>"
                :width 80 :height 40
                :stylesheet "body{margin:0}"
                :output path))
         (box (closure.engine:find-box root "i")))
    (check "an image input is the image size"
           (and box
                (~ (closure.engine:box-width box) 20d0 1d0)
                (~ (closure.engine:box-height box) 20d0 1d0)))
    (multiple-value-bind (r g b) (png-rgb path 4 4)
      (check "an image input paints its image"
             (and (> r 180) (< g 40) (< b 40))))
    (multiple-value-bind (r g b) (png-rgb path 30 4)
      (check "an image input does not draw a text field"
             (and (> r 240) (> g 240) (> b 240))))))

(defun test-select-list ()
  (let* ((path "/tmp/closure-engine-select-list.png")
         (root (closure.engine:render-html
                "<select id='s' size='4'><option>Red</option><option selected>Blue</option><option>Green</option><option>Gold</option></select>"
                :width 120 :height 100
                :stylesheet "body{margin:0}"
                :output path))
         (box (closure.engine:find-box root "s")))
    (check "a list select is four rows tall"
           (and box (~ (closure.engine:box-height box) 70d0 1d0)
                (< (closure.engine:box-width box) 80d0)))
    (multiple-value-bind (r g b) (png-rgb path 40 8)
      (check "an unselected option stays white"
             (and (> r 240) (> g 240) (> b 240))))
    (multiple-value-bind (r g b) (png-rgb path 40 22)
      (check "the selected option is gray"
             (and (< (abs (- r 206)) 16)
                  (< (abs (- g 206)) 16)
                  (< (abs (- b 206)) 16))))
    (multiple-value-bind (r g b) (png-rgb path 10 69)
      (check "a list select keeps its border"
             (and (< (abs (- r 118)) 16)
                  (< (abs (- g 118)) 16)
                  (< (abs (- b 118)) 16))))))

(defun test-disclosure-marker ()
  (let ((path "/tmp/closure-engine-disclosure.png"))
    (closure.engine:render-html
     "<details><summary>Summary</summary>Hidden</details>"
     :width 200 :height 40
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 1 8)
      (check "a closed summary triangle starts at the left"
             (and (< r 20) (< g 20) (< b 20))))
    (multiple-value-bind (r g b) (png-rgb path 7 8)
      (check "the summary triangle reaches Chromium's tip"
             (and (< r 40) (< g 40) (< b 40))))
    (multiple-value-bind (r g b) (png-rgb path 12 8)
      (check "the summary triangle stops before the label"
             (and (> r 240) (> g 240) (> b 240))))))

(defun test-ordered-marker ()
  (let* ((path "/tmp/closure-engine-ol.png")
         (root (closure.engine:render-html
                "<ol><li id='a'>Alpha</li><li>Beta</li></ol>"
                :width 400 :height 80
                :stylesheet "body{margin:0;font-size:16px}"
                :output path))
         (item (closure.engine:find-box root "a")))
    (check "ordered item" item)
    (multiple-value-bind (r g b)
        (png-rgb path
                 (round (- (closure.engine:box-x item) 12))
                 (round (+ (closure.engine:box-y item) 6)))
      (check "the item number sits on Chromium's column"
             (< (+ r g b) 180)))
    (multiple-value-bind (r g b)
        (png-rgb path
                 (round (- (closure.engine:box-x item) 20))
                 (round (+ (closure.engine:box-y item) 6)))
      (check "the item number does not start early"
             (and (> r 240) (> g 240) (> b 240))))))

(defun test-file-input-paint ()
  (let ((path "/tmp/closure-engine-file.png"))
    (closure.engine:render-html
     "<div style='padding-top:0.1px'><input type='file' style='display:block'></div>"
     :width 280 :height 30
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 20 0)
      (check "the file button border uses the nearer pixel"
             (and (< (abs (- r 118)) 12)
                  (< (abs (- g 118)) 12)
                  (< (abs (- b 118)) 12))))
    (multiple-value-bind (r g b) (png-rgb path 20 6)
      (check "the file button is gray" (and (> r 200) (> g 200) (> b 200) (< r 250))))
    (multiple-value-bind (r g b) (png-rgb path 240 6)
      (check "the rest of the file input stays empty"
             (and (> r 250) (> g 250) (> b 250))))))

(defun test-range-color-paint ()
  (let ((path "/tmp/closure-engine-range.png"))
    (closure.engine:render-html
     "<input type='range' min='0' max='100' value='50'><input type='color' value='#ff0000'>"
     :width 200 :height 50
     :stylesheet "body{margin:0} input{display:block}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 20 2)
      (check "the range track is centered, not at the top"
             (and (> r 250) (> g 250) (> b 250))))
    (multiple-value-bind (r g b) (png-rgb path 0 5)
      (check "the range track has a rounded end"
             (and (> r 250) (> g 250) (> b 250))))
    (multiple-value-bind (r g b) (png-rgb path 20 8)
      (declare (ignore g))
      (check "the range fill is blue" (and (< r 40) (> b 200))))
    (multiple-value-bind (r g b) (png-rgb path 20 4)
      (check "the range fill has a fringe"
             (and (< (abs (- r 59)) 30)
                  (< (abs (- g 117)) 30)
                  (< (abs (- b 186)) 30))))
    (multiple-value-bind (r g b) (png-rgb path 110 4)
      (check "the range track has a gray fringe"
             (and (< (abs (- r 178)) 30)
                  (< (abs (- g 178)) 30)
                  (< (abs (- b 178)) 30))))
    (multiple-value-bind (r g b) (png-rgb path 64 2)
      (declare (ignore g))
      (check "the range thumb is a blue disc" (and (< r 40) (> b 200))))
    (multiple-value-bind (r g b) (png-rgb path 110 8)
      (check "the range track past the thumb is gray" (and (> r 200) (> g 200) (> b 200))))
    (multiple-value-bind (r g b) (png-rgb path 10 26)
      (check "the color input shows its value" (and (> r 200) (< g 40) (< b 40))))
    (multiple-value-bind (r g b) (png-rgb path 2 18)
      (check "the color swatch leaves a gray margin"
             (and (> r 220) (> g 220) (> b 220) (< r 250))))))

(defun test-gauge-paint ()
  (let ((path "/tmp/closure-engine-gauge.png"))
    (closure.engine:render-html
     "<progress value='0.5' max='1'></progress><meter value='0.5'></meter>"
     :width 200 :height 40
     :stylesheet "body{margin:0} progress,meter{display:block}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 10 8)
      (declare (ignore g))
      (check "the progress fill is blue" (and (< r 40) (> b 200))))
    (multiple-value-bind (r g b) (png-rgb path 0 5)
      (declare (ignore g b))
      (check "the progress track has a rounded end" (> r 200)))
    (multiple-value-bind (r g b) (png-rgb path 120 8)
      (check "the progress track is gray" (and (> r 200) (> g 200) (> b 200) (< r 250))))
    (multiple-value-bind (r g b) (png-rgb path 10 4)
      (check "the progress fill has a fringe"
             (and (< (abs (- r 59)) 30)
                  (< (abs (- g 117)) 30)
                  (< (abs (- b 186)) 30))))
    (multiple-value-bind (r g b) (png-rgb path 10 20)
      (check "the meter fringe is gray"
             (and (< (abs (- r 203)) 30)
                  (< (abs (- g 203)) 30)
                  (< (abs (- b 203)) 30))))
    (multiple-value-bind (r g b) (png-rgb path 10 24)
      (check "the meter fill is green" (and (< r 40) (> g 80) (< b 40))))))

(defun test-text-input-border ()
  (let ((path "/tmp/closure-engine-text-border.png"))
    (closure.engine:render-html
     "<input value='Hi'>"
     :width 200 :height 40
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 20 0)
      (check "a text input's top edge is gray"
             (and (< (abs (- r 118)) 8) (< (abs (- g 118)) 8) (< (abs (- b 118)) 8))))
    (multiple-value-bind (r g b) (png-rgb path 20 1)
      (check "the inside of a text input stays white"
             (and (> r 250) (> g 250) (> b 250))))))

(defun test-time-paint ()
  (let ((path "/tmp/closure-engine-time.png"))
    (closure.engine:render-html
     "<input type='time'>"
     :width 120 :height 40
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 4 0)
      (check "a time input has a gray border"
             (and (< (abs (- r 118)) 8) (< (abs (- g 118)) 8) (< (abs (- b 118)) 8))))
    (multiple-value-bind (r g b) (png-rgb path 8 12)
      (check "an empty time input shows a solid dash"
             (and (< (abs (- r 39)) 20)
                  (< (abs (- g 39)) 20)
                  (< (abs (- b 39)) 20))))
    (multiple-value-bind (r g b) (png-rgb path 90 6)
      (declare (ignore g b))
      (check "a time input paints a clock" (< r 80)))))

(defun test-date-input ()
  (let* ((root (closure.engine:layout-html
                "<input id='d' type='date'>"
                :width 400 :height 40
                :stylesheet "body{margin:0}"))
         (date (closure.engine:find-box root "d"))
         (path "/tmp/closure-engine-date.png"))
    (check "a date input is sized from its placeholder"
           (and date
                (~ (closure.engine:box-height date) 23d0)
                (> (closure.engine:box-width date) 90d0)
                (< (closure.engine:box-width date) 180d0)))
    (closure.engine:render-html
     "<input type='date'>"
     :width 200 :height 40
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 6 12)
      (check "an empty date input shows its placeholder"
             (and (< r 120) (< g 120) (< b 160))))
    (let ((ix (- (round (closure.engine:box-width date)) 12)))
      (multiple-value-bind (r g b) (png-rgb path ix 7)
        (declare (ignore g b))
        (check "a date input paints a calendar" (< r 40)))
      (multiple-value-bind (r g b) (png-rgb path ix 11)
        (check "the calendar page is white"
               (and (> r 240) (> g 240) (> b 240)))))))

(defun test-month-week ()
  (let* ((root (closure.engine:layout-html
                "<input id='d' type='date'><input id='m' type='month'><input id='w' type='week'>"
                :width 500 :height 40
                :stylesheet "body{margin:0}"))
         (date (closure.engine:find-box root "d"))
         (month (closure.engine:find-box root "m"))
         (week (closure.engine:find-box root "w")))
    (check "month and week fields" (and date month week))
    (check "a month field is wider than a date field"
           (and month date
                (~ (closure.engine:box-height month) 23d0)
                (> (closure.engine:box-width month) (closure.engine:box-width date))))
    (check "a week field is taller than a text line"
           (and week (~ (closure.engine:box-height week) 23d0)))
    (let ((local (closure.engine:find-box
                  (closure.engine:layout-html
                   "<input id='l' type='datetime-local'>"
                   :width 500 :height 40
                   :stylesheet "body{margin:0}")
                  "l")))
      (check "a datetime-local field is wider than a month field"
             (and local month
                  (~ (closure.engine:box-height local) 23d0)
                  (> (closure.engine:box-width local) (closure.engine:box-width month)))))))

(defun test-time-input ()
  (let* ((root (closure.engine:layout-html
                "<input id='t' type='time'>"
                :width 400 :height 40
                :stylesheet "body{margin:0}"))
         (time (closure.engine:find-box root "t")))
    (check "a time input is 103 by 24"
           (and time
                (~ (closure.engine:box-width time) 103d0)
                (~ (closure.engine:box-height time) 24d0)))))

(defun test-control-baseline ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'><input id='f' type='file'> <input id='r' type='range'> <input id='c' type='color'> <input id='t' type='time'></p>"
                :width 1024 :height 80
                :stylesheet "body{margin:0} p{margin:0}"))
         (p (closure.engine:find-box root "p"))
         (file (closure.engine:find-box root "f"))
         (range (closure.engine:find-box root "r"))
         (color (closure.engine:find-box root "c"))
         (time (closure.engine:find-box root "t")))
    (check "control baselines" (and p file range color time))
    (check "the control line is 28.5 tall"
           (~ (closure.engine:box-height p) 28.5d0 0.2d0))
    (check "a file input sits 6px below the color input"
           (~ (- (closure.engine:box-y file) (closure.engine:box-y color)) 6d0 0.2d0))
    (check "a range sits 5px below the color input"
           (~ (- (closure.engine:box-y range) (closure.engine:box-y color)) 5d0 0.2d0))
    (check "a range keeps a 2px side margin"
           (~ (- (closure.engine:box-x range)
                 (+ (closure.engine:box-x file) (closure.engine:box-width file)))
              6d0 1d0))
    (check "a time input sits 4.5px below the color input"
           (~ (- (closure.engine:box-y time) (closure.engine:box-y color)) 4.5d0 0.2d0))))

(defun test-input-widgets ()
  (let* ((root (closure.engine:layout-html
                "<input id='f' type='file'><input id='g' type='range'><input id='c' type='color'>"
                :width 800 :height 40
                :stylesheet "body{margin:0}"))
         (file (closure.engine:find-box root "f"))
         (range (closure.engine:find-box root "g"))
         (color (closure.engine:find-box root "c")))
    (check "file, range, and color" (and file range color))
    (check "a file input is 253 by 21"
           (and file
                (~ (closure.engine:box-width file) 253d0)
                (~ (closure.engine:box-height file) 21d0)))
    (check "a range is 129 by 16"
           (and range
                (~ (closure.engine:box-width range) 129d0)
                (~ (closure.engine:box-height range) 16d0)))
    (check "a color input is 50 by 27"
           (and color
                (~ (closure.engine:box-width color) 50d0)
                (~ (closure.engine:box-height color) 27d0)))))

(defun test-check-submit ()
  (let* ((root (closure.engine:layout-html
                "<input id='c' type='checkbox'><input id='r' type='radio'><input id='s' type='submit' value='Go'><input id='z' type='reset' value='Reset'>"
                :width 400 :height 40
                :stylesheet "body{margin:0}"))
         (check (closure.engine:find-box root "c"))
         (radio (closure.engine:find-box root "r"))
         (submit (closure.engine:find-box root "s"))
         (reset (closure.engine:find-box root "z")))
    (check "checkbox, radio, and buttons" (and check radio submit reset))
    (check "a checkbox and a radio are 13 by 13"
           (and check radio
                (~ (closure.engine:box-width check) 13d0)
                (~ (closure.engine:box-height check) 13d0)
                (~ (closure.engine:box-width radio) 13d0)
                (~ (closure.engine:box-height radio) 13d0)))
    (check "submit and reset shrink to their labels"
           (and submit reset
                (~ (closure.engine:box-width submit) 34.48d0 0.15d0)
                (~ (closure.engine:box-height submit) 21d0)
                (~ (closure.engine:box-width reset) 53.25d0 0.15d0)
                (~ (closure.engine:box-height reset) 21d0)))))

(defun test-open-details ()
  (let* ((root (closure.engine:layout-html
                "<details id='d' open><summary id='s'>Summary</summary><p id='h'>Hidden</p></details>"
                :width 800 :height 120
                :stylesheet "body{margin:0}"))
         (details (closure.engine:find-box root "d"))
         (summary (closure.engine:find-box root "s"))
         (hidden (closure.engine:find-box root "h")))
    (check "an open details shows its body"
           (and details summary hidden
                (~ (closure.engine:box-height details) 52d0)
                (~ (closure.engine:box-height summary) 18d0)
                (~ (closure.engine:box-y hidden) 34d0)
                (~ (closure.engine:box-height hidden) 18d0)
                (eq (closure.engine::box-marker summary) :disclosure-open)))))

(defun test-fieldset-border ()
  (let ((path "/tmp/closure-engine-fieldset.png"))
    (closure.engine:render-html
     "<fieldset><legend>Legend</legend>Field</fieldset>"
     :width 400 :height 80
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 4 1)
      (check "the fieldset border is not at the top of the box"
             (and (> r 250) (> g 250) (> b 250))))
    (multiple-value-bind (r g b) (png-rgb path 4 8)
      (check "the fieldset border crosses the legend"
             (and (< r 180) (< g 180) (< b 180) (> r 140))))
    (multiple-value-bind (r g b) (png-rgb path 4 9)
      (check "the inner fieldset edge is the border color"
             (and (> r 220) (> g 220) (> b 220) (< r 250))))
    (multiple-value-bind (r g b) (png-rgb path 4 52)
      (declare (ignore b))
      (check "the fieldset bottom edge is the dark groove"
             (and (> r 140) (< r 180) (> g 140) (< g 180))))
    (multiple-value-bind (r g b) (png-rgb path 4 53)
      (check "the fieldset bottom edge is the border color"
             (and (> r 220) (> g 220) (> b 220) (< r 250))))
    (multiple-value-bind (r g b) (png-rgb path 17 8)
      (check "the legend leaves a gap in the top border"
             (and (> r 250) (> g 250) (> b 250))))
    (multiple-value-bind (r g b) (png-rgb path 100 24)
      (check "the field text does not repeat the fieldset border"
             (and (> r 250) (> g 250) (> b 250))))))

(defun test-fieldset-details ()
  (let* ((root (closure.engine:layout-html
                "<fieldset id='f'><legend id='l'>Legend</legend>Field</fieldset>
                 <details id='d'><summary id='s'>Summary</summary>Hidden</details>
                 <table id='t'><tr><th id='h'>Head</th></tr></table>"
                :width 400 :height 200
                :stylesheet "body{margin:0}"))
         (field (closure.engine:find-box root "f"))
         (legend (closure.engine:find-box root "l"))
         (details (closure.engine:find-box root "d"))
         (summary (closure.engine:find-box root "s"))
         (table (closure.engine:find-box root "t"))
         (head (closure.engine:find-box root "h"))
         (texts nil))
    (labels ((walk (b)
               (when b
                 (when (and (eq (closure.engine::box-kind b) :text)
                            (member (closure.engine::box-text b) '("Summary" "Hidden" "Field")
                                    :test #'string=))
                   (push b texts))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (let ((sum (find "Summary" texts :key #'closure.engine::box-text :test #'string=))
          (hidden (find "Hidden" texts :key #'closure.engine::box-text :test #'string=))
          (field-text (find "Field" texts :key #'closure.engine::box-text :test #'string=)))
      (check "fieldset, details, and header cell" (and field legend details summary table head))
      (check "fieldset is inset 2px and the legend sits on its top edge"
             (and field legend
                  (~ (closure.engine:box-x field) 2d0)
                  (~ (closure.engine:box-y legend) (closure.engine:box-y field))))
      (check "the field text follows the legend and the top padding"
             (and field legend field-text
                  (> (closure.engine:box-y field-text)
                     (+ (closure.engine:box-y legend) (closure.engine:box-height legend)))))
      (check "a closed details hides everything but the summary"
             (and details summary (not hidden)
                  (~ (closure.engine:box-height details) (closure.engine:box-height summary))))
      (check "the summary marker indents its text"
             (and summary sum (> (closure.engine:box-x sum) (+ (closure.engine:box-x summary) 10d0))))
      (check "a header cell is centered and the table shrinks around it"
             (and head table
                  (eq (closure.engine::computed-text-align (closure.engine::box-style head)) :center)
                  (< (closure.engine:box-width table) 80d0)
                  (~ (- (closure.engine:box-width table) (closure.engine:box-width head)) 4d0 0.5d0)))
      (check "the legend shrinks to its text"
             (and legend (< (closure.engine:box-width legend) 80d0))))))

(defun test-margin-unit ()
  (let* ((root (closure.engine:layout-html
                "<h2 id='h'>Heading</h2>"
                :width 400 :height 80
                :stylesheet "body{margin:0}"))
         (h2 (closure.engine:find-box root "h")))
    (check "a used margin truncates to 1/64px"
           (and h2 (~ (closure.engine:box-y h2) 19.90625d0 0.001d0)))))

(defun test-smaller-headings ()
  (let* ((root (closure.engine:layout-html
                "<h4 id='h4'>Hi</h4><h5 id='h5'>Hi</h5><h6 id='h6'>Hi</h6>"
                :width 400 :height 240
                :stylesheet "body{margin:0}"))
         (h4 (closure.engine:find-box root "h4"))
         (h5 (closure.engine:find-box root "h5"))
         (h6 (closure.engine:find-box root "h6")))
    (check "smaller headings" (and h4 h5 h6))
    (check "h4 keeps the parent size and a 1.33em margin"
           (and h4 (~ (closure.engine:box-y h4) 21.28d0 0.05d0)
                (~ (closure.engine::computed-font-size (closure.engine::box-style h4)) 16d0 0.01d0)))
    (check "h5 is 0.83em with a 1.67em margin"
           (and h5 (~ (closure.engine::computed-font-size (closure.engine::box-style h5)) 13.28d0 0.02d0)))
    (check "h6 is 0.67em"
           (and h6 (~ (closure.engine::computed-font-size (closure.engine::box-style h6)) 10.72d0 0.02d0)))
    (check "a heading margin separates h5 from h4"
           (and h4 h5 (> (closure.engine:box-y h5)
                         (+ (closure.engine:box-y h4) (closure.engine:box-height h4)))))))

(defun test-br-and-center ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'>one<br>two</p><center id='c'>Mid</center><article id='a'>Article</article>"
                :width 200 :height 120
                :stylesheet "body{margin:0} p{margin:0}"))
         (p (closure.engine:find-box root "p"))
         (center (closure.engine:find-box root "c"))
         (article (closure.engine:find-box root "a"))
         (lines nil))
    (labels ((walk (b)
               (when b
                 (when (and (eq (closure.engine::box-kind b) :text)
                            (member (closure.engine::box-text b) '("one" "two" "Mid") :test #'string=))
                   (push b lines))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (let ((one (find "one" lines :key #'closure.engine::box-text :test #'string=))
          (two (find "two" lines :key #'closure.engine::box-text :test #'string=))
          (mid (find "Mid" lines :key #'closure.engine::box-text :test #'string=)))
      (check "br and center boxes" (and p center article one two mid))
      (check "br puts the next word on the following line"
             (and one two (> (closure.engine:box-y two) (closure.engine:box-y one))))
      (check "center puts the word in the middle of the line"
             (and center mid
                  (> (closure.engine:box-x mid) (+ (closure.engine:box-x center) 40d0))
                  (< (+ (closure.engine:box-x mid) (closure.engine:box-width mid))
                     (- (+ (closure.engine:box-x center) (closure.engine:box-width center)) 40d0))))
      (check "article is a full-width block"
             (and article (~ (closure.engine:box-width article) 200d0)
                  (~ (closure.engine:box-x article) 0d0)))
      (let* ((spaced (closure.engine:layout-html
                      "<p><b id='b'>bold</b> <i id='i'>italic</i></p>"
                      :width 300 :height 40
                      :stylesheet "body{margin:0} p{margin:0}"))
             (bold (closure.engine:find-box spaced "b"))
             (italic (closure.engine:find-box spaced "i")))
        (check "a space between two elements is kept"
               (and bold italic
                    (~ (- (closure.engine:box-x italic)
                          (+ (closure.engine:box-x bold) (closure.engine:box-width bold)))
                       4d0 0.2d0)))))))

(defun test-wbr ()
  (let* ((whole (closure.engine:layout-html
                 "<div id='w' style='width:40px'>Supercali</div>"
                 :width 200 :height 80 :stylesheet "body{margin:0}"))
         (split (closure.engine:layout-html
                 "<div id='b' style='width:40px'>Super<wbr>cali</div>"
                 :width 200 :height 80 :stylesheet "body{margin:0}"))
         (w (closure.engine:find-box whole "w"))
         (b (closure.engine:find-box split "b")))
    (check "unbroken word box" w)
    (check "wbr box" b)
    (check "a long word stays one line" (and w (< (closure.engine:box-height w) 28d0)))
    (check "wbr splits the word" (and b (> (closure.engine:box-height b) 30d0)))))

(defun test-nobr ()
  (let* ((wrap (closure.engine:layout-html
                "<div id='w' style='width:40px'>hello world</div>"
                :width 200 :height 80 :stylesheet "body{margin:0}"))
         (stuck (closure.engine:layout-html
                 "<div id='n' style='width:40px'><nobr>hello world</nobr></div>"
                 :width 200 :height 80 :stylesheet "body{margin:0}"))
         (w (closure.engine:find-box wrap "w"))
         (n (closure.engine:find-box stuck "n")))
    (check "wrapping box" w)
    (check "nobr box" n)
    (check "plain words wrap" (and w (> (closure.engine:box-height w) 30d0)))
    (check "nobr stays one line" (and n (< (closure.engine:box-height n) 28d0)))))

(defun test-text-overflow ()
  (labels ((text-of (root)
             (let ((found nil))
               (labels ((walk (b)
                          (when b
                            (when (eq (closure.engine::box-kind b) :text)
                              (push (or (closure.engine::box-text b) "") found))
                            (dolist (c (closure.engine::box-children b)) (walk c)))))
                 (walk root))
               (apply #'concatenate 'string (nreverse found)))))
    (let* ((html "<div id='box' style='width:40px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis'>HelloWorld</div>")
           (out "/tmp/closure-ellipsis.png")
           (root (closure.engine:render-html html :width 80 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "box"))
           (shown (text-of root))
           (short (text-of (closure.engine:layout-html
                            "<div style='width:40px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis'>Hi</div>"
                            :width 80 :height 30 :stylesheet "body{margin:0}"))))
      (format t "~&  ellipsis text: ~S short: ~S~%" shown short)
      (check "ellipsis box" box)
      (check "the box stays the given width" (and box (~ (closure.engine:box-width box) 40d0)))
      (check "the overflowing word ends in an ellipsis"
             (and (search (string #\…) shown)
                  (not (search "HelloWorld" shown))))
      (check "a short word is left unchanged" (string= short "Hi"))
      (let ((ink nil))
        (loop for x from 2 to 36
              do (multiple-value-bind (r g b) (png-rgb out x 12)
                   (declare (ignore g b))
                   (when (< r 80) (setf ink t))))
        (check "the clipped line is painted" ink))
      (multiple-value-bind (r g b) (png-rgb out 55 12)
        (declare (ignore g b))
        (check "the line does not paint past the box" (> r 250))))))

(defun test-overflow-wrap ()
  (labels ((texts (root)
             (let ((found nil))
               (labels ((walk (b)
                          (when b
                            (when (eq (closure.engine::box-kind b) :text)
                              (push b found))
                            (dolist (c (closure.engine::box-children b)) (walk c)))))
                 (walk root))
               (nreverse found))))
    (let* ((plain (closure.engine:layout-html
                   "<div id='p' style='width:40px'>HelloWorld</div>"
                   :width 120 :height 80 :stylesheet "body{margin:0}"))
           (broken (closure.engine:layout-html
                    "<div id='b' style='width:40px;overflow-wrap:break-word'>HelloWorld</div>"
                    :width 120 :height 80 :stylesheet "body{margin:0}"))
           (p (closure.engine:find-box plain "p"))
           (b (closure.engine:find-box broken "b"))
           (parts (texts broken))
           (joined (apply #'concatenate 'string
                          (mapcar (lambda (tb) (or (closure.engine::box-text tb) "")) parts))))
      (format t "~&  overflow-wrap pieces: ~S height ~A~%"
              joined (and b (closure.engine:box-height b)))
      (check "plain and broken boxes" (and p b parts))
      (check "a long word stays one line by default"
             (and p (< (closure.engine:box-height p) 28d0)))
      (check "break-word wraps the long word"
             (and b (> (closure.engine:box-height b) 30d0)))
      (check "the pieces still spell the word" (string= joined "HelloWorld"))
      (check "each piece fits in the box"
             (every (lambda (tb)
                      (and (<= (closure.engine:box-width tb)
                               (+ (closure.engine:box-width b) 0.5d0))
                           (>= (closure.engine:box-x tb) (- (closure.engine:box-x b) 0.5d0))
                           (<= (+ (closure.engine:box-x tb) (closure.engine:box-width tb))
                               (+ (closure.engine:box-x b) (closure.engine:box-width b) 0.5d0))))
                    parts)))))

(defun test-word-break ()
  (labels ((texts (root)
             (let ((found nil))
               (labels ((walk (b)
                          (when b
                            (when (eq (closure.engine::box-kind b) :text)
                              (push b found))
                            (dolist (c (closure.engine::box-children b)) (walk c)))))
                 (walk root))
               (nreverse found))))
    (let* ((root (closure.engine:layout-html
                  "<div id='b' style='width:40px;word-break:break-all'>Hi World</div>"
                  :width 120 :height 80 :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "b"))
           (parts (texts root))
           (joined (apply #'concatenate 'string
                          (mapcar (lambda (tb) (or (closure.engine::box-text tb) "")) parts)))
           (top (and parts (reduce #'min parts :key #'closure.engine:box-y)))
           (first-line (apply #'concatenate 'string
                              (mapcar (lambda (tb) (or (closure.engine::box-text tb) ""))
                                      (remove-if-not (lambda (tb)
                                                       (and top (~ (closure.engine:box-y tb) top)))
                                                     parts)))))
      (format t "~&  word-break pieces: ~S first: ~S height ~A~%"
              (mapcar #'closure.engine::box-text parts)
              first-line
              (and box (closure.engine:box-height box)))
      (check "break-all box" (and box parts))
      (check "break-all wraps onto more than one line"
             (and box (> (closure.engine:box-height box) 30d0)))
      (check "the pieces still spell the words" (string= joined "Hi World"))
      (check "the first line breaks into the next word"
             (and first-line (search "W" first-line) (> (length first-line) 3)))
      (check "each break-all piece fits in the box"
             (every (lambda (tb)
                      (<= (+ (closure.engine:box-x tb) (closure.engine:box-width tb))
                          (+ (closure.engine:box-x box) (closure.engine:box-width box) 0.5d0)))
                    parts)))))

(defun test-caption-wraps ()
  (labels ((line-count (box)
             (let ((n 0))
               (labels ((walk (b)
                          (when b
                            (when (eq (closure.engine::box-kind b) :line) (incf n))
                            (dolist (c (closure.engine::box-children b)) (walk c)))))
                 (walk box))
               n)))
    (let* ((html "<table id='t' style='border-collapse:collapse'>
                    <caption id='cap'>Top caption</caption>
                    <tr>
                      <td style='border:1px solid #000;padding:4px'>One</td>
                      <td style='border:1px solid #000;padding:4px'>Two</td>
                    </tr>
                  </table>")
           (root (closure.engine:layout-html html :width 400 :height 160
                                            :stylesheet "body{margin:0}"))
           (cap (closure.engine:find-box root "cap"))
           (lines (line-count cap)))
      (format t "~&  caption wrap: lines ~D height ~A width ~A~%"
              lines
              (and cap (closure.engine:box-height cap))
              (and cap (closure.engine:box-width cap)))
      (check "caption box" cap)
      (check "the caption wraps onto two lines above the cells"
             (= lines 2)))))

(defun test-pre-lines ()
  (let* ((html "<pre id='p'>mono one
mono two</pre>")
         (root (closure.engine:layout-html html :width 300 :height 80
                                          :stylesheet "body{margin:0} pre{margin:0}"))
         (pre (closure.engine:find-box root "p"))
         (lines nil))
    (labels ((walk (b)
               (when b
                 (when (and (eq (closure.engine::box-kind b) :text)
                            (search "mono" (or (closure.engine::box-text b) "")))
                   (push b lines))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (setf lines (nreverse lines))
    (format t "~&  pre lines ~D~%" (length lines))
    (check "pre keeps both lines" (= (length lines) 2))
    (check "each pre line is 17px"
           (and pre (~ (closure.engine:box-height pre) 34d0)))
    (check "the second pre line starts at the same edge"
           (and (= (length lines) 2)
                (~ (closure.engine:box-x (first lines))
                   (closure.engine:box-x (second lines)))
                (> (closure.engine:box-y (second lines))
                   (closure.engine:box-y (first lines)))))))

(defun test-kerning ()
  (let* ((two (closure.engine::text-advance "Two" 16d0 "serif" :normal :normal))
         (parts (+ (closure.engine::text-advance "T" 16d0 "serif" :normal :normal)
                   (closure.engine::text-advance "w" 16d0 "serif" :normal :normal)
                   (closure.engine::text-advance "o" 16d0 "serif" :normal :normal))))
    (format t "~&  kern Two ~A parts ~A~%" two parts)
    (check "Two matches the browser advance"
           (~ two 28.21875d0 0.001d0))
    (check "Two is tucked tighter than its letters"
           (< two parts))))

(defun test-normal-line-height ()
  (let* ((html "<p id='p'>Hello</p><h1 id='h'>Hi</h1>")
         (root (closure.engine:layout-html html :width 240 :height 120
                                          :stylesheet "body{margin:0} p,h1{margin:0}"))
         (p (closure.engine:find-box root "p"))
         (h (closure.engine:find-box root "h")))
    (format t "~&  normal line: p ~A h1 ~A~%"
            (and p (closure.engine:box-height p))
            (and h (closure.engine:box-height h)))
    (check "a paragraph line is 18px"
           (and p (~ (closure.engine:box-height p) 18d0)))
    (check "a heading line is 37px"
           (and h (~ (closure.engine:box-height h) 37d0)))))

(defun test-default-serif ()
  (labels ((line-count (box)
             (let ((n 0))
               (labels ((walk (b)
                          (when b
                            (when (eq (closure.engine::box-kind b) :line) (incf n))
                            (dolist (c (closure.engine::box-children b)) (walk c)))))
                 (walk box))
               n)))
    (let* ((html "<p id='p' style='width:220px'>Justified words wrap across the line and leave the last line packed at the start of the block.</p>")
           (root (closure.engine:layout-html html :width 400 :height 200
                                            :stylesheet "body{margin:0}"))
           (p (closure.engine:find-box root "p"))
           (lines (line-count p))
           (family (and p (closure.engine::computed-font-family
                           (closure.engine::box-style p)))))
      (format t "~&  default font: family ~A lines ~D~%" family lines)
      (check "the initial font is a serif" (equal '("serif") family))
      (check "the paragraph wraps onto three lines"
             (and p (= lines 3))))))

(defun test-justify ()
  (labels ((texts (root)
             (let ((found nil))
               (labels ((walk (b)
                          (when b
                            (when (and (eq (closure.engine::box-kind b) :text)
                                       (plusp (length (or (closure.engine::box-text b) "")))
                                       (not (string= (closure.engine::box-text b) " ")))
                              (push b found))
                            (dolist (c (closure.engine::box-children b)) (walk c)))))
                 (walk root))
               (nreverse found)))
           (lines-of (parts)
             (let ((groups nil))
               (dolist (tb parts)
                 (let ((group (find-if (lambda (g)
                                         (~ (closure.engine:box-y (car g))
                                            (closure.engine:box-y tb)))
                                       groups)))
                   (if group
                       (setf (cdr (last group)) (list tb))
                       (push (list tb) groups))))
               (nreverse groups)))
           (right-edge (group)
             (reduce #'max group
                     :key (lambda (tb)
                            (+ (closure.engine:box-x tb)
                               (closure.engine:box-width tb))))))
    (let* ((root (closure.engine:layout-html
                  "<div id='p' style='width:140px;text-align:justify'>alpha beta gamma delta epsilon zeta</div>"
                  :width 200 :height 80 :stylesheet "body{margin:0}"))
           (box (closure.engine:find-box root "p"))
           (groups (lines-of (texts root)))
           (first (car groups))
           (last-line (car (last groups)))
           (alone (closure.engine:layout-html
                   "<div id='s' style='width:300px;text-align:justify'>alpha beta</div>"
                   :width 320 :height 40 :stylesheet "body{margin:0}"))
           (single (closure.engine:find-box alone "s"))
           (single-words (texts alone)))
      (format t "~&  justify lines ~D first-right ~A last-right ~A box-right ~A~%"
              (length groups)
              (and first (right-edge first))
              (and last-line (right-edge last-line))
              (and box (+ (closure.engine:box-x box) (closure.engine:box-width box))))
      (check "justified lines" (and box first last-line (>= (length groups) 2)))
      (check "a wrapped line reaches the right edge"
             (and box first
                  (~ (right-edge first)
                     (+ (closure.engine:box-x box) (closure.engine:box-width box)))))
      (check "the last line stays packed at the start"
             (and box last-line
                  (~ (closure.engine:box-x (car last-line)) (closure.engine:box-x box))
                  (< (right-edge last-line)
                     (- (+ (closure.engine:box-x box) (closure.engine:box-width box)) 20d0))))
      (check "a single line is not stretched"
             (and single (>= (length single-words) 2)
                  (< (+ (closure.engine:box-x (second single-words))
                        (closure.engine:box-width (second single-words)))
                     (- (+ (closure.engine:box-x single) (closure.engine:box-width single)) 40d0)))))))

(defun test-collapsed-cell-half ()
  (let* ((html "<table id='t' style='border-collapse:collapse'>
                  <tr><td id='a' style='border:1px solid #000;padding:4px'>One</td></tr>
                </table>")
         (root (closure.engine:layout-html html :width 200 :height 60
                                          :stylesheet "body{margin:0}"))
         (table (closure.engine:find-box root "t"))
         (a (closure.engine:find-box root "a"))
         (text (closure.engine::text-advance "One" 16d0 "serif" :normal :normal)))
    (format t "~&  half cell ~A,~A ~Ax~A table ~A text ~A~%"
            (and a (closure.engine:box-x a)) (and a (closure.engine:box-y a))
            (and a (closure.engine:box-width a)) (and a (closure.engine:box-height a))
            (and table (closure.engine:box-x table)) text)
    (check "collapsed half cell" (and table a))
    (check "the cell starts half a border inside the table"
           (and table a (~ (- (closure.engine:box-x a) (closure.engine:box-x table)) 0.5d0)
                (~ (- (closure.engine:box-y a) (closure.engine:box-y table)) 0.5d0)))
    (check "the cell is content, padding, and one border"
           (and a (~ (closure.engine:box-width a) (+ text 8d0 1d0))
                (~ (closure.engine:box-height a) 27d0)))))

(defun test-em ()
  (let* ((root (closure.engine:layout-html
                "<p><em id='e'>italic</em><span id='s'>italic</span></p>"
                :width 200 :height 40
                :stylesheet "body{margin:0} p{margin:0}"))
         (em (closure.engine:find-box root "e"))
         (plain (closure.engine:find-box root "s")))
    (check "em box" (and em plain))
    (check "em is italic"
           (and em (eq (closure.engine::computed-font-style (closure.engine::box-style em)) :italic)
                (~ (closure.engine:box-height em) 17d0)))
    (check "plain text stays roman"
           (and plain (eq (closure.engine::computed-font-style (closure.engine::box-style plain)) :normal)))))

(defun test-border-collapse ()
  (let* ((html "<table id='t' style='border-collapse:collapse'>
                  <tr>
                    <td id='a' style='border:4px solid #000;width:20px;height:10px'></td>
                    <td id='b' style='border:4px solid #000;width:20px;height:10px'></td>
                  </tr>
                  <tr>
                    <td id='c' style='border:4px solid #000;width:20px;height:10px'></td>
                    <td id='d' style='border:4px solid #000;width:20px;height:10px'></td>
                  </tr>
                </table>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0} table{border-spacing:0}"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c")))
    (format t "~&  collapse: a ~A,~A ~Ax~A b ~A c ~A~%"
            (and a (closure.engine:box-x a)) (and a (closure.engine:box-y a))
            (and a (closure.engine:box-width a)) (and a (closure.engine:box-height a))
            (and b (closure.engine:box-x b))
            (and c (closure.engine:box-y c)))
    (check "collapsed cells" (and a b c))
    (check "adjacent cells meet at the shared vertical border"
           (and a b
                (~ (closure.engine:box-x b)
                   (+ (closure.engine:box-x a) (closure.engine:box-width a)))))
    (check "adjacent cells meet at the shared horizontal border"
           (and a c
                (~ (closure.engine:box-y c)
                   (+ (closure.engine:box-y a) (closure.engine:box-height a)))))))

(defun test-border-spacing ()
  (let* ((html "<table style='border-collapse:separate;border-spacing:12px 8px'>
                  <tr>
                    <td id='a' style='border:1px solid #000;width:20px;height:10px'></td>
                    <td id='b' style='border:1px solid #000;width:20px;height:10px'></td>
                  </tr>
                  <tr>
                    <td id='c' style='border:1px solid #000;width:20px;height:10px'></td>
                    <td id='d' style='border:1px solid #000;width:20px;height:10px'></td>
                  </tr>
                </table>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c")))
    (format t "~&  spacing: a ~A,~A b ~A c ~A~%"
            (and a (closure.engine:box-x a)) (and a (closure.engine:box-y a))
            (and b (closure.engine:box-x b))
            (and c (closure.engine:box-y c)))
    (check "spaced cells" (and a b c))
    (check "horizontal border-spacing separates the cells"
           (and a b
                (~ (- (closure.engine:box-x b)
                      (+ (closure.engine:box-x a) (closure.engine:box-width a)))
                   12d0)))
    (check "vertical border-spacing separates the rows"
           (and a c
                (~ (- (closure.engine:box-y c)
                      (+ (closure.engine:box-y a) (closure.engine:box-height a)))
                   8d0)))))

(defun test-colspan ()
  (let* ((html "<table id='t' style='width:90px;border-collapse:collapse'>
                  <tr>
                    <td id='span' colspan='2'></td>
                    <td id='after'></td>
                  </tr>
                  <tr>
                    <td id='a'></td>
                    <td id='b'></td>
                    <td id='c'></td>
                  </tr>
                </table>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (span (closure.engine:find-box root "span"))
         (after (closure.engine:find-box root "after"))
         (a (closure.engine:find-box root "a"))
         (c (closure.engine:find-box root "c")))
    (format t "~&  colspan: span ~A,~A after ~A a ~A c ~A~%"
            (and span (closure.engine:box-width span))
            (and span (closure.engine:box-x span))
            (and after (closure.engine:box-x after))
            (and a (closure.engine:box-width a))
            (and c (closure.engine:box-x c)))
    (check "spanned cells" (and span after a c))
    (check "the span covers two columns"
           (and span a (~ (closure.engine:box-width span)
                          (* 2d0 (closure.engine:box-width a)))))
    (check "the cell beside the span starts after it"
           (and span after
                (~ (closure.engine:box-x after)
                   (+ (closure.engine:box-x span) (closure.engine:box-width span)))))
    (check "the third column lines up under that cell"
           (and after c (~ (closure.engine:box-x c) (closure.engine:box-x after))))))

(defun test-rowspan ()
  (let* ((html "<table id='t' style='width:60px;border-collapse:collapse'>
                  <tr>
                    <td id='span' rowspan='2'></td>
                    <td id='b'></td>
                  </tr>
                  <tr>
                    <td id='c'></td>
                  </tr>
                </table>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (span (closure.engine:find-box root "span"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c")))
    (format t "~&  rowspan: span ~A,~A ~Ax~A b ~A c ~A,~A~%"
            (and span (closure.engine:box-x span)) (and span (closure.engine:box-y span))
            (and span (closure.engine:box-width span)) (and span (closure.engine:box-height span))
            (and b (closure.engine:box-x b))
            (and c (closure.engine:box-x c)) (and c (closure.engine:box-y c)))
    (check "rowspan cells" (and span b c))
    (check "the next row starts beside the span"
           (and b c (~ (closure.engine:box-x c) (closure.engine:box-x b))
                (> (closure.engine:box-y c) (closure.engine:box-y span))))
    (check "the span covers both rows"
           (and span c
                (~ (+ (closure.engine:box-y span) (closure.engine:box-height span))
                   (+ (closure.engine:box-y c) (closure.engine:box-height c)))))))

(defun test-table-valign ()
  (labels ((text-box (root word)
             (let ((found nil))
               (labels ((walk (b)
                          (when b
                            (when (and (eq (closure.engine::box-kind b) :text)
                                       (string= (or (closure.engine::box-text b) "") word))
                              (setf found b))
                            (dolist (c (closure.engine::box-children b)) (walk c)))))
                 (walk root))
               found)))
    (let* ((html "<table style='border-collapse:collapse'>
                    <tr>
                      <td id='tall' style='height:40px'>A</td>
                      <td id='low' style='vertical-align:bottom'>B</td>
                    </tr>
                  </table>")
           (root (closure.engine:layout-html html :width 200 :height 80
                                            :stylesheet "body{margin:0}"))
           (tall (closure.engine:find-box root "tall"))
           (low (closure.engine:find-box root "low"))
           (a (text-box root "A"))
           (b (text-box root "B")))
      (format t "~&  valign: A ~A tall ~A B ~A low ~A~%"
              (and a (closure.engine:box-y a))
              (and tall (list (closure.engine:box-y tall) (closure.engine:box-height tall)))
              (and b (closure.engine:box-y b))
              (and low (list (closure.engine:box-y low) (closure.engine:box-height low))))
      (check "valign cells" (and tall low a b))
      (check "both cells share the row height"
             (and tall low (~ (closure.engine:box-height low) (closure.engine:box-height tall))
                  (> (closure.engine:box-height tall) 30d0)))
      (check "the top cell keeps its text at the top"
             (and tall a (< (closure.engine:box-y a)
                            (+ (closure.engine:box-y tall) 16d0))))
      (check "bottom alignment sits the text on the bottom"
             (and low b
                  (~ (+ (closure.engine:box-y b) (closure.engine:box-height b))
                     (+ (closure.engine:box-y low) (closure.engine:box-height low))
                     6d0))))))

(defun test-table-valign-middle ()
  (labels ((text-box (root word)
             (let ((found nil))
               (labels ((walk (b)
                          (when b
                            (when (and (eq (closure.engine::box-kind b) :text)
                                       (string= (or (closure.engine::box-text b) "") word))
                              (setf found b))
                            (dolist (c (closure.engine::box-children b)) (walk c)))))
                 (walk root))
               found)))
    (let* ((html "<table style='border-collapse:collapse'>
                    <tr>
                      <td id='tall' style='height:40px'>A</td>
                      <td id='mid' style='vertical-align:middle'>B</td>
                    </tr>
                  </table>")
           (root (closure.engine:layout-html html :width 200 :height 80
                                            :stylesheet "body{margin:0}"))
           (tall (closure.engine:find-box root "tall"))
           (mid (closure.engine:find-box root "mid"))
           (a (text-box root "A"))
           (b (text-box root "B")))
      (format t "~&  valign middle: A ~A B ~A mid ~A~%"
              (and a (closure.engine:box-y a))
              (and b (closure.engine:box-y b))
              (and mid (list (closure.engine:box-y mid) (closure.engine:box-height mid))))
      (check "middle valign cells" (and tall mid a b))
      (check "the top cell keeps its text at the top"
             (and tall a (< (closure.engine:box-y a)
                            (+ (closure.engine:box-y tall) 16d0))))
      (check "middle alignment centers the text"
             (and mid b
                  (let ((cell-mid (+ (closure.engine:box-y mid)
                                     (/ (closure.engine:box-height mid) 2d0)))
                        (text-mid (+ (closure.engine:box-y b)
                                     (/ (closure.engine:box-height b) 2d0))))
                    (~ text-mid cell-mid 4d0)))))))

(defun test-table-auto-width ()
  (let* ((html "<table id='t' style='border-collapse:collapse'>
                  <caption id='cap'>Top caption</caption>
                  <tr>
                    <td id='a' style='border:1px solid #000;padding:4px'>One</td>
                    <td id='b' style='border:1px solid #000;padding:4px'>Two</td>
                  </tr>
                </table>
                <table id='word'>
                  <caption id='long'>Supercalifragilistic</caption>
                  <tr><td id='tiny'>I</td></tr>
                </table>
                <table id='bot' style='border-collapse:collapse'>
                  <caption id='bc' style='caption-side:bottom'>Bottom caption</caption>
                  <tr><td style='border:1px solid #000;padding:4px'>Cell</td></tr>
                </table>
                <table id='wide' style='width:180px'>
                  <tr><td id='c'>One</td><td id='d'>Two</td></tr>
                </table>")
         (root (closure.engine:layout-html html :width 400 :height 240
                                          :stylesheet "body{margin:0}"))
         (table (closure.engine:find-box root "t"))
         (cap (closure.engine:find-box root "cap"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (word (closure.engine:find-box root "word"))
         (long (closure.engine:find-box root "long"))
         (tiny (closure.engine:find-box root "tiny"))
         (bc (closure.engine:find-box root "bc"))
         (wide (closure.engine:find-box root "wide")))
    (format t "~&  auto table: table ~A cap ~Ax~A a ~A b ~A word ~A tiny ~A wide ~A~%"
            (and table (closure.engine:box-width table))
            (and cap (closure.engine:box-width cap))
            (and cap (closure.engine:box-height cap))
            (and a (closure.engine:box-width a))
            (and b (closure.engine:box-x b))
            (and word (closure.engine:box-width word))
            (and tiny (closure.engine:box-width tiny))
            (and wide (closure.engine:box-width wide)))
    (check "auto-width table" (and table cap a b word long tiny wide))
    (check "an auto table stays as wide as its cells"
           (and table (~ (closure.engine:box-width table) 73.875d0 0.001d0)))
    (check "the cells sit side by side inside that table"
           (and a b table
                (> (closure.engine:box-x b) (closure.engine:box-x a))
                (<= (+ (closure.engine:box-x b) (closure.engine:box-width b))
                    (+ (closure.engine:box-x table) (closure.engine:box-width table) 1d0))))
    (check "the caption is only as wide as the table"
           (and cap table (~ (closure.engine:box-width cap)
                             (closure.engine:box-width table) 4d0)))
    (check "a long caption word widens the table"
           (and word tiny long
                (> (closure.engine:box-width word) 80d0)
                (~ (closure.engine:box-width tiny) (closure.engine:box-width word) 4d0)
                (~ (closure.engine:box-width long) (closure.engine:box-width word) 4d0)))
    (check "a wrapped caption does not give a trailing space its own line"
           (and bc (> (closure.engine:box-height bc) 30d0)
                (< (closure.engine:box-height bc) 50d0)))
    (check "a specified table width still fills that width"
           (and wide (~ (closure.engine:box-width wide) 180d0)))))

(defun test-caption ()
  (let* ((html "<table id='t'>
                  <caption id='cap'>Title</caption>
                  <tr><td id='cell'>Cell</td></tr>
                </table>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (table (closure.engine:find-box root "t"))
         (cap (closure.engine:find-box root "cap"))
         (cell (closure.engine:find-box root "cell")))
    (format t "~&  caption: cap ~A,~A cell ~A table ~A~%"
            (and cap (closure.engine:box-y cap))
            (and cap (closure.engine:box-height cap))
            (and cell (closure.engine:box-y cell))
            (and table (closure.engine:box-y table)))
    (check "caption and table" (and table cap cell))
    (check "the caption sits at the top of the table"
           (and table cap (~ (closure.engine:box-y cap) (closure.engine:box-y table))))
    (check "the table starts below the caption"
           (and cap cell
                (>= (closure.engine:box-y cell)
                    (- (+ (closure.engine:box-y cap) (closure.engine:box-height cap)) 0.5d0))))))

(defun test-caption-bottom ()
  (let* ((html "<table id='t'>
                  <caption id='cap' style='caption-side:bottom'>Title</caption>
                  <tr><td id='cell'>Cell</td></tr>
                </table>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (table (closure.engine:find-box root "t"))
         (cap (closure.engine:find-box root "cap"))
         (cell (closure.engine:find-box root "cell")))
    (format t "~&  caption bottom: cell ~A cap ~A table ~A~%"
            (and cell (closure.engine:box-y cell))
            (and cap (closure.engine:box-y cap))
            (and table (closure.engine:box-y table)))
    (check "bottom caption and table" (and table cap cell))
    (check "the first row starts after the default border spacing"
           (and table cell (~ (- (closure.engine:box-y cell) (closure.engine:box-y table)) 2d0)))
    (check "the caption sits below the row"
           (and cap cell
                (>= (closure.engine:box-y cap)
                    (- (+ (closure.engine:box-y cell) (closure.engine:box-height cell)) 0.5d0))))))

(defun test-empty-cells ()
  (let* ((html "<table style='border-spacing:8px;empty-cells:hide'>
                  <tr>
                    <td id='empty' style='background:#cc0000;border:4px solid #000;width:20px;height:16px'></td>
                    <td id='full' style='background:#cc0000;border:4px solid #000;width:20px;height:16px'>X</td>
                  </tr>
                </table>")
         (out "/tmp/closure-empty-cells.png")
         (root (closure.engine:render-html html :width 160 :height 60 :output out
                                          :stylesheet "body{margin:0}"))
         (empty (closure.engine:find-box root "empty"))
         (full (closure.engine:find-box root "full")))
    (format t "~&  empty-cells: empty ~A,~A ~Ax~A full ~A~%"
            (and empty (closure.engine:box-x empty))
            (and empty (closure.engine:box-y empty))
            (and empty (closure.engine:box-width empty))
            (and empty (closure.engine:box-height empty))
            (and full (closure.engine:box-x full)))
    (check "empty and filled cells" (and empty full))
    (check "the empty cell keeps its place"
           (and empty full (> (closure.engine:box-x full) (closure.engine:box-x empty))
                (> (closure.engine:box-width empty) 20d0)))
    (multiple-value-bind (r g b)
        (png-rgb out
                 (round (+ (closure.engine:box-x empty) 6))
                 (round (+ (closure.engine:box-y empty) 8)))
      (format t "~&  empty cell pixel: ~D ~D ~D~%" r g b)
      (check "the empty cell paints no border or background"
             (and (> r 240) (> g 240) (> b 240))))
    (multiple-value-bind (r g b)
        (png-rgb out
                 (round (+ (closure.engine:box-x full) 4))
                 (round (+ (closure.engine:box-y full) 8)))
      (format t "~&  filled cell pixel: ~D ~D ~D~%" r g b)
      (check "the filled cell still paints its background"
             (and (> r 180) (< g 40) (< b 40))))))

(defun test-list-style-inside ()
  (labels ((word (root text)
             (let ((found nil))
               (labels ((walk (b)
                          (when b
                            (when (and (eq (closure.engine::box-kind b) :text)
                                       (string= (or (closure.engine::box-text b) "") text))
                              (setf found b))
                            (dolist (c (closure.engine::box-children b)) (walk c)))))
                 (walk root))
               found)))
    (let* ((html "<ul>
                    <li id='out'>Outside</li>
                    <li id='in' style='list-style-position:inside'>Inside</li>
                  </ul>")
           (outpng "/tmp/closure-list-inside.png")
           (root (closure.engine:render-html html :width 240 :height 80 :output outpng
                                            :stylesheet "body{margin:0}"))
           (outside (closure.engine:find-box root "out"))
           (inside (closure.engine:find-box root "in"))
           (out-word (word root "Outside"))
           (in-word (word root "Inside")))
      (format t "~&  list inside: out-text ~A in-text ~A item ~A~%"
              (and out-word (closure.engine:box-x out-word))
              (and in-word (closure.engine:box-x in-word))
              (and inside (closure.engine:box-x inside)))
      (check "inside and outside items" (and outside inside out-word in-word))
      (check "the inside marker pushes the text in"
             (and out-word in-word
                  (> (closure.engine:box-x in-word)
                     (+ (closure.engine:box-x out-word) 8d0))))
      (multiple-value-bind (r g b)
          (png-rgb outpng
                   (round (- (closure.engine:box-x outside) 14))
                   (round (+ (closure.engine:box-y outside) 10)))
        (format t "~&  outside marker pixel: ~D ~D ~D~%" r g b)
        (check "the outside marker hangs left of the item"
               (and (< r 200) (< g 200) (< b 200))))
      (multiple-value-bind (r g b)
          (png-rgb outpng
                   (round (- (closure.engine:box-x inside) 8))
                   (round (+ (closure.engine:box-y inside) 8)))
        (format t "~&  left of inside item: ~D ~D ~D~%" r g b)
        (check "the inside marker does not hang outside"
               (and (> r 240) (> g 240) (> b 240)))))))

(defun test-list-style-square ()
  (let* ((html "<ul>
                  <li id='disc'>Disc</li>
                  <li id='sq' style='list-style-type:square'>Square</li>
                </ul>")
         (out "/tmp/closure-list-square.png")
         (root (closure.engine:render-html html :width 200 :height 80 :output out
                                          :stylesheet "body{margin:0}"))
         (disc (closure.engine:find-box root "disc"))
         (square (closure.engine:find-box root "sq")))
    (labels ((corner (item)
               (png-rgb out
                        (round (- (closure.engine:box-x item) 16))
                        (round (+ (closure.engine:box-y item) 7)))))
      (check "disc and square items" (and disc square))
      (multiple-value-bind (r g b) (corner disc)
        (format t "~&  disc corner: ~D ~D ~D~%" r g b)
        (check "a disc leaves its corner empty" (and (> r 200) (> g 200) (> b 200))))
      (multiple-value-bind (r g b) (corner square)
        (format t "~&  square corner: ~D ~D ~D~%" r g b)
        (check "a square fills its corner" (and (< r 160) (< g 160) (< b 160)))))))

(defun test-list-style-circle ()
  (let* ((html "<ul>
                  <li id='disc'>Disc</li>
                  <li id='ring' style='list-style-type:circle'>Circle</li>
                </ul>")
         (out "/tmp/closure-list-circle.png")
         (root (closure.engine:render-html html :width 200 :height 80 :output out
                                          :stylesheet "body{margin:0}"))
         (disc (closure.engine:find-box root "disc"))
         (ring (closure.engine:find-box root "ring")))
    (labels ((at (item dx dy)
               (png-rgb out
                        (round (+ (closure.engine:box-x item) dx))
                        (round (+ (closure.engine:box-y item) dy)))))
      (check "disc and circle items" (and disc ring))
      (multiple-value-bind (r g b) (at disc -14 10)
        (format t "~&  disc center: ~D ~D ~D~%" r g b)
        (check "a disc is filled" (and (< r 40) (< g 40) (< b 40))))
      (multiple-value-bind (r g b) (at ring -14 10)
        (format t "~&  circle center: ~D ~D ~D~%" r g b)
        (check "a circle leaves its center empty" (and (> r 240) (> g 240) (> b 240))))
      (multiple-value-bind (r g b) (at ring -14 7)
        (format t "~&  circle edge: ~D ~D ~D~%" r g b)
        (check "a circle draws a ring" (and (< r 220) (< g 220) (< b 220)))))))

(defun test-list-style-image ()
  (let ((png "/tmp/closure-list-marker.png"))
    (write-solid-png png 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<ul>
                  <li id='disc'>Disc</li>
                  <li id='pic' style='list-style-image:url(\"~A\")'>Picture</li>
                </ul>" png))
           (out "/tmp/closure-list-image.png")
           (root (closure.engine:render-html html :width 200 :height 80 :output out
                                            :stylesheet "body{margin:0}"))
           (disc (closure.engine:find-box root "disc"))
           (pic (closure.engine:find-box root "pic")))
      (labels ((at (item dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x item) dx))
                          (round (+ (closure.engine:box-y item) dy)))))
        (check "image marker boxes" (and disc pic))
        (multiple-value-bind (r g b) (at pic -14 10)
          (format t "~&  image center: ~D ~D ~D~%" r g b)
          (check "the image replaces the disc" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at pic -16 10)
          (format t "~&  image beside the disc: ~D ~D ~D~%" r g b)
          (check "the image is wider than a disc" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at disc -16 10)
          (format t "~&  disc beside its center: ~D ~D ~D~%" r g b)
          (check "a disc does not use the image" (not (and (< r 80) (< g 80) (> b 180)))))))))

(defun test-list-style-image-none ()
  (let ((png "/tmp/closure-list-marker.png"))
    (write-solid-png png 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<ul style='list-style-image:url(\"~A\")'>
                  <li id='pic'>Picture</li>
                  <li id='off' style='list-style-image:none'>Off</li>
                </ul>" png))
           (out "/tmp/closure-list-image-none.png")
           (root (closure.engine:render-html html :width 200 :height 80 :output out
                                            :stylesheet "body{margin:0}"))
           (pic (closure.engine:find-box root "pic"))
           (off (closure.engine:find-box root "off")))
      (labels ((at (item dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x item) dx))
                          (round (+ (closure.engine:box-y item) dy)))))
        (check "none marker boxes" (and pic off))
        (multiple-value-bind (r g b) (at pic -14 10)
          (format t "~&  inherited image: ~D ~D ~D~%" r g b)
          (check "the inherited image still paints" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at off -14 10)
          (format t "~&  none center: ~D ~D ~D~%" r g b)
          (check "none keeps a disc" (and (< r 40) (< g 40) (< b 40))))))))

(defun test-background-clip-content ()
  (let* ((html "<div id='pad' style='width:20px;height:20px;padding:8px;background:#cc0000'></div>
                <div id='clip' style='width:20px;height:20px;padding:8px;background:#cc0000;background-clip:content-box'></div>")
         (out "/tmp/closure-engine-bg-clip.png")
         (root (closure.engine:render-html html :width 80 :height 100 :output out
                                          :stylesheet "body{margin:0}"))
         (pad (closure.engine:find-box root "pad"))
         (clip (closure.engine:find-box root "clip")))
    (labels ((at (box dx dy)
               (png-rgb out
                        (round (+ (closure.engine:box-x box) dx))
                        (round (+ (closure.engine:box-y box) dy)))))
      (check "clip boxes" (and pad clip))
      (multiple-value-bind (r g b) (at pad 1 1)
        (format t "~&  padding edge: ~D ~D ~D~%" r g b)
        (check "the default background fills the padding" (and (> r 180) (< g 40) (< b 40))))
      (multiple-value-bind (r g b) (at clip 1 1)
        (format t "~&  clipped padding: ~D ~D ~D~%" r g b)
        (check "content-box leaves the padding empty" (and (> r 240) (> g 240) (> b 240))))
      (multiple-value-bind (r g b) (at clip 10 10)
        (format t "~&  clipped content: ~D ~D ~D~%" r g b)
        (check "content-box still paints the content box" (and (> r 180) (< g 40) (< b 40)))))))

(defun test-background-clip-padding ()
  (let* ((html "<div id='edge' style='width:40px;height:16px;padding:8px;border:6px dashed #000;background:#cc0000'></div>
                <div id='clip' style='width:40px;height:16px;padding:8px;border:6px dashed #000;background:#cc0000;background-clip:padding-box'></div>")
         (out "/tmp/closure-engine-bg-clip-pad.png")
         (root (closure.engine:render-html html :width 120 :height 140 :output out
                                          :stylesheet "body{margin:0}"))
         (edge (closure.engine:find-box root "edge"))
         (clip (closure.engine:find-box root "clip")))
    (labels ((at (box dx dy)
               (png-rgb out
                        (round (+ (closure.engine:box-x box) dx))
                        (round (+ (closure.engine:box-y box) dy)))))
      (check "padding-box clip boxes" (and edge clip))
      (multiple-value-bind (r g b) (at edge 1 1)
        (format t "~&  default border corner: ~D ~D ~D~%" r g b)
        (check "the default background shows in the border area" (and (> r 180) (< g 40) (< b 40))))
      (multiple-value-bind (r g b) (at clip 1 1)
        (format t "~&  padding-box border corner: ~D ~D ~D~%" r g b)
        (check "padding-box leaves the border area empty" (and (> r 240) (> g 240) (> b 240))))
      (multiple-value-bind (r g b) (at clip 10 10)
        (format t "~&  padding-box padding: ~D ~D ~D~%" r g b)
        (check "padding-box still paints the padding" (and (> r 180) (< g 40) (< b 40)))))))

(defun test-background-image-clip-content ()
  (let ((png "/tmp/closure-bg-clip-image.png"))
    (write-solid-png png 40 40 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='full' style='width:20px;height:20px;padding:8px;background-image:url(\"~A\");background-repeat:no-repeat'></div>
                <div id='clip' style='width:20px;height:20px;padding:8px;background-image:url(\"~A\");background-repeat:no-repeat;background-clip:content-box'></div>"
                         png png))
           (out "/tmp/closure-engine-bg-image-clip.png")
           (root (closure.engine:render-html html :width 80 :height 100 :output out
                                            :stylesheet "body{margin:0}"))
           (full (closure.engine:find-box root "full"))
           (clip (closure.engine:find-box root "clip")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x box) dx))
                          (round (+ (closure.engine:box-y box) dy)))))
        (check "image clip boxes" (and full clip))
        (multiple-value-bind (r g b) (at full 1 1)
          (format t "~&  image padding: ~D ~D ~D~%" r g b)
          (check "the default image paints in the padding" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at clip 1 1)
          (format t "~&  clipped image padding: ~D ~D ~D~%" r g b)
          (check "content-box keeps the image out of the padding" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at clip 10 10)
          (format t "~&  clipped image content: ~D ~D ~D~%" r g b)
          (check "content-box still paints the image in the content box" (and (< r 80) (< g 80) (> b 180))))))))

(defun test-background-origin-content ()
  (let ((png "/tmp/closure-bg-origin.png"))
    (write-solid-png png 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='pad' style='width:20px;height:20px;padding:8px;background-image:url(\"~A\");background-repeat:no-repeat'></div>
                <div id='content' style='width:20px;height:20px;padding:8px;background-image:url(\"~A\");background-repeat:no-repeat;background-origin:content-box'></div>"
                         png png))
           (out "/tmp/closure-engine-bg-origin.png")
           (root (closure.engine:render-html html :width 80 :height 100 :output out
                                            :stylesheet "body{margin:0}"))
           (pad (closure.engine:find-box root "pad"))
           (content (closure.engine:find-box root "content")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x box) dx))
                          (round (+ (closure.engine:box-y box) dy)))))
        (check "origin boxes" (and pad content))
        (multiple-value-bind (r g b) (at pad 1 1)
          (format t "~&  default origin: ~D ~D ~D~%" r g b)
          (check "the default image starts in the padding" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at content 1 1)
          (format t "~&  content origin padding: ~D ~D ~D~%" r g b)
          (check "content-box origin leaves the padding empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at content 9 9)
          (format t "~&  content origin image: ~D ~D ~D~%" r g b)
          (check "content-box origin starts the image at the content edge" (and (< r 80) (< g 80) (> b 180))))))))

(defun test-background-origin-border ()
  (let ((png "/tmp/closure-bg-origin-border.png"))
    (write-solid-png png 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='pad' style='width:20px;height:20px;border:8px solid rgba(0,0,0,0);background-image:url(\"~A\");background-repeat:no-repeat'></div>
                <div id='edge' style='width:20px;height:20px;border:8px solid rgba(0,0,0,0);background-image:url(\"~A\");background-repeat:no-repeat;background-origin:border-box'></div>"
                         png png))
           (out "/tmp/closure-engine-bg-origin-border.png")
           (root (closure.engine:render-html html :width 80 :height 120 :output out
                                            :stylesheet "body{margin:0}"))
           (pad (closure.engine:find-box root "pad"))
           (edge (closure.engine:find-box root "edge")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x box) dx))
                          (round (+ (closure.engine:box-y box) dy)))))
        (check "border origin boxes" (and pad edge))
        (multiple-value-bind (r g b) (at pad 1 1)
          (format t "~&  default border corner: ~D ~D ~D~%" r g b)
          (check "the default image starts inside the border" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at edge 1 1)
          (format t "~&  border origin corner: ~D ~D ~D~%" r g b)
          (check "border-box origin starts the image at the outer edge" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at pad 9 9)
          (format t "~&  default inside the border: ~D ~D ~D~%" r g b)
          (check "the default image still paints inside the border" (and (< r 80) (< g 80) (> b 180))))))))

(defun test-background-image-clip-padding ()
  (let ((png "/tmp/closure-bg-clip-pad-image.png"))
    (write-solid-png png 20 20 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='full' style='width:20px;height:20px;border:8px solid rgba(0,0,0,0);background-image:url(\"~A\");background-repeat:no-repeat;background-origin:border-box'></div>
                <div id='clip' style='width:20px;height:20px;border:8px solid rgba(0,0,0,0);background-image:url(\"~A\");background-repeat:no-repeat;background-origin:border-box;background-clip:padding-box'></div>"
                         png png))
           (out "/tmp/closure-engine-bg-image-clip-pad.png")
           (root (closure.engine:render-html html :width 80 :height 120 :output out
                                            :stylesheet "body{margin:0}"))
           (full (closure.engine:find-box root "full"))
           (clip (closure.engine:find-box root "clip")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x box) dx))
                          (round (+ (closure.engine:box-y box) dy)))))
        (check "padding image clip boxes" (and full clip))
        (multiple-value-bind (r g b) (at full 1 1)
          (format t "~&  image under the border: ~D ~D ~D~%" r g b)
          (check "the default image paints under the border" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at clip 1 1)
          (format t "~&  clipped border: ~D ~D ~D~%" r g b)
          (check "padding-box keeps the image out of the border" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at clip 10 10)
          (format t "~&  clipped inside: ~D ~D ~D~%" r g b)
          (check "padding-box still paints the image inside the border" (and (< r 80) (< g 80) (> b 180))))))))

(defun test-background-repeat-space ()
  (let ((png "/tmp/closure-bg-space.png"))
    (write-solid-png png 6 6 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='tile' style='width:20px;height:8px;background-image:url(\"~A\")'></div>
                <div id='gap' style='width:20px;height:8px;background-image:url(\"~A\");background-repeat:space'></div>"
                         png png))
           (out "/tmp/closure-engine-bg-space.png")
           (root (closure.engine:render-html html :width 40 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (tile (closure.engine:find-box root "tile"))
           (gap (closure.engine:find-box root "gap")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x box) dx))
                          (round (+ (closure.engine:box-y box) dy)))))
        (check "space boxes" (and tile gap))
        (multiple-value-bind (r g b) (at tile 6 1)
          (format t "~&  flush tile: ~D ~D ~D~%" r g b)
          (check "the default tiles the image flush" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at gap 6 1)
          (format t "~&  space gap: ~D ~D ~D~%" r g b)
          (check "space leaves a gap between images" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at gap 8 1)
          (format t "~&  space next image: ~D ~D ~D~%" r g b)
          (check "space still paints the next image" (and (< r 80) (< g 80) (> b 180))))))))

(defun test-background-repeat-round ()
  (let ((png "/tmp/closure-bg-round.png"))
    (write-split-png png 8 8 0.8d0 0d0 0d0 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<div id='tile' style='width:12px;height:8px;background-image:url(\"~A\");background-repeat:repeat'></div>
                <div id='round' style='width:12px;height:8px;background-image:url(\"~A\");background-repeat:round'></div>"
                         png png))
           (out "/tmp/closure-engine-bg-round.png")
           (root (closure.engine:render-html html :width 40 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (tile (closure.engine:find-box root "tile"))
           (round (closure.engine:find-box root "round")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x box) dx))
                          (round (+ (closure.engine:box-y box) dy)))))
        (check "round boxes" (and tile round))
        (multiple-value-bind (r g b) (at tile 3 1)
          (format t "~&  unscaled: ~D ~D ~D~%" r g b)
          (check "an unscaled tile keeps the first color there" (and (> r 180) (< g 80) (< b 80))))
        (multiple-value-bind (r g b) (at round 3 1)
          (format t "~&  rounded: ~D ~D ~D~%" r g b)
          (check "round scales the tile so the second color arrives sooner" (and (< r 80) (> b 180))))
        (multiple-value-bind (r g b) (at round 1 1)
          (format t "~&  rounded start: ~D ~D ~D~%" r g b)
          (check "round still starts with the first color" (and (> r 180) (< g 80) (< b 80))))))))

(defun test-object-position-right ()
  (let ((png "/tmp/closure-object-pos.png"))
    (write-solid-png png 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<img id='mid' src='~A' width='20' height='8' style='display:block;object-fit:none'>
                <img id='right' src='~A' width='20' height='8' style='display:block;object-fit:none;object-position:right'>"
                         png png))
           (out "/tmp/closure-engine-object-pos.png")
           (root (closure.engine:render-html html :width 40 :height 30 :output out
                                            :stylesheet "body{margin:0}"))
           (mid (closure.engine:find-box root "mid"))
           (right (closure.engine:find-box root "right")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x box) dx))
                          (round (+ (closure.engine:box-y box) dy)))))
        (check "object position boxes" (and mid right))
        (multiple-value-bind (r g b) (at mid 9 3)
          (format t "~&  centered: ~D ~D ~D~%" r g b)
          (check "the default image sits in the middle" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at right 9 3)
          (format t "~&  right middle: ~D ~D ~D~%" r g b)
          (check "right leaves the middle empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at right 17 3)
          (format t "~&  right edge: ~D ~D ~D~%" r g b)
          (check "right places the image against the right side" (and (< r 80) (< g 80) (> b 180))))))))

(defun test-object-position-bottom ()
  (let ((png "/tmp/closure-object-pos-bottom.png"))
    (write-solid-png png 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<img id='mid' src='~A' width='8' height='20' style='display:block;object-fit:none'>
                <img id='low' src='~A' width='8' height='20' style='display:block;object-fit:none;object-position:bottom'>"
                         png png))
           (out "/tmp/closure-engine-object-pos-bottom.png")
           (root (closure.engine:render-html html :width 20 :height 50 :output out
                                            :stylesheet "body{margin:0}"))
           (mid (closure.engine:find-box root "mid"))
           (low (closure.engine:find-box root "low")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x box) dx))
                          (round (+ (closure.engine:box-y box) dy)))))
        (check "bottom position boxes" (and mid low))
        (multiple-value-bind (r g b) (at mid 3 9)
          (format t "~&  centered: ~D ~D ~D~%" r g b)
          (check "the default image sits in the middle" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at low 3 9)
          (format t "~&  bottom middle: ~D ~D ~D~%" r g b)
          (check "bottom leaves the middle empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at low 3 17)
          (format t "~&  bottom edge: ~D ~D ~D~%" r g b)
          (check "bottom places the image against the bottom" (and (< r 80) (< g 80) (> b 180))))))))

(defun test-object-position-left-bottom ()
  (let ((png "/tmp/closure-object-pos-corner.png"))
    (write-solid-png png 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<img id='mid' src='~A' width='20' height='20' style='display:block;object-fit:none'>
                <img id='corner' src='~A' width='20' height='20' style='display:block;object-fit:none;object-position:left bottom'>"
                         png png))
           (out "/tmp/closure-engine-object-pos-corner.png")
           (root (closure.engine:render-html html :width 40 :height 50 :output out
                                            :stylesheet "body{margin:0}"))
           (mid (closure.engine:find-box root "mid"))
           (corner (closure.engine:find-box root "corner")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x box) dx))
                          (round (+ (closure.engine:box-y box) dy)))))
        (check "corner position boxes" (and mid corner))
        (multiple-value-bind (r g b) (at mid 9 9)
          (format t "~&  centered: ~D ~D ~D~%" r g b)
          (check "the default image sits in the middle" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at corner 9 9)
          (format t "~&  corner middle: ~D ~D ~D~%" r g b)
          (check "left bottom leaves the middle empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at corner 1 17)
          (format t "~&  corner: ~D ~D ~D~%" r g b)
          (check "left bottom places the image in the bottom-left corner" (and (< r 80) (< g 80) (> b 180))))))))

(defun test-object-position-percent ()
  (let ((png "/tmp/closure-object-pos-percent.png"))
    (write-solid-png png 4 4 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<img id='mid' src='~A' width='20' height='20' style='display:block;object-fit:none'>
                <img id='corner' src='~A' width='20' height='20' style='display:block;object-fit:none;object-position:100% 0'>"
                         png png))
           (out "/tmp/closure-engine-object-pos-percent.png")
           (root (closure.engine:render-html html :width 40 :height 50 :output out
                                            :stylesheet "body{margin:0}"))
           (mid (closure.engine:find-box root "mid"))
           (corner (closure.engine:find-box root "corner")))
      (labels ((at (box dx dy)
                 (png-rgb out
                          (round (+ (closure.engine:box-x box) dx))
                          (round (+ (closure.engine:box-y box) dy)))))
        (check "percent position boxes" (and mid corner))
        (multiple-value-bind (r g b) (at mid 9 9)
          (format t "~&  centered: ~D ~D ~D~%" r g b)
          (check "the default image sits in the middle" (and (< r 80) (< g 80) (> b 180))))
        (multiple-value-bind (r g b) (at corner 9 9)
          (format t "~&  percent middle: ~D ~D ~D~%" r g b)
          (check "100% 0 leaves the middle empty" (and (> r 240) (> g 240) (> b 240))))
        (multiple-value-bind (r g b) (at corner 17 1)
          (format t "~&  percent corner: ~D ~D ~D~%" r g b)
          (check "100% 0 places the image in the top-right corner" (and (< r 80) (< g 80) (> b 180))))))))

(defun test-aspect-ratio ()
  (let* ((html "<div id='plain' style='width:40px'></div>
                <div id='box' style='width:40px;aspect-ratio:2 / 1'></div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (plain (closure.engine:find-box root "plain"))
         (box (closure.engine:find-box root "box")))
    (check "aspect-ratio boxes" (and plain box))
    (format t "~&  plain ~S ratio ~Sx~S~%"
            (and plain (closure.engine:box-height plain))
            (and box (closure.engine:box-width box))
            (and box (closure.engine:box-height box)))
    (check "an empty box stays flat" (and plain (~ (closure.engine:box-height plain) 0d0)))
    (check "the ratio box keeps its width" (and box (~ (closure.engine:box-width box) 40d0)))
    (check "2 / 1 makes the box half as tall as it is wide"
           (and box (~ (closure.engine:box-height box) 20d0)))))

(defun test-aspect-ratio-width ()
  (let* ((html "<div id='stretch' style='height:20px'></div>
                <div id='box' style='height:20px;width:auto;aspect-ratio:2 / 1'></div>")
         (root (closure.engine:layout-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"))
         (stretch (closure.engine:find-box root "stretch"))
         (box (closure.engine:find-box root "box")))
    (check "aspect width boxes" (and stretch box))
    (format t "~&  stretch ~S ratio ~Sx~S~%"
            (and stretch (closure.engine:box-width stretch))
            (and box (closure.engine:box-width box))
            (and box (closure.engine:box-height box)))
    (check "a height alone still stretches" (and stretch (~ (closure.engine:box-width stretch) 200d0)))
    (check "the ratio keeps the height" (and box (~ (closure.engine:box-height box) 20d0)))
    (check "2 / 1 makes the auto width twice the height"
           (and box (~ (closure.engine:box-width box) 40d0)))))

(defun test-aspect-ratio-content ()
  (let* ((html "<div id='flat' style='width:40px;aspect-ratio:2 / 1'></div>
                <div id='box' style='width:40px;aspect-ratio:2 / 1'><div style='height:40px;margin:0'></div></div>")
         (root (closure.engine:layout-html html :width 200 :height 120
                                          :stylesheet "body{margin:0}"))
         (flat (closure.engine:find-box root "flat"))
         (box (closure.engine:find-box root "box")))
    (check "aspect content boxes" (and flat box))
    (format t "~&  flat ~S grown ~Sx~S~%"
            (and flat (closure.engine:box-height flat))
            (and box (closure.engine:box-width box))
            (and box (closure.engine:box-height box)))
    (check "an empty ratio box stays at the ratio" (and flat (~ (closure.engine:box-height flat) 20d0)))
    (check "the grown box keeps its width" (and box (~ (closure.engine:box-width box) 40d0)))
    (check "taller content grows the box past the ratio"
           (and box (~ (closure.engine:box-height box) 40d0)))))

(defun test-aspect-ratio-image ()
  (let* ((png "/tmp/closure-aspect-square.png"))
    (write-solid-png png 20 20 0.1d0 0.2d0 0.9d0)
    (let* ((html (format nil "<img id='plain' src='~A' style='width:40px'>
                          <img id='box' src='~A' style='width:40px;aspect-ratio:2 / 1'>"
                         png png))
           (root (closure.engine:layout-html html :width 200 :height 80
                                            :stylesheet "body{margin:0}"))
           (plain (closure.engine:find-box root "plain"))
           (box (closure.engine:find-box root "box")))
      (check "aspect image boxes" (and plain box))
      (format t "~&  square ~Sx~S ratio ~Sx~S~%"
              (and plain (closure.engine:box-width plain))
              (and plain (closure.engine:box-height plain))
              (and box (closure.engine:box-width box))
              (and box (closure.engine:box-height box)))
      (check "a square file stays square" (and plain (~ (closure.engine:box-height plain) 40d0)))
      (check "the ratio image keeps its width" (and box (~ (closure.engine:box-width box) 40d0)))
      (check "2 / 1 makes the square image 20px tall"
             (and box (~ (closure.engine:box-height box) 20d0))))))

(defun test-aspect-ratio-image-height ()
  (let* ((png "/tmp/closure-aspect-square.png"))
    (write-solid-png png 20 20 0.8d0 0.1d0 0.1d0)
    (let* ((html (format nil "<img id='plain' src='~A' style='height:20px'>
                          <img id='box' src='~A' style='height:20px;aspect-ratio:2 / 1'>"
                         png png))
           (root (closure.engine:layout-html html :width 200 :height 80
                                            :stylesheet "body{margin:0}"))
           (plain (closure.engine:find-box root "plain"))
           (box (closure.engine:find-box root "box")))
      (format t "~&  square-h ~Sx~S ratio ~Sx~S~%"
              (and plain (closure.engine:box-width plain))
              (and plain (closure.engine:box-height plain))
              (and box (closure.engine:box-width box))
              (and box (closure.engine:box-height box)))
      (check "height aspect boxes" (and plain box))
      (check "a square file with only a height stays square"
             (and plain (~ (closure.engine:box-width plain) 20d0)
                  (~ (closure.engine:box-height plain) 20d0)))
      (check "the ratio image keeps its height"
             (and box (~ (closure.engine:box-height box) 20d0)))
      (check "2 / 1 makes the square image 40px wide"
             (and box (~ (closure.engine:box-width box) 40d0))))))

(defun test-code-font ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'>Use <code id='c'>code</code> now</p>"
                :width 300 :height 40
                :stylesheet "body{margin:0} p{margin:0}"))
         (code (closure.engine:find-box root "c")))
    (format t "~&  code ~A,~A ~Ax~A~%"
            (and code (closure.engine:box-x code))
            (and code (closure.engine:box-y code))
            (and code (closure.engine:box-width code))
            (and code (closure.engine:box-height code)))
    (check "code box" code)
    (check "code is 13px monospace"
           (and code
                (~ (closure.engine::computed-font-size (closure.engine::box-style code)) 13d0)
                (member "monospace"
                        (closure.engine::computed-font-family (closure.engine::box-style code))
                        :test #'string=)))
    (check "code is as wide as the word at 13px"
           (and code
                (~ (closure.engine:box-width code)
                   (closure.engine::text-advance "code" 13d0 "monospace" :normal :normal))))))

(defun test-tt ()
  (let* ((root (closure.engine:layout-html
                "<p><tt id='w'>WWWW</tt><tt id='i'>iiii</tt><span id='s'>WWWW</span><span id='n'>iiii</span></p>"
                :width 500 :height 40
                :stylesheet "body{margin:0} p{margin:0;font-size:16px}"))
         (wide (closure.engine:find-box root "w"))
         (narrow (closure.engine:find-box root "i"))
         (prop-w (closure.engine:find-box root "s"))
         (prop-n (closure.engine:find-box root "n")))
    (check "tt boxes" (and wide narrow prop-w prop-n))
    (check "tt is monospace"
           (and wide
                (member "monospace"
                        (closure.engine::computed-font-family (closure.engine::box-style wide))
                        :test #'string=)))
    (check "tt gives wide and narrow letters the same advance"
           (and wide narrow
                (~ (closure.engine:box-width wide) (closure.engine:box-width narrow) 3d0)))
    (check "proportional letters keep different advances"
           (and prop-w prop-n
                (> (closure.engine:box-width prop-w)
                   (+ (closure.engine:box-width prop-n) 4d0))))))

(defun test-nested-q ()
  (let* ((root (closure.engine:layout-html
                "<p><q>outer <q>inner</q> end</q></p>"
                :width 400 :height 40
                :stylesheet "body{margin:0} p{margin:0}"))
         (texts nil))
    (labels ((walk (b)
               (when b
                 (when (and (eq (closure.engine::box-kind b) :text)
                            (closure.engine::box-text b)
                            (not (string= (closure.engine::box-text b) " ")))
                   (push (closure.engine::box-text b) texts))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (setf texts (nreverse texts))
    (format t "~&  nested q texts: ~S~%" texts)
    (check "nested q uses single quotes inside double quotes"
           (equal texts (list (string (code-char #x201C))
                              "outer"
                              (string (code-char #x2018))
                              "inner"
                              (string (code-char #x2019))
                              "end"
                              (string (code-char #x201D)))))))

(defun test-blockquote ()
  (let* ((root (closure.engine:layout-html
                "<p id='p'>Plain</p><blockquote id='q'>Quoted</blockquote>"
                :width 300 :height 80
                :stylesheet "body{margin:0} p{margin:0} blockquote{margin-top:0;margin-bottom:0}"))
         (plain (closure.engine:find-box root "p"))
         (quote (closure.engine:find-box root "q")))
    (check "blockquote box" (and plain quote))
    (check "blockquote is indented"
           (and plain quote (> (closure.engine:box-x quote)
                               (+ (closure.engine:box-x plain) 20d0))))
    (check "blockquote indent is 40px"
           (and quote (~ (closure.engine:box-x quote) 40d0)))))

(defun test-q ()
  (let* ((root (closure.engine:layout-html
                "<p>Say <q>hello</q> now</p>"
                :width 300 :height 40
                :stylesheet "body{margin:0} p{margin:0}"))
         (texts nil))
    (labels ((walk (b)
               (when b
                 (when (and (eq (closure.engine::box-kind b) :text)
                            (closure.engine::box-text b)
                            (not (string= (closure.engine::box-text b) " ")))
                   (push (closure.engine::box-text b) texts))
                 (dolist (c (closure.engine::box-children b)) (walk c)))))
      (walk root))
    (setf texts (nreverse texts))
    (format t "~&  q texts: ~S~%" texts)
    (check "q keeps the surrounding words"
           (and (member "Say" texts :test #'string=)
                (member "now" texts :test #'string=)))
    (check "q wraps the word in quotes"
           (loop for rest on texts
                 thereis (and (cdr rest) (cddr rest)
                              (string= (first rest) (string (code-char #x201C)))
                              (string= (second rest) "hello")
                              (string= (third rest) (string (code-char #x201D))))))
    (let ((open nil)
          (mark (string (code-char #x201C))))
      (labels ((find-open (b)
                 (when b
                   (when (and (eq (closure.engine::box-kind b) :text)
                              (string= (or (closure.engine::box-text b) "") mark))
                     (setf open b))
                   (dolist (c (closure.engine::box-children b)) (find-open c)))))
        (find-open root))
      (check "open quote is the wide curly mark"
             (and open (~ (closure.engine:box-width open) 7.1d0 0.15d0))))))

(defun test-figure ()
  (let* ((root (closure.engine:layout-html
                "<figure id='f'><figcaption id='c'>Figure</figcaption></figure>"
                :width 400 :height 80
                :stylesheet "body{margin:0}"))
         (fig (closure.engine:find-box root "f"))
         (cap (closure.engine:find-box root "c")))
    (check "figure box" (and fig cap))
    (check "figure is indented 40px"
           (and fig (~ (closure.engine:box-x fig) 40d0)))
    (check "figure leaves 40px on the right"
           (and fig (~ (closure.engine:box-width fig) 320d0)))
    (check "figcaption fills the figure"
           (and fig cap (~ (closure.engine:box-x cap) (closure.engine:box-x fig))
                (~ (closure.engine:box-width cap) (closure.engine:box-width fig))))))

(defun test-dl ()
  (let* ((root (closure.engine:layout-html
                "<dl><dt id='t'>Term</dt><dd id='d'>Definition</dd></dl>"
                :width 400 :height 120
                :stylesheet "body{margin:0}"))
         (term (closure.engine:find-box root "t"))
         (def (closure.engine:find-box root "d")))
    (check "definition list boxes" (and term def))
    (check "definition is indented 40px"
           (and term def (~ (closure.engine:box-x def)
                            (+ (closure.engine:box-x term) 40d0))))
    (check "definition sits on the next line"
           (and term def (> (closure.engine:box-y def) (closure.engine:box-y term))))))

(defun test-hr ()
  (let* ((html "<hr id='r'>")
         (out "/tmp/closure-engine-hr.png")
         (root (closure.engine:render-html html :width 200 :height 40 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "r")))
    (check "hr box" box)
    (check "hr spans the content width"
           (and box (~ (closure.engine:box-width box) 200d0)))
    (check "hr is the two borders of an inset rule"
           (and box (~ (closure.engine:box-height box) 2d0)))
    (labels ((rgb (x y)
               (and (<= 0 y 39) (multiple-value-list (png-rgb out x y))))
             (ink (pixel)
               (and pixel (< (first pixel) 250))))
      (let* ((y (floor (closure.engine:box-y box)))
             (top (rgb 100 y))
             (bot (rgb 100 (1+ y))))
        (format t "~&  hr y ~D top ~A bottom ~A~%" y top bot)
        (check "hr paints the left of the rule" (ink (rgb 4 y)))
        (check "hr paints the middle of the rule" (ink top))
        (check "hr paints the right of the rule" (ink (rgb 196 y)))
        (check "the inset top edge is darker than the bottom"
               (and top bot (< (first top) (first bot))))
        (check "the default rule matches Chromium gray"
               (and top bot
                    (< (abs (- (first top) 154)) 20)
                    (< (abs (- (first bot) 238)) 20)))
        (check "the rule does not paint past its borders"
               (and (not (ink (rgb 100 (1- y))))
                    (not (ink (rgb 100 (+ y 2))))))))))

(defun test-input-size ()
  (let* ((html "<input id='d' value='Hi'>
                <input id='a' size='10' value='Hi'>
                <input id='b' size='40' value='Hi'>
                <input id='c' size='10' style='width:80px' value='Hi'>
                <input id='s' type='submit' size='10' value='Go'>")
         (root (closure.engine:layout-html html :width 800 :stylesheet "body{margin:0}"))
         (d (closure.engine:find-box root "d"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c"))
         (s (closure.engine:find-box root "s")))
    (check "sized inputs" (and d a b c s))
    (check "the default field stays 177 wide"
           (and d (~ (closure.engine:box-width d) 177d0)))
    (check "size sets the field width in characters"
           (and a b
                (~ (closure.engine:box-width a) 97d0 6d0)
                (> (closure.engine:box-width b) (+ (closure.engine:box-width a) 150d0))
                (~ (closure.engine:box-height a) 21d0)))
    (check "an author width wins over size"
           (and c (~ (closure.engine:box-width c) 80d0)))
    (check "size does not widen a submit button"
           (and s (< (closure.engine:box-width s) 80d0)))))

(defun test-textarea-size ()
  (let* ((html "<textarea id='d'></textarea>
                <textarea id='a' cols='40' rows='4'></textarea>
                <textarea id='b' cols='10' rows='2'></textarea>
                <textarea id='c' cols='40' rows='4' style='width:80px;height:30px'></textarea>")
         (root (closure.engine:layout-html html :width 600 :stylesheet "body{margin:0}"))
         (d (closure.engine:find-box root "d"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c")))
    (check "sized textareas" (and d a b c))
    (check "the default textarea is 181 by 40"
           (and d (~ (closure.engine:box-width d) 181d0)
                (~ (closure.engine:box-height d) 40d0)))
    (check "cols and rows grow the textarea"
           (and a b
                (> (closure.engine:box-width a) (+ (closure.engine:box-width b) 100d0))
                (> (closure.engine:box-height a) (+ (closure.engine:box-height b) 20d0))
                (~ (closure.engine:box-width b) 101d0 4d0)
                (~ (closure.engine:box-height a) 74d0 4d0)))
    (check "an author size wins over cols and rows"
           (and c (~ (closure.engine:box-width c) 86d0)
                (~ (closure.engine:box-height c) 36d0)))))

(defun test-placeholder ()
  (let* ((html "<input id='a' placeholder='Name'><input id='b' placeholder='Name' value='Hi'><textarea id='c' placeholder='Note'></textarea>")
         (path "/tmp/closure-engine-placeholder.png")
         (root (closure.engine:render-html html :width 500 :height 80
                                          :stylesheet "body{margin:0}"
                                          :output path)))
    (labels ((gray-ink (box)
               (when box
                 (loop for y from (round (+ (closure.engine:box-y box) 2d0))
                         below (round (+ (closure.engine:box-y box)
                                         (closure.engine:box-height box) -2d0))
                       thereis
                       (loop for x from (round (+ (closure.engine:box-x box) 2d0))
                               below (round (+ (closure.engine:box-x box) 50d0))
                             thereis
                             (multiple-value-bind (r g b)
                                 (png-rgb path x y)
                               (and (< (abs (- r 117)) 25)
                                    (< (abs (- g 117)) 25)
                                    (< (abs (- b 117)) 25)))))))
             (dark-ink (box)
               (when box
                 (loop for y from (round (+ (closure.engine:box-y box) 2d0))
                         below (round (+ (closure.engine:box-y box)
                                         (closure.engine:box-height box) -2d0))
                       thereis
                       (loop for x from (round (+ (closure.engine:box-x box) 2d0))
                               below (round (+ (closure.engine:box-x box) 40d0))
                             thereis
                             (multiple-value-bind (r g b)
                                 (png-rgb path x y)
                               (< (+ r g b) 80)))))))
      (let ((a (closure.engine:find-box root "a"))
            (b (closure.engine:find-box root "b"))
            (c (closure.engine:find-box root "c")))
        (check "placeholder fields" (and a b c))
        (check "an empty field paints its placeholder gray"
               (gray-ink a))
        (check "a value stays darker than the placeholder"
               (dark-ink b))
        (check "an empty textarea paints its placeholder gray"
               (gray-ink c))))))

(defun test-disabled ()
  (let* ((html "<button id='a' disabled>OK</button><button id='b'>OK</button><input id='c' disabled value='Hi'><input id='d' disabled value='Hi' style='color:#c00'>")
         (path "/tmp/closure-engine-disabled.png")
         (root (closure.engine:render-html html :width 400 :height 40
                                          :stylesheet "body{margin:0}"
                                          :output path))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c"))
         (d (closure.engine:find-box root "d"))
         (as (and a (closure.engine::box-style a)))
         (cs (and c (closure.engine::box-style c)))
         (ds (and d (closure.engine::box-style d))))
    (check "disabled controls" (and a b c d as cs ds))
    (check "a disabled button fades its text and face"
           (and as
                (< (fourth (closure.engine::computed-color as)) 0.5d0)
                (< (fourth (closure.engine::computed-background as)) 0.5d0)))
    (check "a disabled field uses gray text and a faded background"
           (and cs
                (~ (* (first (closure.engine::computed-color cs)) 255d0) 84d0 2d0)
                (< (fourth (closure.engine::computed-background cs)) 0.5d0)))
    (check "an author color wins on a disabled field"
           (and ds (> (first (closure.engine::computed-color ds)) 0.7d0)))
    (let ((x (round (+ (closure.engine:box-x c) 60d0)))
          (y (round (+ (closure.engine:box-y c) 10d0))))
      (multiple-value-bind (r g b) (png-rgb path x y)
        (check "the disabled field is the faded gray"
               (and (< r 254) (> r 240)
                    (< g 254) (> g 240)
                    (< b 254) (> b 240)))))))

(defun test-dir-attr ()
  (let* ((html "<div id='box' dir='rtl' style='width:200px'><span id='a'>One</span> <span id='b'>Two</span></div>
                <div dir='ltr' style='width:200px'><span id='c'>One</span></div>
                <div dir='rtl' style='direction:ltr;width:200px'><span id='e'>One</span></div>")
         (root (closure.engine:layout-html html :width 300 :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "box"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c"))
         (e (closure.engine:find-box root "e")))
    (check "direction boxes" (and box a b c e))
    (check "dir=rtl places the line on the right, still in source order"
           (and a b
                (eq (closure.engine::computed-direction (closure.engine::box-style box)) :rtl)
                (> (closure.engine:box-x a) 100d0)
                (> (closure.engine:box-x b) (closure.engine:box-x a))
                (> (+ (closure.engine:box-x b) (closure.engine:box-width b)) 180d0)))
    (check "dir=ltr stays on the left"
           (and c (< (closure.engine:box-x c) 5d0)))
    (check "an author direction wins over dir"
           (and e (< (closure.engine:box-x e) 5d0)
                (eq (closure.engine::computed-direction (closure.engine::box-style e)) :ltr)))))

(defun test-hidden-attr ()
  (let* ((html "<p id='a' hidden>secret</p><p id='b'>shown</p><span id='d' hidden>xxxx</span><span id='e'>y</span><p id='c' hidden style='display:block'>forced</p><p id='u' hidden='until-found'>later</p>")
         (root (closure.engine:layout-html html :width 200
                                          :stylesheet "body{margin:0} p{margin:0}"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (d (closure.engine:find-box root "d"))
         (e (closure.engine:find-box root "e"))
         (c (closure.engine:find-box root "c"))
         (u (closure.engine:find-box root "u")))
    (check "hidden leaves no box" (and (null a) (null d) b e c u))
    (check "the next block starts where the hidden one would have"
           (and b (< (closure.engine:box-y b) 4d0)))
    (check "a hidden span does not push the next word"
           (and e (< (closure.engine:box-x e) 4d0)))
    (check "an author display wins over hidden"
           (and c (> (closure.engine:box-width c) 20d0)
                (eq (closure.engine::computed-display (closure.engine::box-style c)) :block)))
    (check "hidden=until-found stays in the layout"
           (and u (> (closure.engine:box-height u) 0d0)))))

(defun test-reversed-list ()
  (let* ((html "<ol reversed><li id='a'>A</li><li id='b'>B</li><li id='c'>C</li></ol>
                <ol reversed start='5'><li id='d'>A</li><li id='e'>B</li></ol>
                <ol reversed type='a'><li id='f'>A</li><li id='g'>B</li></ol>
                <ol><li id='h'>A</li><li id='i'>B</li></ol>")
         (root (closure.engine:layout-html html :width 200 :stylesheet "body{margin:0}"))
         (marker (lambda (id)
                   (let ((box (closure.engine:find-box root id)))
                     (and box (closure.engine::box-marker box))))))
    (check "reversed markers"
           (and (funcall marker "a") (funcall marker "h")))
    (check "reversed counts down from the length"
           (and (string= (funcall marker "a") "3.")
                (string= (funcall marker "b") "2.")
                (string= (funcall marker "c") "1.")))
    (check "reversed start counts down from that number"
           (and (string= (funcall marker "d") "5.")
                (string= (funcall marker "e") "4.")))
    (check "reversed letters count down too"
           (and (string= (funcall marker "f") "b.")
                (string= (funcall marker "g") "a.")))
    (check "a forward list still starts at one"
           (and (string= (funcall marker "h") "1.")
                (string= (funcall marker "i") "2.")))))

(defun test-hr-color ()
  (let* ((html "<hr id='a' color='#c00'><hr id='b' color='blue' size='6'><hr id='c' color='#c00' style='border-top-color:#0a0;border-right-color:#0a0;border-bottom-color:#0a0;border-left-color:#0a0'>")
         (path "/tmp/closure-engine-hr-color.png")
         (root (closure.engine:render-html html :width 200 :height 80
                                          :stylesheet "body{margin:0}"
                                          :output path))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c"))
         (as (and a (closure.engine::box-style a)))
         (bs (and b (closure.engine::box-style b)))
         (cs (and c (closure.engine::box-style c))))
    (check "colored rules" (and a b c as bs cs))
    (check "hr color makes a solid rule in that color"
           (and as
                (eq (closure.engine::computed-bts as) :solid)
                (> (first (closure.engine::computed-btc as)) 0.7d0)
                (< (second (closure.engine::computed-btc as)) 0.1d0)
                (> (first (closure.engine::computed-background as)) 0.7d0)))
    (check "a sized colored rule keeps the size and the color"
           (and b bs
                (~ (closure.engine:box-height b) 6d0)
                (> (third (closure.engine::computed-background bs)) 0.7d0)))
    (check "an author border color wins and the rule background stays"
           (and cs
                (> (second (closure.engine::computed-btc cs)) 0.5d0)
                (> (first (closure.engine::computed-background cs)) 0.7d0)))
    (let ((x (round (+ (closure.engine:box-x a) 4d0)))
          (y (round (closure.engine:box-y a))))
      (multiple-value-bind (r g b) (png-rgb path x y)
        (check "the colored rule is painted red"
               (and (> r 180) (< g 40) (< b 40)))))))

(defun test-image-border ()
  (let* ((png "file:///home/jmercouris/Source/closure/screenshots/square.png")
         (html (format nil "<img id='a' border='2' width='20' height='20' src='~A'><img id='b' border='0' width='20' height='20' src='~A'><img id='c' border='4' style='border:1px solid #0a0' width='20' height='20' src='~A'>"
                       png png png))
         (path "/tmp/closure-engine-image-border.png")
         (root (closure.engine:render-html html :width 200 :height 40
                                          :stylesheet "body{margin:0}"
                                          :output path))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (c (closure.engine:find-box root "c"))
         (as (and a (closure.engine::box-style a)))
         (bs (and b (closure.engine::box-style b)))
         (cs (and c (closure.engine::box-style c))))
    (check "bordered images" (and a b c as bs cs))
    (check "an image border is a solid frame around the content"
           (and a as
                (~ (closure.engine:box-width a) 24d0)
                (~ (closure.engine:box-height a) 24d0)
                (~ (closure.engine::computed-bt as) 2d0 0.01d0)
                (eq (closure.engine::computed-bts as) :solid)))
    (check "border=0 leaves the image its content size"
           (and b bs
                (~ (closure.engine:box-width b) 20d0)
                (~ (closure.engine::computed-bt bs) 0d0 0.01d0)))
    (check "an author border wins over the image attribute"
           (and c cs
                (~ (closure.engine:box-width c) 22d0)
                (~ (closure.engine::computed-bt cs) 1d0 0.01d0)
                (> (second (closure.engine::computed-btc cs)) 0.5d0)))
    (let ((x (round (closure.engine:box-x a)))
          (y (round (closure.engine:box-y a))))
      (multiple-value-bind (r g b) (png-rgb path x y)
        (check "the image border is painted black"
               (and (< r 40) (< g 40) (< b 40)))))))

(defun test-hspace ()
  (let* ((png "file:///home/jmercouris/Source/closure/screenshots/square.png")
         (html (format nil "<img id='a' hspace='10' vspace='6' width='20' height='20' src='~A'><span id='t'>x</span><img id='b' hspace='10' style='margin-left:2px' width='20' height='20' src='~A'>" png png))
         (root (closure.engine:layout-html html :width 200 :stylesheet "body{margin:0}"))
         (a (closure.engine:find-box root "a"))
         (tail (closure.engine:find-box root "t"))
         (b (closure.engine:find-box root "b"))
         (as (and a (closure.engine::box-style a)))
         (bs (and b (closure.engine::box-style b))))
    (check "spaced images" (and a tail b as bs))
    (check "hspace and vspace are the image margins"
           (and as
                (~ (closure.engine::computed-ml as) 10d0 0.01d0)
                (~ (closure.engine::computed-mr as) 10d0 0.01d0)
                (~ (closure.engine::computed-mt as) 6d0 0.01d0)
                (~ (closure.engine::computed-mb as) 6d0 0.01d0)
                (~ (closure.engine:box-x a) 10d0)
                (~ (closure.engine:box-y a) 6d0)))
    (check "the following word starts after the right space"
           (and a tail
                (> (closure.engine:box-x tail)
                   (+ (closure.engine:box-x a) (closure.engine:box-width a) 8d0))))
    (check "an author margin wins on that side only"
           (and bs
                (~ (closure.engine::computed-ml bs) 2d0 0.01d0)
                (~ (closure.engine::computed-mr bs) 10d0 0.01d0)))))

(defun test-br-clear ()
  (labels ((at (html id)
             (closure.engine:find-box
              (closure.engine:layout-html html :width 200 :stylesheet "body{margin:0}")
              id)))
    (let ((cleared (at "<div style='float:left;width:40px;height:40px'></div>before<br clear='left'><span id='a'>after</span>" "a"))
          (beside (at "<div style='float:left;width:40px;height:40px'></div>before<br><span id='b'>beside</span>" "b"))
          (right (at "<div style='float:right;width:40px;height:40px'></div>side<br clear='right'><span id='c'>below</span>" "c"))
          (authored (at "<div style='float:left;width:40px;height:40px'></div>z<br clear='left' style='clear:none'><span id='d'>stay</span>" "d")))
      (check "cleared lines" (and cleared beside right authored))
      (check "br clear=left starts the next line below the float"
             (and cleared
                  (>= (closure.engine:box-y cleared) 38d0)
                  (< (closure.engine:box-x cleared) 8d0)))
      (check "a plain br stays beside the float"
             (and beside
                  (< (closure.engine:box-y beside) 20d0)
                  (> (closure.engine:box-x beside) 30d0)))
      (check "br clear=right starts below a right float"
             (and right (>= (closure.engine:box-y right) 38d0)))
      (check "an author clear wins over the br attribute"
             (and authored
                  (< (closure.engine:box-y authored) 20d0)
                  (> (closure.engine:box-x authored) 30d0))))))

(defun test-list-type-attr ()
  (let* ((html "<ol type='a' start='3'><li id='a'>One</li><li id='b'>Two</li></ol>
                <ol type='I'><li id='r' value='4'>Four</li><li id='s'>Five</li></ol>
                <ul type='square'><li id='q'>Item</li></ul>
                <ol type='a'><li id='css' style='list-style-type:decimal'>X</li></ol>")
         (root (closure.engine:layout-html html :width 200
                                          :stylesheet "body{margin:0}"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b"))
         (r (closure.engine:find-box root "r"))
         (s (closure.engine:find-box root "s"))
         (q (closure.engine:find-box root "q"))
         (css (closure.engine:find-box root "css")))
    (check "typed lists" (and a b r s q css))
    (check "type=a with start counts letters"
           (and a b
                (string= (closure.engine::box-marker a) "c.")
                (string= (closure.engine::box-marker b) "d.")))
    (check "type=I and value use roman numerals"
           (and r s
                (string= (closure.engine::box-marker r) "IV.")
                (string= (closure.engine::box-marker s) "V.")))
    (check "ul type=square uses a square marker"
           (and q (eq (closure.engine::box-marker q) :square)))
    (check "an author list-style-type wins over type"
           (and css (string= (closure.engine::box-marker css) "1.")))))

(defun test-nowrap-attr ()
  (let* ((html "<table id='wide' style='width:40px'><tr><td id='a' nowrap>one two three</td></tr></table>
                <table id='wrap' style='width:40px'><tr><td id='b'>one two three</td></tr></table>
                <table id='css' style='width:40px'><tr><td id='c' nowrap style='white-space:normal'>one two three</td></tr></table>")
         (root (closure.engine:layout-html html :width 200
                                          :stylesheet "body{margin:0}"))
         (wide (closure.engine:find-box root "wide"))
         (a (closure.engine:find-box root "a"))
         (wrap (closure.engine:find-box root "wrap"))
         (b (closure.engine:find-box root "b"))
         (css (closure.engine:find-box root "css"))
         (c (closure.engine:find-box root "c")))
    (check "nowrap cells" (and wide a wrap b css c))
    (check "nowrap keeps the cell on one line and widens the table"
           (and wide a wrap b
                (> (closure.engine:box-width wide) 70d0)
                (> (closure.engine:box-width wide) (+ (closure.engine:box-width wrap) 20d0))
                (< (closure.engine:box-height a) 30d0)
                (> (closure.engine:box-height b) 40d0)))
    (check "an author white-space wins over nowrap"
           (and css c
                (< (closure.engine:box-width css) 60d0)
                (> (closure.engine:box-height c) 40d0)
                (eq (closure.engine::computed-white-space (closure.engine::box-style c))
                    :normal)))))

(defun test-hr-size ()
  (let* ((html "<hr id='s' size='8'>
                <hr id='n' noshade size='6' width='80'>
                <hr id='one' size='1'>
                <hr id='css' size='10' style='height:4px'>")
         (path "/tmp/closure-engine-hr-size.png")
         (root (closure.engine:render-html html :width 200 :height 120
                                          :stylesheet "body{margin:0}"
                                          :output path))
         (sized (closure.engine:find-box root "s"))
         (flat (closure.engine:find-box root "n"))
         (one (closure.engine:find-box root "one"))
         (css (closure.engine:find-box root "css")))
    (check "sized rules" (and sized flat one css))
    (check "hr size is the border box"
           (and sized (~ (closure.engine:box-height sized) 8d0)))
    (let ((st (closure.engine::box-style flat)))
      (check "noshade is a solid gray bar of the given size"
             (and flat st
                  (~ (closure.engine:box-height flat) 6d0)
                  (~ (closure.engine:box-width flat) 82d0)
                  (eq (closure.engine::computed-bts st) :solid)
                  st (consp (closure.engine::computed-background st))
                  (> (first (closure.engine::computed-background st)) 0.4d0))))
    (let ((st (closure.engine::box-style one)))
      (check "hr size=1 keeps a single edge"
             (and one st
                  (~ (closure.engine:box-height one) 1d0)
                  (~ (closure.engine::computed-bb st) 0d0 0.01d0))))
    (check "an author height wins over hr size"
           (and css (~ (closure.engine:box-height css) 6d0)))
    (let ((x (round (+ (closure.engine:box-x sized) 8d0)))
          (top (round (closure.engine:box-y sized)))
          (mid (round (+ (closure.engine:box-y sized) 4d0)))
          (bot (round (+ (closure.engine:box-y sized) (closure.engine:box-height sized) -1d0))))
      (multiple-value-bind (r g b) (png-rgb path x top)
        (check "a sized rule's top edge is the dark gray"
               (and (< (abs (- r 154)) 20) (< (abs (- g 154)) 20) (< (abs (- b 154)) 20))))
      (multiple-value-bind (r g b) (png-rgb path x mid)
        (check "a sized rule leaves the middle empty"
               (and (> r 240) (> g 240) (> b 240))))
      (multiple-value-bind (r g b) (png-rgb path x bot)
        (check "a sized rule's bottom edge is the light gray"
               (and (< (abs (- r 238)) 20) (< (abs (- g 238)) 20) (< (abs (- b 238)) 20)))))
    (let ((x (round (+ (closure.engine:box-x flat) 8d0)))
          (y (round (+ (closure.engine:box-y flat) 3d0))))
      (multiple-value-bind (r g b) (png-rgb path x y)
        (check "noshade fills the bar gray"
               (and (< (abs (- r 128)) 30) (< (abs (- g 128)) 30) (< (abs (- b 128)) 30)))))))

(defun test-svg-text ()
  (let ((path "/tmp/closure-engine-svg-text.png"))
    (closure.engine:render-html
     "<svg width='200' height='40'><text x='8' y='28' fill='red' font-size='20'>Hi</text></svg>"
     :width 200 :height 60
     :stylesheet "body{margin:0}"
     :output path)
    (multiple-value-bind (r g b) (png-rgb path 10 15)
      (check "svg text is painted in its fill"
             (and (> r 200) (< g 50) (< b 50))))
    (multiple-value-bind (r g b) (png-rgb path 4 4)
      (check "svg text does not fill the viewport"
             (and (> r 240) (> g 240) (> b 240))))))

(defun test-svg-default ()
  (let* ((html "<svg id='plain'><rect width='100' height='50' fill='red'/></svg>
                <svg id='box' viewBox='0 0 100 50'><rect width='100' height='50' fill='green'/></svg>")
         (path "/tmp/closure-engine-svg-default.png")
         (root (closure.engine:render-html html :width 200 :height 400
                                          :stylesheet "body{margin:0}"
                                          :output path))
         (plain (closure.engine:find-box root "plain"))
         (boxed (closure.engine:find-box root "box")))
    (check "an svg without a size is 300 by 150"
           (and plain
                (~ (closure.engine:box-width plain) 300d0)
                (~ (closure.engine:box-height plain) 150d0)))
    (check "a viewBox svg fills the line and keeps its ratio"
           (and boxed
                (~ (closure.engine:box-width boxed) 200d0)
                (~ (closure.engine:box-height boxed) 100d0)))
    (multiple-value-bind (r g b) (png-rgb path 10 10)
      (check "the unsized svg paints its rect"
             (and (> r 180) (< g 40) (< b 40))))
    (multiple-value-bind (r g b) (png-rgb path 180 10)
      (check "the unsized svg leaves the rest of the viewport empty"
             (and (> r 240) (> g 240) (> b 240))))
    (multiple-value-bind (r g b)
        (png-rgb path 10 (round (+ (closure.engine:box-y boxed) 10)))
      (check "the viewBox svg paints its rect across the box"
             (and (< r 40) (> g 100) (< b 40))))))

(defun test-svg ()
  (let* ((html "<svg id='s' width='40' height='20' viewBox='0 0 40 20'>
                  <rect x='0' y='0' width='20' height='20' fill='#cc0000'/>
                  <path d='M20 0 H40 V20 H20 Z' fill='#00aa00'/>
                  <circle cx='10' cy='10' r='4' fill='#0000cc'/>
                </svg>")
         (out "/tmp/closure-engine-svg.png")
         (root (closure.engine:render-html html :width 80 :height 40 :output out
                                          :stylesheet "body{margin:0}"))
         (box (closure.engine:find-box root "s")))
    (check "svg box" box)
    (check "svg width" (and box (~ (closure.engine:box-width box) 40d0)))
    (check "svg height" (and box (~ (closure.engine:box-height box) 20d0)))
    (let ((x (round (closure.engine:box-x box)))
          (y (round (closure.engine:box-y box))))
      (multiple-value-bind (r g b) (png-rgb out (+ x 30) (+ y 4))
        (format t "~&  svg path pixel: ~D ~D ~D~%" r g b)
        (check "svg path painted green" (and (> g 120) (< r 80))))
      (multiple-value-bind (r g b) (png-rgb out (+ x 10) (+ y 10))
        (format t "~&  svg circle pixel: ~D ~D ~D~%" r g b)
        (check "svg circle painted blue" (and (> b 150) (< r 80) (< g 80)))))))

(defun test-location-history ()
  (let ((dir "/tmp/closure-loc/"))
    (ensure-directories-exist dir)
    (with-open-file (out (concatenate 'string dir "next.html")
                         :direction :output :if-exists :supersede)
      (write-string "<h1 id='there'>Arrived</h1>" out))
    (let ((base "file:///tmp/closure-loc/page.html"))
      (multiple-value-bind (root final departed)
          (closure.engine:layout-html
           "<p id='stay'>no</p><script>location.href = 'next.html';</script>"
           :width 200 :height 40 :stylesheet "body{margin:0}" :base base)
        (declare (ignore final))
        (check "redirect landed" (closure.engine:find-box root "there"))
        (check "departed page was kept" departed)
        (let* ((from (first departed))
               (history (closure.engine::browse-history-go
                         (closure.engine::browse-history-start (first from) (second from))
                         root base 0d0))
               (back (closure.engine::browse-history-back history))
               (previous (closure.engine::browse-history-current back)))
          (check "back is the page that assigned location.href"
                 (closure.engine:find-box (first previous) "stay")))))))

(defun test-location-href ()
  (let ((dir "/tmp/closure-loc/"))
    (ensure-directories-exist dir)
    (with-open-file (out (concatenate 'string dir "next.html")
                         :direction :output :if-exists :supersede)
      (write-string "<h1 id='there'>Arrived</h1>" out))
    (let* ((base "file:///tmp/closure-loc/page.html")
           (read-root (closure.engine:layout-html
                       "<div id='out'></div><script>document.getElementById('out').textContent = location.href;</script>"
                       :width 200 :height 40 :stylesheet "body{margin:0}" :base base))
           (out (closure.engine:find-box read-root "out"))
           (gone (closure.engine:layout-html
                  "<p id='stay'>no</p><script>location.href = 'next.html';</script>"
                  :width 200 :height 40 :stylesheet "body{margin:0}" :base base)))
      (check "script read location.href"
             (and out (search "page.html" (closure.engine::dom-text-content
                                           (closure.engine::box-element out)))))
      (check "location.href left the old page" (null (closure.engine:find-box gone "stay")))
      (check "location.href loaded the target" (closure.engine:find-box gone "there")))))

(defun test-document-title ()
  (let* ((html "<title>Old</title>
                <div id='out'></div>
                <script>
                  document.getElementById('out').textContent = document.title;
                  document.title = 'New';
                </script>")
         (root (closure.engine:layout-html html :width 200 :height 40
                                          :stylesheet "body{margin:0}"))
         (out (closure.engine:find-box root "out")))
    (check "script read the old title"
           (and out (search "Old" (closure.engine::dom-text-content
                                   (closure.engine::box-element out)))))
    (check "script title reaches the window"
           (search "New" (closure.engine::window-title root "file:///x")))))

(defun test-post-form ()
  (let* ((port 18766)
         (script "/tmp/closure-post-server.py")
         (body-file "/tmp/closure-post-body"))
    (with-open-file (out script :direction :output :if-exists :supersede)
      (write-string "from http.server import BaseHTTPRequestHandler, HTTPServer
class H(BaseHTTPRequestHandler):
    def do_POST(self):
        n = int(self.headers.get('Content-Length', '0'))
        open('/tmp/closure-post-body', 'wb').write(self.rfile.read(n))
        data = b\"<p id='posted'>ok</p>\"
        self.send_response(200)
        self.send_header('Content-Type', 'text/html')
        self.send_header('Content-Length', str(len(data)))
        self.end_headers()
        self.wfile.write(data)
    def log_message(self, fmt, *args):
        pass
HTTPServer(('127.0.0.1', 18766), H).serve_forever()
" out))
    (let ((proc (uiop:launch-program (list "python3" script)
                                     :output "/tmp/closure-post.log"
                                     :error-output :output)))
      (unwind-protect
           (progn
             (loop repeat 50
                   until (ignore-errors
                          (drakma:http-request (format nil "http://127.0.0.1:~D/" port)
                                               :method :post :content "ping=1")
                          t)
                   do (sleep 0.05))
             (let* ((base "http://127.0.0.1:18766/")
                    (html "<form method='post' action='/take'>
                             <input name='q' value='hello world'>
                             <input type='submit' id='send' value='Go'>
                           </form>")
                    (root (closure.engine:layout-html html :width 300 :height 80
                                                     :stylesheet "body{margin:0}" :base base))
                    (send (closure.engine:find-box root "send")))
               (check "post button" send)
               (multiple-value-bind (next url)
                   (closure.engine:submit-form root
                                               (+ (closure.engine:box-x send) 1d0)
                                               (+ (closure.engine:box-y send) 1d0)
                                               0 base 300 80)
                 (check "post painted the response"
                        (and next (closure.engine:find-box next "posted")))
                 (check "post url has no query" (and url (not (search "q=" url))))
                 (let ((sent (ignore-errors (uiop:read-file-string body-file))))
                   (check "post body reached the server"
                          (and sent (search "q=hello+world" sent)))))))
        (uiop:terminate-process proc)
        (ignore-errors (uiop:wait-process proc))))))

(defun test-history ()
  (let* ((start (closure.engine::browse-history-start :page-a "file:///a"))
         (gone (closure.engine::browse-history-go start :page-b "file:///b" 12d0))
         (back (closure.engine::browse-history-back gone))
         (fwd (closure.engine::browse-history-forward back)))
    (check "history opens the new page"
           (eq (first (closure.engine::browse-history-current gone)) :page-b))
    (check "back returns to the first page"
           (eq (first (closure.engine::browse-history-current back)) :page-a))
    (check "back restores the scroll offset"
           (~ (third (closure.engine::browse-history-current back)) 12d0))
    (check "back at the start stays put"
           (eq (closure.engine::browse-history-back back) back))
    (check "forward returns to the new page"
           (eq (first (closure.engine::browse-history-current fwd)) :page-b))
    (check "forward at the end stays put"
           (eq (closure.engine::browse-history-forward gone) gone))
    (check "backspace is back" (eq (closure.engine::event-scroll-command :key #xff08) :back))
    (check "shift-backspace is forward"
           (eq (closure.engine::event-scroll-command :key #xff08 1) :forward))
    (check "window title carries the url"
           (search "file:///b" (closure.engine::window-title nil "file:///b")))
    (let ((rooted (closure.engine:layout-html
                   "<title>Hello</title><p id='p'>x</p>"
                   :width 80 :height 40 :stylesheet "body{margin:0}")))
      (check "document title wins"
             (search "Hello" (closure.engine::window-title rooted "file:///x"))))))

(defun test-submit-form ()
  (let ((dir "/tmp/closure-form/"))
    (ensure-directories-exist dir)
    (with-open-file (out (concatenate 'string dir "result.html")
                         :direction :output :if-exists :supersede)
      (write-string "<p id='done'>ok</p>" out))
    (let* ((base "file:///tmp/closure-form/page.html")
           (html "<form action='result.html'>
                    <input name='q' value='hello world'>
                    <input type='checkbox' name='extra'>
                    <input type='submit' id='send' value='Go'>
                  </form>")
           (root (closure.engine:layout-html html :width 300 :height 80
                                            :stylesheet "body{margin:0}" :base base))
           (send (closure.engine:find-box root "send")))
      (check "submit button" send)
      (multiple-value-bind (next url)
          (closure.engine:submit-form root
                                      (+ (closure.engine:box-x send) 1d0)
                                      (+ (closure.engine:box-y send) 1d0)
                                      0 base 300 80)
        (check "get reached the action" (and next (closure.engine:find-box next "done")))
        (check "query encodes the text field" (and url (search "q=hello+world" url)))
        (check "unchecked box is omitted" (and url (not (search "extra=" url))))))))

(defun test-follow-link ()
  (let ((dir "/tmp/closure-nav/"))
    (ensure-directories-exist dir)
    (with-open-file (out (concatenate 'string dir "next.html")
                         :direction :output :if-exists :supersede)
      (write-string "<h1 id='there'>Arrived</h1>" out))
    (let* ((base "file:///tmp/closure-nav/page.html")
           (root (closure.engine:layout-html
                  "<a id='go' href='next.html'>Go</a>"
                  :width 200 :height 40 :stylesheet "body{margin:0}" :base base))
           (go (closure.engine:find-box root "go")))
      (check "link box" go)
      (check "link href" (and go (equal (closure.engine:link-href go) "next.html")))
      (multiple-value-bind (next new-base)
          (closure.engine:follow-link root
                                      (+ (closure.engine:box-x go) 1d0)
                                      (+ (closure.engine:box-y go) 1d0)
                                      0 base 200 40)
        (check "followed to next page" (and next (closure.engine:find-box next "there")))
        (check "base is the target" (and new-base (search "next.html" new-base)))))))

(defun test-window-scroll ()
  (check "wheel down" (~ (closure.engine::scroll-step 0 :wheel-down 100 500) 48d0))
  (check "scroll clamps" (~ (closure.engine::scroll-step 380 :wheel-down 100 500) 400d0))
  (check "page down" (~ (closure.engine::scroll-step 0 :page-down 100 500) 100d0))
  (check "home" (~ (closure.engine::scroll-step 40 :home 100 500) 0d0))
  (check "button 4 is wheel up" (eq (closure.engine::event-scroll-command :button 4) :wheel-up))
  (check "escape quits" (eq (closure.engine::event-scroll-command :key #xff1b) :quit))
  (let ((root (closure.engine:layout-html "<p>Hi</p>" :width 80 :height 40
                                         :stylesheet "body{margin:0}")))
    (check "run-window without a display" (null (closure.engine:run-window root :width 80 :height 40)))))

(defun test-scroll-and-hit ()
  (let* ((html "<div id='a' style='position:absolute;top:0;left:0;width:20px;height:20px;background:#c00'></div>
                <div id='b' style='position:absolute;top:100px;left:0;width:20px;height:20px;background:#0c0'></div>")
         (out "/tmp/closure-engine-scroll.png")
         (root (closure.engine:render-html html :width 80 :height 30 :scroll-y 100
                                          :output out :stylesheet "body{margin:0}"))
         (a (closure.engine:find-box root "a"))
         (b (closure.engine:find-box root "b")))
    (check "scroll boxes" (and a b))
    (check "hit at top is a" (eq (closure.engine:hit-test root 5 5) a))
    (check "scrolled hit is b" (eq (closure.engine:hit-test root 5 5 100) b))
    (multiple-value-bind (r g bch) (png-rgb out 4 4)
      (format t "~&  scrolled pixel: ~D ~D ~D~%" r g bch)
      (check "viewport shows the lower box" (and (> g 150) (< r 80))))
    (check "no display means no window" (null (closure.engine:paint-window root :width 80 :height 30)))))

(defun test-lisp-script ()
  (let ((dir "/tmp/closure-js/"))
    (ensure-directories-exist dir)
    (with-open-file (out (concatenate 'string dir "set.js")
                         :direction :output :if-exists :supersede)
      (write-string "document.getElementById('from').textContent = 'from-file';" out))
    (let* ((html "<h1 id='from'>no</h1><script src='set.js'></script>")
           (root (closure.engine:layout-html html :width 200 :height 40
                                            :stylesheet "body{margin:0}"
                                            :base "file:///tmp/closure-js/page.html"))
           (box (closure.engine:find-box root "from")))
      (check "external script box" box)
      (check "lisp ran the external script"
             (and box (search "from-file"
                              (closure.engine::dom-text-content
                               (closure.engine::box-element box))))))))

(defun test-external-css ()
  (let ((dir "/tmp/closure-css/"))
    (ensure-directories-exist dir)
    (with-open-file (out (concatenate 'string dir "b.css")
                         :direction :output :if-exists :supersede)
      (write-string "#ext { width: 70px; height: 12px; background: #00aa00; }" out))
    (with-open-file (out (concatenate 'string dir "a.css")
                         :direction :output :if-exists :supersede)
      (write-string "@import \"b.css\"; #ext { margin-left: 4px; }" out))
    (let* ((html "<link rel='stylesheet' href='a.css'><div id='ext'>x</div>")
           (root (closure.engine:layout-html html :width 200 :height 40
                                            :stylesheet "body{margin:0}"
                                            :base "file:///tmp/closure-css/page.html"))
           (box (closure.engine:find-box root "ext")))
      (check "linked sheet box" box)
      (check "imported width" (and box (~ (closure.engine:box-width box) 70d0)))
      (check "sheet margin" (and box (~ (closure.engine:box-x box) 4d0))))))

(defun run-tests ()
  (format t "~&Closure engine tests~%")
  (test-javascript)
  (test-png-image)
  (test-gif-and-jpeg)
  (test-relative-and-http-image)
  (test-background-and-object-fit)
  (test-background-no-repeat)
  (test-background-position-right)
  (test-background-position-center)
  (test-background-position-bottom)
  (test-background-position-right-bottom)
  (test-background-size-contain)
  (test-background-size-cover)
  (test-background-size-lengths)
  (test-background-size-percent)
  (test-background-position-percent)
  (test-dir-optgroup)
  (test-tfoot-menu)
  (test-slot-columns)
  (test-picture-map)
  (test-dialog-search)
  (test-iframe-border)
  (test-iframe-object)
  (test-form-metrics)
  (test-form-controls)
  (test-external-css)
  (test-lisp-script)
  (test-scroll-and-hit)
  (test-window-scroll)
  (test-follow-link)
  (test-submit-form)
  (test-history)
  (test-post-form)
  (test-document-title)
  (test-location-href)
  (test-location-history)
  (test-inline-layout)
  (test-sub-sup)
  (test-mark)
  (test-line-through)
  (test-list-markers)
  (test-hr)
  (test-input-size)
  (test-textarea-size)
  (test-placeholder)
  (test-disabled)
  (test-dir-attr)
  (test-hidden-attr)
  (test-reversed-list)
  (test-hr-color)
  (test-image-border)
  (test-hspace)
  (test-br-clear)
  (test-list-type-attr)
  (test-nowrap-attr)
  (test-hr-size)
  (test-q)
  (test-blockquote)
  (test-nested-q)
  (test-dl)
  (test-figure)
  (test-small)
  (test-big)
  (test-u-ins)
  (test-overline)
  (test-underline-and-overline)
  (test-underline-and-line-through)
  (test-text-decoration-color)
  (test-text-underline-offset)
  (test-text-decoration-thickness)
  (test-text-decoration-dashed)
  (test-text-decoration-wavy)
  (test-text-decoration-double)
  (test-text-decoration-dotted)
  (test-text-shadow)
  (test-text-shadow-two)
  (test-linear-gradient)
  (test-linear-gradient-right)
  (test-linear-gradient-angle)
  (test-linear-gradient-stop)
  (test-radial-gradient)
  (test-radial-gradient-closest-side)
  (test-radial-gradient-at-corner)
  (test-repeating-linear-gradient)
  (test-code-font)
  (test-tt)
  (test-nobr)
  (test-text-overflow)
  (test-overflow-wrap)
  (test-word-break)
  (test-caption-wraps)
  (test-kerning)
  (test-pre-lines)
  (test-normal-line-height)
  (test-default-serif)
  (test-justify)
  (test-collapsed-cell-half)
  (test-em)
  (test-border-collapse)
  (test-border-spacing)
  (test-colspan)
  (test-rowspan)
  (test-table-valign)
  (test-table-valign-middle)
  (test-table-auto-width)
  (test-caption)
  (test-caption-bottom)
  (test-empty-cells)
  (test-list-style-inside)
  (test-list-style-square)
  (test-list-style-circle)
  (test-list-style-image)
  (test-list-style-image-none)
  (test-background-clip-content)
  (test-background-clip-padding)
  (test-background-image-clip-content)
  (test-background-origin-content)
  (test-background-origin-border)
  (test-background-image-clip-padding)
  (test-background-repeat-space)
  (test-background-repeat-round)
  (test-object-position-right)
  (test-object-position-bottom)
  (test-object-position-left-bottom)
  (test-object-position-percent)
  (test-aspect-ratio)
  (test-aspect-ratio-width)
  (test-aspect-ratio-content)
  (test-aspect-ratio-image)
  (test-aspect-ratio-image-height)
  (test-wbr)
  (test-address)
  (test-ruby-rtc)
  (test-ruby-rb)
  (test-ruby)
  (test-table-size-attr)
  (test-valign-attr)
  (test-align-attr)
  (test-bgcolor)
  (test-table-cell-attrs)
  (test-table-bordercolor)
  (test-table-frame)
  (test-table-rules)
  (test-table-border-attr)
  (test-frameset)
  (test-embed-paint)
  (test-embed-size)
  (test-obsolete-tail)
  (test-listing-abbr)
  (test-font-marquee)
  (test-replaced-widgets)
  (test-check-line)
  (test-check-paint)
  (test-select-arrow)
  (test-button-border)
  (test-textarea-grip)
  (test-password-paint)
  (test-image-input)
  (test-select-list)
  (test-disclosure-marker)
  (test-ordered-marker)
  (test-file-input-paint)
  (test-range-color-paint)
  (test-gauge-paint)
  (test-text-input-border)
  (test-time-paint)
  (test-date-input)
  (test-month-week)
  (test-time-input)
  (test-control-baseline)
  (test-input-widgets)
  (test-check-submit)
  (test-open-details)
  (test-fieldset-border)
  (test-fieldset-details)
  (test-margin-unit)
  (test-smaller-headings)
  (test-br-and-center)
  (test-text-indent)
  (test-letter-spacing)
  (test-word-spacing)
  (test-text-transform)
  (test-visibility-hidden)
  (test-outline)
  (test-outline-dashed)
  (test-outline-dotted)
  (test-outline-double)
  (test-border-double)
  (test-border-groove)
  (test-border-ridge)
  (test-border-inset)
  (test-border-outset)
  (test-solid-border-rows)
  (test-outline-offset)
  (test-box-shadow)
  (test-inset-shadow)
  (test-two-shadows)
  (test-float-left)
  (test-float-right)
  (test-clear-both)
  (test-relative)
  (test-relative-text)
  (test-auto-margin)
  (test-margin-left-auto)
  (test-margin-collapse)
  (test-parent-child-margin)
  (test-parent-child-bottom-margin)
  (test-margin-separation)
  (test-margin-bottom-separation)
  (test-float-below)
  (test-br)
  (test-nested-inline)
  (test-link-underline)
  (test-link-underline-nested)
  (test-inline-image-baseline)
  (test-inline-image-middle)
  (test-image-alt)
  (test-image-alt-clip)
  (test-svg-text)
  (test-svg-default)
  (test-svg)
  (test-selectors-and-cascade)
  (test-flex)
  (test-flex-wrap)
  (test-flex-wrap-reverse)
  (test-flex-align-content)
  (test-flex-align-content-end)
  (test-flex-align-content-between)
  (test-flex-align-content-around)
  (test-flex-align-content-evenly)
  (test-grid-and-calc)
  (test-absolute-and-media)
  (test-paint-and-speed)
  (format t "~&All engine tests passed.~%")
  t)
