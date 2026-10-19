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

(defun run-tests ()
  (format t "~&Closure engine tests~%")
  (test-selectors-and-cascade)
  (test-flex)
  (test-grid-and-calc)
  (test-absolute-and-media)
  (test-paint-and-speed)
  (format t "~&All engine tests passed.~%")
  t)
