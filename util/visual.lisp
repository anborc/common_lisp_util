;;;; visual.lisp
(in-package #:util)

;;divide the line segment, [point0, point1], into n parts. return the first part's end point
(defun between-point (point0 point1 &key (n 2))
  (list
    (+ (/ (- (car point1) (car point0)) (float n)) (car point0))
    (+ (/ (- (second point1) (second point0)) (float n)) (second point0))))
;;(between-point '(1 1) '(2 2) :n 2)

(defun bezier (bezier-points &optional points)
  (let ((width 700) (height 700))
    (sdl:with-init ()
      (sdl:window width height :position nil
	:title-caption "Bezier, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 255 :g 255 :b 255))
      (sdl:with-color (a-col (sdl:color :r 0 :g 0 :b 0))
	(dolist (p bezier-points) 
	  (sdl:draw-bezier (list
			     (sdl:point :x (round (car (car p))) :y (round (second (car p))))
			     (sdl:point :x (round (car (cadr p))) :y (round (second (cadr p))))
			     (sdl:point :x (round (car (caddr p))) :y (round (second (caddr p))))
			     (sdl:point :x (round (car (cadddr p))) :y (round (second (cadddr p))))
			     )
	    ;:color sdl:*red*
	    )))
      (sdl:draw-circle-* 350 350 50 :color sdl:*red*)
      (when points (sdl:with-color (a-col (sdl:color :r 255 :g 0 :b 0))
	  (sdl:draw-shape (mapcar #'(lambda (p) (sdl:point :x (round (car p)) :y (round (second p)))) points))))
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
        (:key-down-event ()
			 (when (sdl:key-down-p :sdl-key-escape)
			   (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display))))))

(defun test-bezier0 ()
  ;;concave polygon, convex polygon, self-intersecting polygon
  ;;(defparameter points '((300 300) (400 300) (400 400) (350 350) (300 400))) ;square within triangle
  ;;(defparameter points '((300 300) (400 300) (350 350) (400 400) (350 350) (300 400))) "umbrella"
  (defparameter points '((300 300) (400 300) (350 350) (400 400) (300 400) (350 350))) ;hourglass n0=2 n1=1
  ;;(defparameter points '((300 300) (400 300) (400 400) (300 400))) ;square
  ;;(defparameter points '((300 300) (400 300) (300 400) (400 400))) ;self-intersecting square like "8"
  ;;(defparameter points '((300 300) (350 350) (400 300) (350 350) (400 400) (350 350) (300 400) (350 350))) ;"x"
  ;;(defparameter points '((300 300) (400 300) (350 350))) ;triangle
  (defparameter m-points '())
  (defparameter mm-points '())
  (defparameter bezier-points '())
  ;;when n0 = 1 --> n1 >= 1 the same as original polygon, n1 < 1 extend each edge in one direction, more smaller n1 is, longer the extended edge is.
  ;;when n1 = 0.2 --> n0 changing from 2 to 1, a petal sharped to one line 多维空间卷缩到一维空间  
  (let ((n0 2)) ;default n0 = 2  
    (dotimes (i (length points))
    (if (/= i (- (length points) 1))
      (setf m-points (append m-points (list (between-point (nth i points) (nth (+ 1 i) points) :n n0))))
      (setf m-points (append m-points (list (between-point (nth i points) (car points) :n n0))))
      )))
  (print m-points)
  (let ((n1 2)) ;n: <0.5 crossing-flower, < 0.6 inner-sharp, < 1 inner-round, <= 2 smooth-round, > 2 close-to-middle-points
    ;n1: prototype 0.2 crossing-flower, 0.8 self similar polygon, 1.8 round 
    (dotimes (i (length m-points))
      (if (/= i (- (length m-points) 1))
	(setf mm-points (append mm-points (list (between-point (nth i m-points) (nth (+ 1 i) points) :n n1)
					    (between-point (nth (+ 1 i) m-points) (nth (+ 1 i) points) :n n1))))
	(setf mm-points (append mm-points (list (between-point (nth i m-points) (car points) :n n1)
					    (between-point (car m-points) (car points) :n n1)))))))
  (print mm-points)
  (dotimes (i (length m-points))
    (if (/= i (- (length m-points) 1))
      (push (list (nth i m-points) (nth (* i 2) mm-points) (nth (+ 1 (* i 2)) mm-points) (nth (+ 1 i) m-points)) bezier-points)
      (push (list (nth i m-points) (nth (* i 2) mm-points) (nth (+ 1 (* i 2)) mm-points) (car m-points)) bezier-points)
      ))
  (print bezier-points)
  ;(bezier bezier-points points)
  (bezier bezier-points)
  )
;(test-bezier0)

(defun get-bezier-points (&key (points '((300 300) (400 300) (400 400) (300 400))) (n0 2) (n1 1.8))
  (let ((m-points '()) (mm-points '()) (bezier-points '()))
    (dotimes (i (length points))
      (if (/= i (- (length points) 1))
	(setf m-points (append m-points (list (between-point (nth i points) (nth (+ 1 i) points) :n n0))))
	(setf m-points (append m-points (list (between-point (nth i points) (car points) :n n0))))))
    (dotimes (i (length m-points))
      (if (/= i (- (length m-points) 1))
	(setf mm-points (append mm-points (list (between-point (nth i m-points) (nth (+ 1 i) points) :n n1)
					    (between-point (nth (+ 1 i) m-points) (nth (+ 1 i) points) :n n1))))
	(setf mm-points (append mm-points (list (between-point (nth i m-points) (car points) :n n1)
					    (between-point (car m-points) (car points) :n n1))))))
    (dotimes (i (length m-points))
      (if (/= i (- (length m-points) 1))
	(push (list (nth i m-points) (nth (* i 2) mm-points) (nth (+ 1 (* i 2)) mm-points) (nth (+ 1 i) m-points)) bezier-points)
	(push (list (nth i m-points) (nth (* i 2) mm-points) (nth (+ 1 (* i 2)) mm-points) (car m-points)) bezier-points)))
    bezier-points))

(defun test-bezier ()
  (bezier (get-bezier-points))
  )
;(test-bezier)

#||
prototyp
n0=2,n1=0.2 crossing-flower
n0=1,n1=1   self polygon
n0=2,n1=2   round 
concave polygon, convex polygon, self-intersecting polygon
||#

;;;; lispbuilder and opengl -----------------------------------------------------------------

(defun draw-triangle2 (&key (origin '(0 0)) (width 300) (height 300))
  (gl:viewport (car origin) (second origin) width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -3 5 -5 5 -1 1)
  (gl:disable :depth-test :lighting :light0 :light1)
  (gl:with-primitive :triangles
    (gl:color 1 0 0)
    (gl:vertex -1 -1 0) 
    (gl:color 0 1 0)
    (gl:vertex 0 1 0)   
    (gl:color 0 0 1)
    (gl:vertex 1 -1 0)) 
  )

(defun gl-circle (&key (center '(0 0)) (r 1) (segments 36) (color '(1 1 0 1)) (stroke-color '(0 0 0 0)))
  (let ((points (polygon (make-turtle) :center center :radius r :n segments)))
    (when (/= (fourth color) 0)
      (apply #'gl:color color)
      (dotimes (i (length points))
	(gl:with-primitive :triangles
	  (gl:vertex (car center) (second center) 0)
	  (gl:vertex (car (nth i points)) (second (nth i points)) 0)
	  (if (< i (- (length points) 1))
	    (gl:vertex (car (nth (+ i 1) points)) (second (nth (+ i 1) points)) 0)
	    (gl:vertex (car (nth 0 points)) (second (nth 0 points)) 0)))))
    (when (/= (fourth stroke-color) 0)
      (apply #'gl:color stroke-color)
      (gl:with-primitives :line-loop
	(dolist (p points)
	  (gl:vertex (car p) (second p)))))))

(defun gl-triangle (&key (triangle '((0 0)(2 0)(1 2))) (color '(1 1 1 1)) (stroke-color '(0 0 0 1)))
  (when (/= (fourth color) 0)
    (apply #'gl:color color)
    (gl:with-primitive :triangles
      (dolist (p triangle)
	(gl:vertex (car p) (second p)))))
  (when (/= (fourth stroke-color) 0)
    (apply #'gl:color stroke-color)
    (gl:with-primitives :line-loop
      (dolist (p triangle)
	(gl:vertex (car p) (second p))))))

(defun gl-rectangle (&key (min-p '(0 0)) (max-p '(1 1)) (color '(1 1 0 1)) (stroke-color '(0 0 0 0)))
  (when (/= (fourth color) 0)
    (apply #'gl:color color)
    (gl:rect (car min-p) (second min-p) (car max-p) (second max-p)))
  (when (/= (fourth stroke-color) 0)
    (apply #'gl:color stroke-color)
    (gl:with-primitives :line-loop
      (gl:vertex (car min-p) (second min-p))
      (gl:vertex (car max-p) (second min-p))
      (gl:vertex (car max-p) (second max-p))
      (gl:vertex (car min-p) (second max-p))
      )))

(defun gl-polygon (&key (polygon '((0 0)(2 0)(1 2))) (color '(1 1 1 1)) (stroke-color '(0 0 0 1)))
  (if (= (length polygon) 3)
    (gl-triangle :triangle polygon :color color :stroke-color stroke-color)
    (progn
      (when (/= (fourth color) 0)
	(apply #'gl:color color)
	(let ((triangles (trianglize-edges :edge-list (edge-list-from-point-list polygon))))
	  ;;(show "triangles {}" triangles)
	  ;;(show "number of triangles: {}" (length triangles))
	  ;;(dolist (tri triangles) (show "centroid-of-triangle {} of tri {}" (centroid-of-triangle :triangle tri) tri))
	  (setf triangles (remove-if-not (lambda (tri) (point-in-polygon :point (centroid-of-triangle :triangle tri) :polygon polygon)) triangles))
	  (dolist (triangle triangles)
	    ;;(gl-triangle :triangle triangle :color color :stroke-color stroke-color)
	    (gl:with-primitive :triangles (dolist (p triangle) (gl:vertex (car p) (second p))))
	    )))
      (when (/= (fourth stroke-color) 0)
	(apply #'gl:color stroke-color)
	(gl:with-primitives :line-loop
	  (dolist (p polygon)
	    (gl:vertex (car p) (second p))))))))

(defun before-visual-2D (&key (origin '(0 0)) (width 300) (height 300) (color '(1 0 0 1)) (border? t))
  (gl:viewport (car origin) (second origin) width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho (car origin) (+ (car origin) width) (second origin) (+ (second origin) height) -1 1)
  (gl:disable :depth-test :lighting :light0 :light1)
  (when border? (draw-border :origin origin :width width :height height))
  )

(defun before-visual-3D (&key (origin '(0 0)) (width 300) (height 300) (border? t))
  ;;(when border? (before-visual-2D :origin origin :width width :height height :border? t))
  (gl:viewport (car origin) (second origin) width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (when border?
    (gl:ortho (car origin) (+ (car origin) width) (second origin) (+ (second origin) height) -1 1)
    (gl:disable :depth-test :lighting :light0 :light1)
    (draw-border :origin origin :width width :height height)
    )
  (gl:load-identity)
  (glu:perspective 50 (/ width height) 1.0 10.0)
  (glu:look-at -2 2 3 0 0 0 0 1 0)
  (gl:light :light0 :position #(0 0 1 0))
  (gl:light :light0 :diffuse #(1 0 0 0))
  (gl:light :light1 :position #(-1 2 -0.5 0))
  (gl:light :light1 :diffuse #(0 1 0 0))
  (gl:enable :depth-test :lighting :light0 :light1)
  ;;(gl:cull-face :back)
  ;;(gl:depth-func :less)
  ;;(gl:disable :dither)
  (gl:shade-model :smooth)
  ;;(gl:light-model :light-model-local-viewer 1)
  ;;(gl:color-material :front :ambient-and-diffuse)
  ;;(gl:enable :light0 :lighting :cull-face :depth-test)
  ;;(gl:load-identity)
  ;;(gl:translate 0 0 -5)
  ;;(gl:rotate 30 1 1 0)
  ;;(gl:clear :color-buffer-bit :depth-buffer-bit)
  ;;(gl:color 1 1 1)
  ;;(gl:front-face :cw)
  ;;(glut:solid-teapot 1)
  ;;(gl:front-face :ccw)
  )

(defun draw-scene (&key (origin '(0 0)) (width 300) (height 300) (s (get-context)))
  ;;sea
  (gl:color 0.7 0.7 0.7)
  (show "s: {}" s)
  (show "(>> s 'h): {}" (>> s 'h))
  (gl:rect (car origin) (second origin) (- (+ (car origin) width) 1) (+ (second origin) (* height (>> s 'h))))
  (dolist (e (remove-if (lambda (x) (equal x 'h)) (hash-keys s)))
    (show "(>> s e): {}" (>> s e))
    (let ((x (+ (car origin) (* width (>> s `(,e x)))))
	   (y (+ (second origin) (* height (>> s `(,e y)))))
	   (r (* (min width height) (>> s `(,e r)))))
      (cond
	((equal e 'circle)
	  (gl-circle :center (list x y) :r r :color '(1 1 0 1) :stroke-color '(0 0 0 0)))
	((equal e 'triangle)
	  (let ((points (triangle (make-turtle) :center (list x y) :radius r)))
	    (gl:color 0 1 0)
	    (gl:with-primitive :triangles
	      (dolist (p points)
		(gl:vertex (car p) (second p) 0)))))
	((equal e 'square)
	  (progn
	    (gl:color 0 0 1)
	    (gl:rect (- x r) (- y r) (+ x r) (+ y r)))
	  ))))
  ;;border
  (gl:color 0 0 0)
  (gl:with-primitives :line-loop
    (gl:vertex (car origin) (second origin))
    (gl:vertex (+ (car origin) (- width 1)) (second origin))
    (gl:vertex (+ (car origin) (- width 1)) (+ (second origin) (- height 1)))
    (gl:vertex (car origin) (+ (second origin) (- height 1)))))

(defun get-scene2 ()
  (let* ((scene '(rectangle (0 0) (1 1)))
	  (land '(polygon (0.34 0) (1 0) (1 0.4) (0.48 0.4)))
	  (i-point (line-segments-intersection-point :line-segment0 '((0.34 0) (0.48 0.4)) :line-segment1 '((0 0.34) (1 0.34))))
	  (sea (list 'polygon '(0 0) '(0.34 0) (list (car i-point) (second i-point)) (list 0 (second i-point))))
	  (sky (list 'polygon (car (last sea)) (fourth sea) (car (last land)) (fourth land) (list 1 1) (list 0 1)))
	  (sun '(circle (0.2 0.8) 0.06))
	  (bow '(triangle (0.091 0.41) (0.16 0.34) (0.16 0.41))) ;;boat head
	  (hull '(rectangle (0.16 0.34) (0.226 0.41)))          ;;boat body
	  (poop '(triangle (0.226 0.34) (0.294 0.41) (0.226 0.41)));;boat tail
	  (sail '(triangle (0.164 0.548) (0.164 0.41) (0.254 0.478)))  ;;boat sail
	  (fish-head '(triangle (0.164 0.129) (0.164 0.215) (0.119 0.172)))
	  (fish-body '(triangle (0.164 0.215) (0.164 0.129) (0.217 0.172)))
	  (fishtail '(triangle (0.252 0.144) (0.252 0.201) (0.217 0.172)))
	  (fisheye '(circle (0.147 0.172) 0.012))
	  (wall '(rectangle (0.54 0.4) (0.74 0.55)))
	  (window '(rectangle (0.57 0.47) (0.63 0.53)))
	  (door '(rectangle (0.66 0.4) (0.72 0.52)))
	  (roof '(triangle (0.5 0.55) (0.78 0.55) (0.64 0.64)))
	  (trunk '(rectangle (0.846 0.4) (0.899 0.53)))
	  (crown '(triangle (0.802 0.53) (0.942 0.53) (0.872 0.62)))
	  (crown2 '(triangle (0.836 0.62) (0.908 0.62) (0.872 0.67))))
    (>> 'scene scene 'land land 'sea sea 'sky sky 'sun sun 'bow bow 'hull hull 'poop poop 'sail sail 'fish-head fish-head 'fish-body fish-body 'fishtail fishtail 'fisheye fisheye 'wall wall 'window window 'door door 'roof roof 'trunk trunk 'crown crown 'crown2 crown2)))
;;(show "{}" (hash2lst (get-scene2)))

(defun get-scene3 (&key (s (>>)))
  (>> s 'sce (>> 'shape 'rect 'x 50 'y 50 'rx 50 'ry 50 'a 0 'type 'bg)) ;scene
  (let ((x 50)(y 67)) (>> s 'sky (>> 'shape 'rect 'x x 'y y 'rx x 'ry (- 100 y) 'a 0 'type 'bg))) ;a:angle [-1, 1]-->[-pi, pi]
  (let* ((x (>> s `(sky x)))
	  (y-max (- (>> s `(sky y)) (>> s `(sky ry))))	  
	  (y (/ y-max 2)))
    (>> s 'sea (>> 'shape 'rect 'x x 'y y 'rx x 'ry y 'a 0 'type 'bg))
    (>> s 'lan (>> 'shape 'rect 'x 70 'y (+ y 2) 'rx 25 'ry (+ y 2) 'a 0 'type 'bg))) ;land
  ;;(>> s 'sun2 (>> 'shape 'elli 'x 20 'y 85 'rx (/ 63 5) 'ry (/ 63 10) 'a 1/4 'type 'obj))
  (>> s 'sun (>> 'shape 'elli 'x 20 'y 85 'rx (/ 63 10) 'ry (/ 63 10) 'a 0 'type 'obj))
  (let* ((rx (/ (>> s `(lan rx)) 2)) (ry (* 2 (/ rx 3))) (x (+ (- (>> s `(lan x)) (>> s `(lan rx))) rx 5)) (y (+ (>> s `(lan y)) (>> s `(lan ry)) ry)))
    (>> s 'wal (>> 'shape 'rect 'x x 'y y 'rx rx 'ry ry 'a 0 'type 'obj)) ;wall
    (>> s 'rof (>> 'shape 'tria 'x x 'y (+ y ry) 'rx (* 2 (/ rx 3)) 'ry (/ (* rx 13) 10) 'a 1/2 'type 'obj))) ;roof
  (let* ((rx (/ (>> s `(wal ry)) 2)) (x (- (>> s `(wal x)) (/ (>> s `(wal rx)) 2))) (y (+ (>> s `(wal y)) (/ (>> s `(wal ry)) 4))))
    (>> s 'win (>> 'shape 'rect 'x x 'y y 'rx rx 'ry rx 'a 0 'type 'obj))) ;window
  (let* ((rx (/ (* (>> s `(wal ry)) 2) 5)) (x (+ (>> s `(wal x)) (/ (>> s `(wal rx)) 2)))
	  (ry (/ (* (>> s `(wal ry)) 4) 5))
	  (y (+ ry (- (>> s `(wal y)) (>> s `(wal ry))))))
    (>> s 'dor (>> 'shape 'rect 'x x 'y y 'rx ry 'ry rx 'a 1/2 'type 'obj))) ;door
  (let* ((rx (/ (>> s `(lan rx)) 11)) (ry (* 3 rx)) (x (- (+ (>> s `(lan x)) (>> s `(lan rx))) (+ rx 5))) (y (+ (>> s `(lan y)) (>> s `(lan ry)) ry)))
    (>> s 'trk (>> 'shape 'rect 'x x 'y y 'rx ry 'ry rx 'a 1/2 'type 'obj)) ;trunk
    (>> s 'cro (>> 'shape 'tria 'x x 'y (+ y ry) 'rx (/ (* rx 7) 2) 'ry (* rx 3) 'a 1/2 'type 'obj)) ;crown
    (>> s 'crw (>> 'shape 'tria 'x x 'y (+ (>> s `(cro y)) (>> s `(cro rx))) 'rx (* rx 2) 'ry (/ (* rx 3) 2) 'a 1/2 'type 'obj)));crown2
  (let* ((rx (/ (* (>> s `(trk ry)) 9) 5)) (ry (/ (* rx 7) 10)) (x (>> s `(sun x))) (y (+ (>> s `(sea y)) (>> s `(sea ry)))))
    (>> s 'hul (>> 'shape 'rect 'x x 'y y 'rx rx 'ry ry 'a 0 'type 'obj)) ;hull
    (>> s 'bow (>> 'shape 'tria 'x (- x rx ry) 'y y 'rx (* (sqrt 2) ry) 'ry (* (sqrt 2) ry) 'a (/ 1 4) 'type 'obj))
    (>> s 'pop (>> 'shape 'tria 'x (+ x rx ry) 'y y 'rx (* (sqrt 2) ry) 'ry (* (sqrt 2) ry) 'a (/ 3 4) 'type 'obj));poop
    (>> s 'sal (>> 'shape 'tria 'x (- x (/ (* rx 4) 5)) 'y (+ y ry (/ (* ry 11) 5)) 'rx (* 3 ry) 'ry (/ (* ry 11) 5) 'a 0 'type 'obj)));sail
  (let* ((rx (>> s `(trk ry))) (x (>> s `(sal x))) (y (>> s `(sea ry))))
    (>> s 'fad (>> 'shape 'tria 'x x 'y y 'rx rx 'ry rx 'a 1 'type 'obj));fish-head
    (>> s 'fdy (>> 'shape 'tria 'x x 'y y 'rx (/ (* rx 3) 2) 'ry rx 'a 0 'type 'obj));fish-body
    (>> s 'fil (>> 'shape 'tria 'x (+ x (/ (* rx 3) 2) (/ (* rx 2) 3)) 'y y 'rx (/ (* rx 2) 3) 'ry (/ (* rx 4) 9) 'a 1 'type 'obj));fish-tail
    (>> s 'fye (>> 'shape 'elli 'x (- x (/ (* rx 2) 5)) 'y y 'rx (/ rx 5) 'ry (/ rx 5) 'a 0 'type 'obj)));fish-eye
  s)
;;(show-hash (get-scene3))
;;(show "{}" (length (hash-keys (get-scene3))))

(defun get-scene32 (&key (s (>>)))
  (>> s 'sce (>> 'shape 'rect 'ori '(0 0) 'len '(100 100) 'ra '(0 0) 'a 0 'type 'bg)) ;scene
  (>> s 'sky (>> 'shape 'rect 'ori '(0 100) 'len '(100 66) 'ra '(0 1) 'a 0 'type 'bg)) ;a:angle [-1, 1]-->[-pi, pi]
  (>> s 'sea (>> 'shape 'rect 'ori '(0 0) 'len '(100 34) 'ra '(0 0) 'a 0 'type 'bg))
  (>> s 'lan (>> 'shape 'rect 'ori '(70 19) 'len '(50 38) 'ra '(1/2 1/2) 'a 0 'type 'bg)) ;land
  ;;(>> s 'sun2 (>> 'shape 'elli 'ori '(20 85) 'len '(63/3 63/5) 'ra '(1/2 1/2) 'a 1/4 'type 'obj))
  (>> s 'sun (>> 'shape 'elli 'ori '(20 85) 'len '(63/5 63/5) 'ra '(1/2 1/2) 'a 0 'type 'obj))
  (>> s 'wal (>> 'shape 'rect 'ori '(125/2 139/3) 'len '(25 50/3) 'ra '(1/2 1/2) 'a 0 'type 'obj)) ;wall
  (>> s 'rof (>> 'shape 'tria 'ori '(125/2 164/3) 'len '(65/2 25/3) 'ra '(1/2 0) 'a 0 'type 'obj)) ;roof
  (>> s 'win (>> 'shape 'rect 'ori '(225/4 581/12) 'len '(25/3 25/3) 'ra '(1/2 1/2) 'a 0 'type 'obj)) ;window
  (>> s 'dor (>> 'shape 'rect 'ori '(275/4 134/3) 'len '(20/3 40/3) 'ra '(1/2 1/2) 'a 0 'type 'obj));door
  (>> s 'trk (>> 'shape 'rect 'ori '(965/11 493/11) 'len '(50/11 150/11) 'ra '(1/2 1/2) 'a 0 'type 'obj)) ;trunk
  (>> s 'cro (>> 'shape 'tria 'ori '(965/11 568/11) 'len '(150/11 175/22) 'ra '(1/2 0) 'a 0 'type 'obj)) ;crown
  (>> s 'crw (>> 'shape 'tria 'ori '(965/11 1311/22) 'len '(75/11 50/11) 'ra '(1/2 0) 'a 0 'type 'obj)) ;crown2
  (>> s 'hul (>> 'shape 'rect 'ori '(20 34) 'len '(90/11 63/11) 'ra '(1/2 1/2) 'a 0 'type 'obj)) ;hull
  (>> s 'bow (>> 'shape 'tria 'ori (list (- 20 90/22) (+ 34 63/22)) 'len '(63/11 63/11) 'ra '(1 1) 'a 0 'type 'obj))
  (>> s 'pop (>> 'shape 'tria 'ori (list (+ 20 90/22) (+ 34 63/22)) 'len '(63/11 63/11) 'ra '(0 1) 'a 0 'type 'obj));poop
  (>> s 'sal (>> 'shape 'tria 'ori '(184/11 2374/55) 'len '(189/22 63/5) 'ra '(0 1/2) 'a 0 'type 'obj));sail
  (>> s 'fad (>> 'shape 'tria 'ori '(184/11 17) 'len '(25/11 50/11) 'ra '(1 1/2) 'a 0 'type 'obj));fish-head
  (>> s 'fdy (>> 'shape 'tria 'ori '(184/11 17) 'len '(75/22 50/11) 'ra '(0 1/2) 'a 0 'type 'obj));fish-body
  (>> s 'fil (>> 'shape 'tria 'ori (list (+ 184/11 75/22 50/33) 17) 'len '(50/33 2) 'ra '(1 1/2) 'a 0 'type 'obj));fish-tail
  (>> s 'fye (>> 'shape 'elli 'ori '(174/11 17) 'len '(10/11 10/11) 'ra '(1/2 1/2) 'a 0 'type 'obj));fish-eye
  s)
;;(show-hash (get-scene32))

(defun draw-scene3 (&key (origin '(0 0)) (width 300) (height 300) (s (get-scene3))
		     (scale (/ (min width height) (+ (>> s `(sce x)) (>> s `(sce rx))))))
  (gl:color 0.7 0.7 0.7)  
  (dolist (k (hash-keys s))
    ;;(show "draw-scene3 {}" k)
    (let* ((x (+ (car origin) (* scale (>> s `(,k x)))))
	    (y (+ (second origin) (* scale (>> s `(,k y)))))
	    (rx (* scale (>> s `(,k rx))))
	    (ry (* scale (>> s `(,k ry))))
	    (obj (obj2polygon (>> 'shape (>> s `(,k shape)) 'x x 'y y 'rx rx 'ry ry 'a (>> s `(,k a))))))
      (gl-polygon :polygon obj :color '(1 1 1 1) :stroke-color '(0 0 0 1)))))

(defun draw-scene32 (&key (origin '(0 0)) (width 300) (height 300) (s (get-scene32))
		     (scale (/ (min width height) (car (>> s `(sce len))))))
  (gl:color 0.7 0.7 0.7)  
  (dolist (k (hash-keys s))
    (let* ((x (+ (car origin) (* scale (car (>> s `(,k ori))))))
	    (y (+ (second origin) (* scale (second (>> s `(,k ori))))))
	    (len-x (* scale (car (>> s `(,k len)))))
	    (len-y (* scale (second (>> s `(,k len)))))
	    (obj (obj2polygon2 (>> 'shape (>> s `(,k shape)) 'ori (list x y) 'len (list len-x len-y) 'ra (>> s `(,k ra)) 'a (>> s `(,k a))))))
      (gl-polygon :polygon obj :color '(1 1 1 1) :stroke-color '(0 0 0 1)))))

(defun draw-polygons (&key (origin '(0 0)) (width 300) (height 300) (polygons '(((0 0)(3 0)(3 3)(0 3)) ((2 0)(4 -2)(4 4)(1 4)(3 2)(3 1)))) (color '(1 1 1 0)) (stroke-color '(0 0 0 1)))
  (let* ((points (apply #'append polygons))
	  (xs (mapcar (lambda (p) (car p)) points))
	  (ys (mapcar (lambda (p) (second p)) points))
	  (min-x (apply #'min xs))
	  (min-y (apply #'min ys))
	  (len-x (- (apply #'max xs) min-x))
	  (len-y (- (apply #'max ys) min-y))
	  (scale (if (> (/ width height) (/ len-x len-y)) (/ height len-y) (/ width len-x)))
	  ;;(scale (/ (min width height) (max len-x len-y)))
	  )
    ;;(show "width {} height {} len-x {} len-y {} scale {}" width height len-x len-y scale)
    (dolist (poly polygons)
      (let ((scaled-poly (mapcar (lambda (p) (list (+ (car origin) (* scale (- (car p) min-x))) (+ (second origin) (* scale (- (second p) min-y))))) poly)))
	(gl-polygon :polygon scaled-poly :color color :stroke-color stroke-color)))))

(defun show-polygons (&key (width 600) (height 600) (polygons '(((0 0)(3 0)(3 3)(0 3)) ((2 0)(4 -2)(4 4)(1 4)(3 2)(3 1)))) (color '(1 1 1 0)) (stroke-color '(0 0 0 1)))
  (sdl:with-init ()
    (sdl:window width height :flags sdl:sdl-opengl :title-caption "visual" :icon-caption "visual")
    (setf (sdl:frame-rate) 0)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	(gl:clear-color 1 1 1 1)
	(gl:clear :color-buffer :depth-buffer)
	(let* ((origin (list 5 5)) (width (- width 10)) (height (- height 10)))  
	  (before-visual-2D :origin origin :width width :height height :color '(0 0 1 1) :border? nil)
	  ;;(draw-scene2 :origin origin :width width :height height :s s)
	  (draw-polygons :origin origin :width width :height height :polygons polygons :color color :stroke-color stroke-color))	  
	(gl:flush)
	(sdl:update-display)))))
;;(show-polygons)
;;(show-polygons :width 600 :height 200 :polygons '(((0 0)(3 0)(3 3)(0 3)) ((2 0)(3 -1)(4 -1)(20 4)(1 4)(3 2)(3 1))) :color '(1 1 1 0) :stroke-color '(0 0 0 1))

(defun draw-scene30 (&key (origin '(0 0)) (width 300) (height 300) (s (get-scene3))
		     (scale (/ (min width height) (+ (>> s `(scene x)) (>> s `(scene rx))))))
  (gl:color 0.7 0.7 0.7)  
  (dolist (k (hash-keys s))
    (let ((x (+ (car origin) (* scale (>> s `(,k x)))))
	   (y (+ (second origin) (* scale (>> s `(,k y)))))
	   (rx (* scale (>> s `(,k rx))))
	   (ry (* scale (>> s `(,k ry))))
	   (a (* 180 (>> s `(,k a)))))
      (cond
	((equal (>> s `(,k shape)) 'elli)
	  (gl-circle :center (list x y) :r rx :color '(1 1 0 0) :stroke-color '(0 0 0 1)))
	((equal (>> s `(,k shape)) 'tria)
	  (let ((triangle (list (list (- x rx) y) (list (+ x rx) y) (list x (+ y ry)))))
	    (gl:translate x y 0)
	    (gl:rotate a 0 0 1)
	    (gl:translate (- 0 x) (- 0 y) 0)
	    (gl-triangle :triangle triangle)
	    (gl:translate x y 0)
	    (gl:rotate (- 0 a) 0 0 1)
	    (gl:translate (- 0 x) (- 0 y) 0)
	    ))
	((equal (>> s `(,k shape)) 'rect)
	  (let ((min-x (- x rx))
		 (min-y (- y ry))
		 (max-x (+ x rx))
		 (max-y (+ y ry)))
	    (gl-rectangle :min-p (list min-x min-y) :max-p (list max-x max-y) :color '(1 1 1 1) :stroke-color '(0 0 0 1))))))))

(defun draw-scene2 (&key (origin '(0 0)) (width 300) (height 300) (s (get-scene2)))
  (gl:color 0.7 0.7 0.7)  
  (dolist (e (hash-keys s))
    (cond
      ((equal (car (>> s e)) 'circle)
	(gl-circle :center (list  (+ (car origin) (* width (car (second (>> s e)))))  (+ (second origin) (* height (second (second (>> s e)))))) :r (* (min width height) (third (>> s e))) :color '(1 1 0 0) :stroke-color '(0 0 0 1)))
      ((or (equal (car (>> s e)) 'polygon) (equal (car (>> s e)) 'triangle))
	(gl:with-primitive :line-loop
	  (dotimes (i (length (cdr (>> s e))))
	    (gl:vertex (+ (car origin) (* width (car (nth i (cdr (>> s e)))))) (+ (second origin) (* height (second (nth i (cdr (>> s e))))))))))
      ((equal (car (>> s e)) 'rectangle)
	(let ((min-x (+ (car origin) (* width (car (second (>> s e))))))
	       (min-y (+ (second origin) (* height (second (second (>> s e))))))
	       (max-x (+ (car origin) (* width (car (third (>> s e))))))
	       (max-y (+ (second origin) (* height (second (third (>> s e)))))))
	  (gl-rectangle :min-p (list min-x min-y) :max-p (list max-x max-y) :color '(1 1 0 0) :stroke-color '(0 0 0 1)))))))

(defun main-loop-house (&key (width 600) (height 600) (s (get-scene2)))
  (sdl:with-init ()
    (sdl:window width height :flags sdl:sdl-opengl :title-caption "visual" :icon-caption "visual")
    (setf (sdl:frame-rate) 0)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	(gl:clear-color 1 1 1 1)
	(gl:clear :color-buffer :depth-buffer)
	(let* ((origin (list 5 5)) (width (- width (* (car origin) 2))) (height (- height (* (second origin) 2))))  
	  (before-visual-2D :origin origin :width width :height height :color '(0 0 1 1) :border? t)
	  ;;(draw-scene2 :origin origin :width width :height height :s s)
	  (draw-scene3 :origin origin :width width :height height :s (get-scene3))
	  ;;(draw-scene32 :origin origin :width width :height height :s (get-scene32))
	  )	  
	(gl:flush)
	(sdl:update-display)))))
;;(main-loop-house :width 600 :height 600) 
;;(let* ((s (get-scene3)) (polygons (mapcar (lambda (obj) (obj2polygon obj))(hash-values s)))) (show-polygons :width 600 :height 600 :polygons polygons :color '(1 1 1 1) :stroke-color '(0 0 0 1)))
;;(let* ((s (get-scene32)) (polygons (mapcar (lambda (obj) (obj2polygon2 obj))(hash-values s)))) (show-polygons :width 600 :height 600 :polygons polygons :color '(1 1 1 1) :stroke-color '(0 0 0 1)))

(defmethod draw-curve (&key (origin '(0 0)) (width 300) (height 300) (timex 2))
  ;;(gl:color 0.7 0.7 0.7)
  ;;(gl:rect (+ (car origin) 10) (+ (second origin) 10) (- (+ (car origin) width) 10) (- (+ (second origin) height) 10))
  ;;(gl:line-width 2)
  (gl:color 0 0 0)
  (gl:with-primitives :line-loop
    (gl:vertex (car origin) (second origin))
    (gl:vertex (+ (car origin) (- width 1)) (second origin))
    (gl:vertex (+ (car origin) (- width 1)) (+ (second origin) height))
    (gl:vertex (car origin) (+ (second origin) height))
    )
  ;;(gl:line-width 1)
  (gl:color 1 0 0)
  (let* ((origin (mapcar (lambda (x) (+ 5 x)) origin))
	  (width (- width 10))
	  (height (- height 10))
	  (data (mapcar (lambda (x) (expt (- 100 x) 2)) (range timex)))
	  (max-data (apply #'max data))
	  (scale (/ height max-data))
	  (scale-x (/ width timex))
	  )
    (when (< scale 1) (setf data (mapcar (lambda (x) (* x scale)) data)))
    (gl:with-primitives :line-strip
      (dotimes (i timex)
	(if (> timex width)
	  (gl:vertex (+ (car origin) (* i scale-x)) (+ (second origin) (nth i data)))
	  (gl:vertex (+ (car origin) i) (+ (second origin) (nth i data))))))))

(defun plot-gl (&key (origin '(0 0)) (width 300) (height 300) (data '(1 2 3)) (max-data nil) (color '(0 0 1)) (border? nil))
  (when border? (draw-border :origin origin :width width :height height))
  ;;(gl:line-width 1)
  (apply #'gl:color color)
  (when (null max-data) (max-data (apply #'max data)))
  (let* ((origin (mapcar (lambda (x) (+ 5 x)) origin))
	  (width (- width 10))
	  (height (- height 10))
	  (scale (/ height max-data))
	  (scale-x (/ width (length data))))
    (setf data (mapcar (lambda (x) (* x scale)) data))
    (gl:with-primitives :line-strip
      (dotimes (i (length data))
	(if (> (length data) width)
	  (gl:vertex (+ (car origin) (* i scale-x)) (+ (second origin) (nth i data)))
	  (gl:vertex (+ (car origin) i) (+ (second origin) (nth i data))))))))

(defun visual-samples (samples &key (origin '(0 0)) (width 300) (height 300) (color '(1 0 0 1)) (border? nil));g is a hash table
  (when border? (draw-border :origin origin :width width :height height))
  (apply #'gl:color color)
  (gl:with-primitives :points
    (dolist (sample samples)
      ;;(show "sample {}" sample)
      (gl:vertex
	;;(car sample) (second sample)
	(+ (car origin) (* width (car sample)))
	(+ (second origin) (* height (second sample)))
	))))

(defun draw-border (&key (origin '(0 0)) (width 300) (height 300) (color '(0 0 0)))
  (apply #'gl:color color)
  (gl:with-primitives :line-loop
    (gl:vertex (car origin) (second origin))
    (gl:vertex (+ (car origin) (- width 1)) (second origin))
    (gl:vertex (+ (car origin) (- width 1)) (+ (second origin) (- height 1)))
    (gl:vertex (car origin) (+ (second origin) (- height 1)))))

(defun visual-gng (g &key (origin '(0 0)) (width 300) (height 300) (color '(1 0 0 1)) (border? nil));g is a hash table
  (when border? (draw-border :origin origin :width width :height height))
  (when (> (hash-table-count (>> g 'nodes)) 0)
    (dolist (k (hash-keys (>> g 'nodes)))
      (let* ((pos (>> g `(nodes ,k pos)))
	      (posx (+ (car origin) (* width (car pos))))
	      (posy (+ (second origin) (* height (second pos)))))
	(gl-circle :center (list posx posy) :r 2 :color '(1 1 0 0) :stroke-color color)))
    (when (> (hash-table-count (>> g 'edges)) 0)
      (dolist (e (hash-keys (>> g 'edges)))
	(let* ((n0 (car (>> g `(edges ,e nodes))))
		(n1 (second (>> g `(edges ,e nodes))))
		(pos0 (>> g `(nodes ,n0 pos)))
		(pos1 (>> g `(nodes ,n1 pos)))
		(pos0x (+ (car origin) (* width (car pos0))))
		(pos0y (+ (second origin) (* height (second pos0))))
		(pos1x (+ (car origin) (* width (car pos1))))
		(pos1y (+ (second origin) (* height (second pos1)))))
	  (gl:with-primitives :lines
	    (gl:vertex pos0x pos0y)
	    (gl:vertex pos1x pos1y))
	  )))))

;;;; rotate cube ------------------------------------------------------------
;;http://blog.lowsnr.net/2013/04/14/using-opengl-with-common-lisp-and-macos-x/

(defconstant +window-width+  600)
(defconstant +window-height+ 600)
 
(defun cube-vertices0 ()
  #(
     #(0 0 0) ;;0 
     #(0 1 0) ;;1
     #(1 1 0) ;;2
     #(1 0 0) ;;3
     #(0 0 1) ;;4
     #(0 1 1) ;;5
     #(1 1 1) ;;6
     #(1 0 1) ;;7
     ))

(defun cube-vertices (&key (origin '(0 0 0)) (rx 0.5) (ry 1) (rz rx))
  (let ((cx (car origin))
	 (cy (second origin))
	 (cz (third origin)))
    (list
      (list (- cx rx) (- cy ry) (- cz rz)) ;;0 
      (list (- cx rx) (+ cy ry) (- cz rz)) ;;1
      (list (+ cx rx) (+ cy ry) (- cz rz)) ;;2
      (list (+ cx rx) (- cy ry) (- cz rz)) ;;3
      (list (- cx rx) (- cy ry) (+ cz rz)) ;;4
      (list (- cx rx) (+ cy ry) (+ cz rz)) ;;5
      (list (+ cx rx) (+ cy ry) (+ cz rz)) ;;6
      (list (+ cx rx) (- cy ry) (+ cz rz)) ;;7
      )))

(defun cube-faces ()
  '((#(4 7 6 5) #(0 0 1))
    (#(5 6 2 1) #(0 1 0))
    (#(1 2 3 0) #(0 0 -1))
    (#(0 3 7 4) #(0 -1 0))
    (#(4 5 1 0) #(-1 0 0))
    (#(3 2 6 7) #(1 0 0))))

(defun tetrahedron-vertices ()
  #(
     #(0 0 0) ;;0 
     #(0 1 0) ;;1
     #(1 0 0) ;;2
     #(0 0 1) ;;3
     ))

(defun tetrahedron-faces ()
  '((#(0 2 3) #(0 -1 0))
     (#(2 1 3) #(1 1 1))
     (#(0 3 1) #(-1 0 0))
     (#(0 1 2) #(0 0 -1))
    ))

(defun draw-box (verts faces)
  (labels ((set-normal (n)
             (gl:normal (aref n 0) (aref n 1) (aref n 2)))
	    (set-vertex (index)
	      (let ((v (nth index verts)))
		(gl:vertex (car v) (second v) (third v))))
	    (draw-face (vertex-indices normal)
	      (set-normal normal)
	      (gl:begin :quads)
	      (map 'nil #'set-vertex vertex-indices)
	      (gl:end)))
    (map 'nil #'(lambda (x) (draw-face (first x) (second x))) faces)))

(defun draw-figure2 (verts faces)
  (labels ((set-normal (n)
             (gl:normal (aref n 0) (aref n 1) (aref n 2)))
	    (set-vertex (index)
	      (let ((v (aref verts index)))
		(gl:vertex (aref v 0) (aref v 1) (aref v 2))))
	    (draw-face (vertex-indices normal)
	      (set-normal normal)
	      (gl:begin :triangles)
	      (map 'nil #'set-vertex vertex-indices)
	      (gl:end)))
    (map 'nil #'(lambda (x) (draw-face (first x) (second x))) faces)))

(defun draw-frame (&key (rotx 0) (roty 0) (rotz 0))
  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (gl:translate 0.5 0.5 0.5)
  (gl:rotate rotx 1 0 0)
  (gl:rotate roty 0 1 0)
  (gl:rotate rotz 0 0 1)
  (gl:translate -0.5 -0.5 -0.5)
  (draw-box (cube-vertices) (cube-faces))
  ;;(draw-figure2 (tetrahedron-vertices) (tetrahedron-faces))
  (gl:pop-matrix))

(defun visual3d (&key (width 600) (height 300) (rotx 0) (roty 0) (rotz 0))
  (sdl:with-init ()
    (sdl:window width height :flags sdl:sdl-opengl :title-caption "visual" :icon-caption "visual")
    (setf (sdl:frame-rate) 0)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	(gl:clear-color 1 1 1 1)
	(gl:clear :color-buffer :depth-buffer)
	(let* ((origin (list 5 5)) (width (- (min width height) 10)) (height width))
	  (before-visual-3D :origin origin :width width :height height :border? t)
	  ;;(glu:look-at -2 2 3 0 0 0 0 1 0)
	  ;;(setf rotx (mod (+ rotx 2.5) 360.0))
          ;;(setf roty (mod (+ roty 0.7) 360.0))
          ;;(setf rotz (mod (+ rotz 4.4) 360.0))
	  (draw-frame :rotx rotx :roty roty :rotz rotz))	  
	(gl:flush)
	(sdl:update-display)))))
;;(visual3d)

(defun draw-figure (verts faces)
  (labels ((set-normal (n)
             (gl:normal (aref n 0) (aref n 1) (aref n 2)))
	    (set-vertex (index)
	      (let ((v (aref verts index)))
		(gl:vertex (aref v 0) (aref v 1) (aref v 2))))
	    (draw-face (vertex-indices normal)
	      (set-normal normal)
	      (gl:begin :quads)
	      (map 'nil #'set-vertex vertex-indices)
	      (gl:end)))
    (map 'nil #'(lambda (x) (draw-face (first x) (second x))) faces)))

(defun start ()
  (let ((rotx 0)
        (roty 0)
        (rotz 0))
    (sdl:with-init ()
      (sdl:window +window-width+ +window-height+ 
                  :opengl t
                  :opengl-attributes '((:sdl-gl-depth-size   16)
                                       (:sdl-gl-doublebuffer 1)))
      (setf (sdl:frame-rate) 0)
 
      (gl:viewport 0 0 +window-width+ +window-height+)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 50 (/ +window-height+ +window-width+) 1.0 10.0)
      (glu:look-at -2 2 4 
                    0.5 0.5 0.5 
                    0 1 0)
 
      (gl:matrix-mode :modelview)
      (gl:load-identity)
 
      (gl:clear-color 0 0 0 0)
      (gl:shade-model :flat)
      (gl:cull-face :back)
      (gl:polygon-mode :front :fill)
      (gl:draw-buffer :back)
      (gl:material :front :ambient-and-diffuse #(0.7 0.7 0.7 0.4))
      (gl:light :light0 :position #(0 0 1 0))
      (gl:light :light0 :diffuse #(1 0 0 0))
      (gl:light :light1 :position #(-1 2 -0.5 0))
      (gl:light :light1 :diffuse #(0 1 0 0))
      (gl:enable :cull-face :depth-test
                 :lighting :light0 :light1)
 
      (gl:clear :color-buffer :depth-buffer)
      (draw-frame :rotx rotx :roty roty :rotz rotz)
      (sdl:update-display)
 
      (sdl:with-events ()
        (:quit-event () t)
        (:video-expose-event () (sdl:update-display))
        (:idle
          (setq rotx (mod (+ rotx 2.5) 360.0))
          (setq roty (mod (+ roty 0.7) 360.0))
          (setq rotz (mod (+ rotz 4.4) 360.0))
          (gl:clear :color-buffer :depth-buffer)
          (draw-frame :rotx rotx :roty roty :rotz rotz)
          (sdl:update-display))))))
;;(start)

;;(defun test-cgn ()
;;  (cgn:start-gnuplot)
;;  (cgn:plot-function "cos(x)")
;;  (cgn:plot-function "x**2+y**2")
;;  (cgn:plot-points '(1 2 3 4) '(2 5 1 5))
;;  (cgn:close-gnuplot)
;;  )
;;(test-cgn)

(defun test-vgplot ()
  (progn
    (vgplot:plot #(1 2 3) #(0 -2 -17) ";silly example;")
    (vgplot:title "Simple curve")
    (vgplot:text 1.2 -14 "Plot vectors with legend and add a title"))
  ;;splot [-2:2][-2:2] x**2+y**2
  (vgplot:plot '(1 2 3) '(1 2 3) ";silly example0;" '(1 2 3) '(0 -2 -17) ";silly example;")
  (progn
    (defvar x)
    (defvar y)
    (setf x (vgplot:range 0 (* 2 pi) 0.01))
    (setf y (map 'vector #'sin x))
    (vgplot:plot x y "y = sin(x)")
    (vgplot:xlabel "[rad]")
    (vgplot:ylabel "magnitude")
    (vgplot:text 0.2 -0.6
      "Use function range to create vectors and add labels to axes"))

  (and "text-show-labels shows the active labels and their tags"
    (vgplot:text-show-label))
  ;;label 1 "Plot vectors with legend and add a title" at (1.2, -14, 0) left not rotated back  nopoint
  ;;label 2 "Plot vectors with legend and add a title" at (1.2, -14, 0) left not rotated back  nopoint
  ;;label 3 "Use function range to create vectors and add labels to axes" at (0.2, -0.6, 0) left not rotated back  nopoint
  ;;label 4 "Use function range to create vectors and add labels to axes" at (0.2, -0.6, 0) left not rotated back  nopoint

  (and "Usefull to delete or change labels" (vgplot:text-delete 1 2 3))
  (vgplot:text 0.5 -0.5 "You can use a different size" :fontsize 14)
  (progn
    (vgplot:text 0.5 -0.5 "You can change a present label" :tag 1 :rotation 60
      :font "Times" :fontsize 12)
    (and "font definitions for text change also the font of the legend,
the following is needed to change the keys back")
    (vgplot:format-plot t "set key font \",10\"")
    (vgplot:replot))
  (progn
    (vgplot:text-delete 1)
    (vgplot:text 3 0.5 "axis can change the range of the x axis")
    (vgplot:axis (list (/ pi 2) 5)))
  (progn
    (vgplot:text -0.5 -0.5 "axis can also change the range of both x and y axis"
      :tag 1)
    (vgplot:axis (list -1 pi -1.2 1.2)))
  (progn
    (vgplot:text 0.5 -0.5 "t means autoscale and nil unchanged corresponding axis"
      :tag 1)
    (vgplot:axis '(t nil)))
  (progn
    (vgplot:text 0.5 -0.5 "another example of the use of axis" :tag 1)
    (vgplot:axis '(nil nil -1.5 t)))
  (progn (vgplot:text 0.5 -0.5 "Remove the grid" :tag 1) (vgplot:grid nil))
  (and "close-plot closes the actual plot" (vgplot:close-plot))
  (progn
    (defvar z)
    (setf z (map 'vector #'cos x))
    (vgplot:plot x y "b;y = sin(x);" x z
      "g;y = cos(x);")
    (vgplot:title "Some Other Graphs"))

  (progn
    (vgplot:legend :outside :boxon :southeast :right)
    (vgplot:title "Use legend to manipulate the legend (aka keys)"))
  ;;title is "Some Other Graphs", offset at ((character units) 0, 0, 0)

  (progn
    (vgplot:legend :at 5 0.4)
    (vgplot:title "Place the legend directly at position x y"))
  (progn
    (vgplot:new-plot)
    (setf y
      (map 'vector #'(lambda (a) (sin (* 2 a))) x))
    (vgplot:plot x y "+k;y = cos(2x) (new-plot);")
    (vgplot:text 0.5 -0.5 "new-plot adds a new plot window"))
  (vgplot:text-show-label)
  ;;label 1 "new-plot adds a new plot window" at (0.5, -0.5, 0) left not rotated back  nopoint

  (progn
    (vgplot:plot x y "og;y = cos(2x) (new-plot);")
    (vgplot:text 0.5 0.5 "Different line style" :tag 1))
  (progn
    (and "(format-plot) allows direct commands to the running gnuplot process")
    (vgplot:format-plot t "set size square 0.5,0.5~%")
    (vgplot:replot))
  (vgplot:close-all-plots)
  (let ((x #(1 2 3 4)))
    (vgplot:subplot 3 2 0)
    (vgplot:plot x #(0.5 2 -3 4) x #(-1 2 3 4))
    (vgplot:title "Use of multiplots")
    (vgplot:subplot 3 2 1)
    (vgplot:plot x #(0.2 0.4 3 0.4) x #(-1 -2 3 4))
    (vgplot:title "")
    (vgplot:subplot 3 2 2)
    (vgplot:plot x #(-1 2 3 4) x #(-1 -2 -3 4))
    (vgplot:subplot 3 2 3)
    (vgplot:plot x #(1 -1.8 -2.8 4) x #(-1 -2 -3 -4))
    (vgplot:subplot 3 2 4)
    (vgplot:plot x #(1 2 -3 4) x #(1 -2 3 4))
    ;;(dotimes (i 100) (vgplot:plot x (list 1 (random 5) -3 4) x #(1 -2 3 4)))
    (vgplot:subplot 3 2 5)
    (vgplot:plot x #(1 -2 3 4) x #(1 -2 -3 4)))
  ;;title is "Use of multiplots", offset at ((character units) 0, 0, 0)
  ;;title is "", offset at ((character units) 0, 0, 0)
  (vgplot:close-all-plots)
  (let ((x '(0)) (y '(1)))
    (dotimes (i 100)      
      (setf y (append y (list (+ i (- (random 10) 5)))))
      (setf x (range (length y))) 
      (vgplot:subplot 2 2 0)
      (vgplot:plot x y)
      (vgplot:title "Linear axes (plot)")
      (vgplot:subplot 2 2 1)
      (vgplot:semilogx x y)
      (vgplot:title "Log x axis (semilogx)")
      (vgplot:subplot 2 2 2)
      (vgplot:semilogy x y)
      (vgplot:title "Log y axis (semilogy)")
      (vgplot:subplot 2 2 3)
      (vgplot:loglog x y)
      (vgplot:title "Log x and y axis (loglog)")))
  (vgplot:close-plot)
  (progn
    (setf y #(0 4 6.5 6.8 6.5 6.2 6.1 6.05 6.0 6.0))
    (vgplot:stairs y)
    (vgplot:title "Example of a stairstep plot"))
  (progn
    (setf x (vgplot:range (length y)))
    (vgplot:stairs x y "line 1" '(0 1 10) '(0 6 6) "k;line 2;"))
  (flet ((e-fun (x)
	   (- 1 (exp (- x)))))
    (let* ((x (vgplot:range 0 6 0.1))
	    (y (map 'vector #'e-fun x))
	    (xd (vgplot:range 7))
	    (yd (map 'vector #'e-fun xd))
	    (sampled (vgplot:stairs-no-plot xd yd)))
      (vgplot:plot x y "b;continuous;" (first sampled)
	(second sampled) "sampled"))
    (vgplot:legend :southeast)
    (vgplot:title
      "Example of a mixture of continuous and discrete characteristics"))
  (progn
    (vgplot:bar :y '(((0.9 0.8 0.3)) ((0.6 0.7 0.1))))
    (vgplot:title "Very simple example of a bar plot"))
  (progn
    (vgplot:bar :x #(2011 2013 2014) :y
      '((#(0.9 0.8 0.3) :label "Values 1")
	 (#(0.6 0.7 0.1) :label "Values 2")))
    (vgplot:title "Simple example of a bar plot"))
  (progn
    (vgplot:bar :x #("January 2015" "Mars 2015" "June 2015") :y
      '((#(0.8 0.9 -0.3) :color "blue" :label "Values 1")
	 (#(0.7 0.65 0.5) :color "red" :label "Values 2")
	 (#(0.75 0.4 0.1) :color "cyan" :label "Values 3")
	 (#(0.6 0.5 0.2) :color "green" :label "Values 4"))
      :width 0.5)
    (vgplot:axis '(t t -0.5 1))
    (vgplot:xlabel "Year of the event")
    (vgplot:ylabel "Result")
    (vgplot:grid nil)
    (vgplot:title "Another example of a bar plot"))
  (progn
    (vgplot:bar :x #("January 2015" "Mars 2015" "June 2015") :y
      '((#(0.8 0.9 -0.3) :color "blue" :label "Values 1")
	 (#(0.7 0.65 0.5) :color "red" :label "Values 2")
	 (#(0.75 0.4 0.1) :color "cyan" :label "Values 3")
	 (#(0.6 0.5 0.2) :color "green" :label "Values 4"))
      :width 0.5 :gap 4)
    (vgplot:axis '(t t -0.5 1))
    (vgplot:xlabel "Year of the event")
    (vgplot:ylabel "Result")
    (vgplot:grid nil)
    (vgplot:title "Use of the gap parameter in a grouped bar plot (gap = 4.0)"))
  (progn
    (vgplot:bar :x #("January 2015" "Mars 2015" "June 2015") :y
      '((#(0.8 0.9 -0.3) :color "blue" :label "Values 1")
	 (#(0.7 0.65 0.5) :color "red" :label "Values 2")
	 (#(0.75 0.4 0.1) :color "cyan" :label "Values 3")
	 (#(0.6 0.5 0.2) :color "green" :label "Values 4"))
      :width 1.2)
    (vgplot:axis '(t t -0.5 1))
    (vgplot:xlabel "Year of the event")
    (vgplot:ylabel "Result")
    (vgplot:grid nil)
    (vgplot:title
      "Use of the width parameter in a grouped bar plot (width = 1.2)"))
  (progn
    (vgplot:bar :x #("John" "Paul" "Mary") :y
      '((#(0.8 0.9 0.3) :color "blue" :label "Values 1")
	 (#(0.7 0.65 0.5) :color "red" :label "Values 2")
	 (#(0.75 0.4 0.1) :color "cyan" :label "Values 3")
	 (#(0.6 0.5 0.2) :color "green" :label "Values 4"))
      :width 0.5 :style "stacked")
    (vgplot:axis '(t t 0 3))
    (vgplot:xlabel "Student")
    (vgplot:ylabel "Result")
    (vgplot:title "Example of a stacked bar plot"))
  (vgplot:close-plot)
  (or "The following works if you copy data.txt and data.csv
from vgplot's source directory to your directory")
  (when (cl-fad:file-exists-p "data.txt") (vgplot:plot-file "data.txt"))
  (when (cl-fad:file-exists-p "data.csv") (vgplot:plot-file "data.csv"))
  (when (cl-fad:file-exists-p "data.csv")
    (vgplot:plot (first (vgplot:load-data-file "data.csv")))
    (vgplot:text 2 -1 "load-data-file returns data from a csv file"))
  (vgplot:close-all-plots)
  )

