;;;; gng.lisp ;growing neural gas

(in-package #:util)

;; do not use defparameter,defconstant,defvar, and try to copy-table, they will affect each other
(defun make-gng
  (&key
    (input-dim 2)
    (max-nodes 2147483647)
    ) 
  (>>
    'input-dim input-dim
    'max-nodes max-nodes
    'eps-b 0.2 ;;the rate of moving closest node to sample
    'eps-n 0.006 ;;the rate of moving neighbors of closest node to sample
    'max-age 50
    'lambd 100
    'alpha 0.5
    'd 0.995
    'time-len 0
    'nodes (>>)
    'edges (>>)))

(defun add-gng-node (&key (gng (make-gng)) (pos '(0 0)) (cum-error 0.0))
  (let ((key (if (= 0 (hash-table-count (>> gng 'nodes))) 0 (+ 1 (reduce #'max (hash-keys (>> gng 'nodes)))))))
    (>> gng `(nodes ,key) (>> 'pos pos 'cum-error cum-error))
    key))

(defun add-gng-edge (&key (gng (make-gng)) (nodes '(0 1)) (age 0))
  (let ((key (if (= 0 (hash-table-count (>> gng 'edges))) 0 (+ 1 (reduce #'max (hash-keys (>> gng 'edges)))))))
    (>> gng `(edges ,key) (>> 'nodes nodes 'age age))
    key))

(defun get-nearest-gng-nodes (&key (gng (make-gng)) (vector '(0 0)))
  ;;Return the two nodes in the graph that are nearest to vector and their squared distances. return (list (dist0 node0) (dist1 node1))
  (let ((distances (mapcar #'(lambda (x) (list (squared-distance :v0 vector :v1 (>> gng `(nodes ,x pos))) x)) (hash-keys (>> gng 'nodes)))))
    (setf distances (sort (copy-seq distances) #'list<))
    (list (car distances) (second distances))))

(defun move-gng-node (&key (gng (make-gng)) (node 0) (vector '(0 0)) (eps (>> gng 'eps-b)))
  (loop for i in (range (length vector)) do
    (let ((pos-i (nth i (>> gng `(nodes ,node pos)))))
      (setf (nth i (>> gng `(nodes ,node pos))) (+ pos-i (* eps (- (nth i vector) pos-i)))))))

(defun remove-old-gng-edges (&key (gng (make-gng)) (edges (hash-keys (>> gng 'edges))))
  ;;Remove all edges older than the maximal age
  (loop for k in edges do
    (when (> (>> gng `(edges ,k age)) (>> gng 'max-age))
      (let ((nodes-now (>> gng `(edges ,k nodes))))
	(remhash k (>> gng 'edges))
	(loop for node in nodes-now do
	  (when (= 0 (node-degree gng :node node))
	    (remhash node (>> gng 'nodes))))))))

(defun insert-new-gng-node (&key (gng (make-gng)))
  ;;Insert a new node in the graph where it is more necessary i.e. where the error is the largest
  (let ((errors (mapcar #'(lambda (x) (list (>> gng `(nodes ,x cum-error)) x)) (hash-keys (>> gng 'nodes))))
	 (q-node nil)
	 (neighbor-nodes '())
	 (neighbor-errors '())
	 (f-node nil)
	 )
    (setf errors (sort (copy-seq errors) #'list>)) 
    (setf q-node (cadar errors)) 
    (setf neighbor-nodes (neighbors gng :node q-node)) 
    (setf neighbor-errors (mapcar #'(lambda (x) (list (>> gng `(nodes ,x cum-error)) x)) neighbor-nodes)) 
    (setf neighbor-errors (sort (copy-seq neighbor-errors) #'list>)) 
    (setf f-node (cadar neighbor-errors)) 
    ;;new node, halfway between the worst node and the worst of its neighbors
    (let ((new-node (add-gng-node :gng gng :pos (mapcar #'(lambda (x y) (* 0.5 (+ x y))) (>> gng `(nodes ,q-node pos)) (>> gng `(nodes ,f-node pos)))))) 
      (remhash (car (get-edges gng :nodes (list q-node f-node))) (>> gng 'edges))
      (add-gng-edge :gng gng :nodes (list q-node new-node))
      (add-gng-edge :gng gng :nodes (list f-node new-node))
      (let ((q-error (>> gng `(nodes ,q-node cum-error))))
	(>> gng `(nodes ,q-node cum-error) (* q-error (>> gng 'alpha))))
      (let ((f-error (>> gng `(nodes ,f-node cum-error))))
	(>> gng `(nodes ,f-node cum-error) (* f-error (>> gng 'alpha))))
      (>> gng `(nodes ,new-node cum-error) (* 0.5 (+ (>> gng `(nodes ,q-node cum-error)) (>> gng `(nodes ,f-node cum-error))))))))

(defun train-gng (&key (gng (make-gng)) (input '((0 0))) (init-nodes 2))
  (when (= 0 (hash-table-count (>> gng 'nodes))) ;if missing, generate two initial nodes at random
    (loop for i in (range init-nodes) do
      ;;(add-gng-node :gng gng :pos (mapcar #'(lambda (x) (normal-random :mean 0.5 :deviation 0.2)) (range (input-dim gng))))
      (add-gng-node :gng gng :pos (mapcar (lambda (x) (random-uniform (list (- x 0.01) (+ x 0.01)))) (random-choice input)))))
  (dolist (vector input)
    (let ((time-len (>> gng 'time-len))) (>> gng 'time-len (+ 1 time-len))) ;; incf time-len
    (let* ((nearest-nodes (get-nearest-gng-nodes :gng gng :vector vector)) ;(list (dist0 node0) (dist1 node1))
	    (node0 (cadr (first nearest-nodes)))
	    (dist0 (car (first nearest-nodes)))
	    (edges0 (get-emanating-edges gng :node node0))
	    (node1 (cadr (second nearest-nodes))))
      (dolist (e edges0) ;increase age of the emanating edges of the closest node0
	(let ((age-now (>> gng `(edges ,e age))))
	  (>> gng `(edges ,e age) (+ 1 age-now))))
      (let ((c-error (>> gng `(nodes ,node0 cum-error)))) ;update error of the closest node0
	(>> gng `(nodes ,node0 cum-error) (+ c-error (sqrt dist0))))
      ;;move nearest node and neighbours
      (move-gng-node :gng gng :node node0 :vector vector :eps (>> gng 'eps-b))
      (let ((neighbor-nodes (neighbors gng :node node0)))
	(dolist (n neighbor-nodes)
	  (move-gng-node :gng gng :node n :vector vector :eps (>> gng 'eps-n)))
	;;update the edge connecting node0 and node1
	(if (member node1 neighbor-nodes :test #'equal)
	  (let ((edge01 (car (get-edges gng :nodes (list node0 node1)))))
	    (>> gng `(edges ,edge01 age) 0))
	  (add-gng-edge :gng gng :nodes (list node0 node1))))
      ;; remove old edges
      (remove-old-gng-edges :gng gng :edges edges0)
      ;; add a new node each lambd steps
      (when (and (= 0 (mod (>> gng 'time-len) (>> gng 'lambd))) (< (hash-table-count (>> gng 'nodes)) (>> gng 'max-nodes)))
	(insert-new-gng-node :gng gng))
      ;; decrease the errors of all nodes
      (loop for node in (hash-keys (>> gng 'nodes)) do
	(let ((c-error (>> gng `(nodes ,node cum-error))))
	  (>> gng `(nodes ,node cum-error) (* c-error (>> gng 'd))))))))

(defun circumference-distr (&key (center '(0.0 0.0)) (radius 1.0) (n 2000))
  (let* ((phi (mapcar #'(lambda (x) (random (* 2 pi))) (range n))))
    (mapcar #'(lambda (x) (list (+ (* radius (cos x)) (car center)) (+ (* radius (sin x)) (second center)))) phi)))

(defun circle-distr (&key (center '(0.0 0.0)) (radius 1.0) (n 2000))
  (let* ((phi (mapcar #'(lambda (x) (random (* 2 pi))) (range n)))
	  (sqrt-r (mapcar #'(lambda (x) (sqrt (random (* radius radius)))) (range n))))
    (mapcar #'(lambda (x) (list (+ (* (nth x sqrt-r) (cos (nth x phi))) (car center)) (+ (* (nth x sqrt-r) (sin (nth x phi))) (second center)))) (range n))))

(defun rectangle-distr (&key (center '(0.0 0.0)) (w 2.0) (h 1.0) (n 2000))
  (mapcar #'(lambda (x) (list (+ (random-uniform (list (/ w -2) (/ w 2))) (car center))
			  (+ (random-uniform (list (/ h -2) (/ h 2))) (second center)))) (range n)))

(defun test-gng ()
  (let ((g (make-gng :max-nodes 10)))    
    (show "g: {}" g)
    (let ((node (add-gng-node :gng g)))
      (format t "node-index: ~a~%" node)) 
    (add-gng-node :gng g :pos '(2 3))
    (add-gng-node :gng g :pos '(4 5))
    (add-gng-node :gng g :pos (mapcar #'(lambda (x y) (* 0.5 (+ x y))) (>> g `(nodes 1 pos)) (>> g `(nodes 2 pos))))
    (show "(>> g 'nodes) {}" (hash2lst (>> g 'nodes)))
    (add-gng-edge :gng g)
    (add-gng-edge :gng g :nodes '(1 2) :age 100)
    (add-gng-edge :gng g :nodes '(3 0) :age 0)
    (let ((pair-nodes '(2 1)))
      (format t "the edge of pair-nodes ~a is ~a~%" pair-nodes (car (get-edges g :nodes pair-nodes)))
      )
    (format t "before inserting new node~%")
    (show "(>> g 'edges) {}" (hash2lst (>> g 'edges)))
    (insert-new-gng-node :gng g)
    (format t "after inserting new node~%")
    (show "(>> g 'edges) {}" (hash2lst (>> g 'edges)))
    (let ((node 1)) (format t "neighbors of node ~a: ~a~%" node (neighbors g :node node)))
    (format t "~a~%" (get-nearest-gng-nodes :gng g :vector '(3 5)))
    (move-gng-node :gng g :node 2 :vector '(3 5))
    (show "(>> g 'nodes) {}" (hash2lst (>> g 'nodes)))
    (node-degree g :node 2)
    (remove-old-gng-edges :gng g)
    (show "(>> g 'edges) {}" (hash2lst (>> g 'edges))) 
    (let ((lst (hash2lst (>> g 'edges))))
      (format t "~a~%" lst)
      (format t "~a~%" (getf (getf lst 0) 'to)))
    (loop for i in (range 10000) do
      (train-gng :gng g :input '((20 20) (25 20) (25 25) (20 25))))
    (format t "nodes: ~a~%" (hash2lst (>> g 'nodes)))
    (format t "edges: ~a~%" (hash2lst (>> g 'edges)))))
;;(test-gng)

(defun set-samples ()
  (let* ((n 1000)
	  (cf1 (circumference-distr :center '(0.8 0.475) :radius 0.1 :n n))
	  (cf2 (circumference-distr :center '(0.65 0.4) :radius 0.015 :n n))
	  (cl1 (circle-distr :center '(0.25 0.65) :radius 0.025 :n (/ n 2)))
	  (cl2 (circle-distr :center '(0.675 0.625) :radius 0.035 :n n))
	  (r1 (rectangle-distr :center '(0.425 0.5) :w 0.05 :h 0.2 :n n))
	  (r2 (rectangle-distr :center '(0.575 0.5) :w 0.05 :h 0.2 :n n))
	  (r3 (rectangle-distr :center '(0.5 0.575) :w 0.1 :h 0.05 :n (/ n 2)))
	  (r4 (rectangle-distr :center '(0.5 0.425) :w 0.1 :h 0.05 :n (/ n 2)))
	  (samples (append cf1 cf2 cl1 cl2 r1 r2 r3 r4)))
    (shuffle samples)))
;;(show "{}" (set-samples))

(defun main-loop-gng-test (&key (width 600) (height 300) (timex 2) (samples (set-samples)) (g (make-gng)))
  (sdl:with-init ()
    (sdl:window width height :flags sdl:sdl-opengl
      :title-caption "visual"
      :icon-caption "visual")
    ;;(sdl:window w-width w-height :opengl t :opengl-attributes '((:sdl-gl-depth-size 16)(:sdl-gl-doublebuffer 1)))
    (setf (sdl:frame-rate) 0)
    (>> g 'max-nodes 1000)
    ;; load GL extensions
    ;;(setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	(when (< timex 3000) (incf timex))
	(train-gng :gng g :input (list (random-choice samples)) :init-nodes 3)
	;; keep working with main loop (sbcl) :fd-handler swank:*communication-style*
	;;#+(and sbcl (not sb-thread)) (restartable (sb-sys:serve-all-events 0))
	(gl:clear-color 1 1 1 1)
	(gl:clear :color-buffer :depth-buffer)  
	(let ((origin (list 50 50)) (width (/ width 3)) (height (/ height 1.5)))
	  (before-visual-2D :origin origin :width width :height height :color '(0 0 1 1) :border? t)
	  (visual-samples samples :origin origin :width width :height height :color '(0 0 1 1) :border? t)
	  (visual-gng g :origin origin :width width :height height :color '(1 0 0 1) :border? nil))
	;;(draw-triangle2 :origin (list (/ width 2) 0) :width (/ width 2) :height height)
	(let ((origin (list (+ (/ width 2) 10) 10)) (width (- (/ width 2) 20)) (height (- height 20)))
	  (before-visual-2D :origin origin :width width :height height :color '(0 0 1 1) :border? t)
	  (draw-curve :origin origin :width width :height height :timex timex))
	(gl:flush)
	(sdl:update-display)))))
;;(main-loop-gng-test :width 1000 :height 300)






