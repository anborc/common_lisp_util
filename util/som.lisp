;;;; som.lisp
;;SOMETHING WRONG WITH experiments/util/SOM3.py
;;~/experiments/201204_language_game2_bricks_netlogo/SOM2.lisp

;;TODO: is it necessary to set (>> som 'epoch 0) before each train-som

(in-package #:util)

;;(squared-distance :v0 '(1 2 3) :v1 '(2 3 5.2))

;; do not use defparameter,defconstant,defvar, and try to copy-table, they will affect each other
(defun make-som 
  (&key
    (r 12);;default 12*12=144 utterances as nodes
    (epochs 40)
    (epoch-iterations 400) ;;400
    (init-learning-rate 0.9)
    (nodes (>>)) ;;(nodes-labels (>>)) match the same key (number) with node-label and node
    ;;each node or sample should be the same length (dim). e.g. [0.3,0.5,0.1,0.9] for 4 features
    (node-dim 1024) 
    )
  (if (equalp (>>) nodes) ;;set node-dim as the same as the length of each node
    (dotimes (i (* r r)) (>> nodes i (randomize node-dim)))
    (let ((key (car (random-sample :seq (hash-keys nodes))))
	   (num-nodes (length (hash-keys nodes))))
      (setf node-dim (length (>> nodes key)))
      (setf r (ceiling (sqrt num-nodes)))
      (when (> (* r r) num-nodes)
	(dotimes (i (- (* r r) num-nodes))
	  ;;copy the first (- (* r r) num-nodes) nodes 
	  (>> nodes (length (hash-keys nodes)) (copy (>> nodes i)))))))
  (>>
    'r r
    'epochs epochs
    'epoch-iterations epoch-iterations
    'init-learning-rate init-learning-rate
    'nodes nodes ;;prototypes e.g. a prototype [0.3,0.5,0.1,0.9] with dims = 4
    'node-dim node-dim
    'time-constant (let ((x (log (/ r 2)))) (when (= 0 x) (setf x 0.000001)) (/ epochs x)) 
    ;;threshold of training, if epoch increased to epochs, training would stop
    'epoch 1  
    ;; recent-cates store the last 5 runs of categorizing the samples
    ;; if the last 5 results are the same, it means SOM becomes stable, no need to train again
    'recent-cates (range 5) ;;mapping-lsts threshold for stopping training
    ))

;;return the radius of influence for current epoch
(defun som-radius-decay (&key (som (make-som)))  
  (let* ((r (>> som 'r))
	  (epoch (>> som 'epoch))
	  (time-constant (>> som 'time-constant)))
    (* (/ r 2) (exp (- (/ epoch time-constant))))))

;; return the learning rate for current epoch
(defun som-learning-rate-decay (&key (som (make-som)))
  (let ((init-learning-rate (>> som 'init-learning-rate))
	 (epoch (>> som 'epoch))
	 (epochs (>> som 'epochs)))
    (* init-learning-rate (exp (- (/ epoch (- epochs epoch)))))))

;; calculate the neiborhood function depending on dist to bmu and current radius
;; both dist and radius are squared
(defun som-influence-decay (&key (dist 1) (radius 2))
  ;; dist/2/radius == dist/(2*radius)
  (exp (- (/ dist 2 radius))))

;; categorize sample
;; find the closest node (BMU: Best Matching Unit) matching sample, and return the key of the node.
;; the key can be taken as the category of the sample
(defun som-find-bmu (&key (som (make-som :nodes (>> 0 '(0.2 0.3) 1 '(0.1 0.3)))) (sample '(0.1 0.35)))
  (let ((diss (mapcar #'(lambda (x) (list (squared-distance :v0 sample :v1 (>> som `(nodes ,x))) x)) (hash-keys (>> som 'nodes)))))
    (second (car (sort diss 'list<))))) ;;return the key of matched SOM node

;; som-categorize = som-find-bmu
(defun som-categorize (&key (som (make-som)) (sample (randomize 1024)))
  (som-find-bmu :som som :sample sample)) ;;return the key of matched SOM node

(defun som-nodes-distance (&key (n0 2) (n1 21) (r 12))
  (let ((x0 (mod n0 r))
        (y0 (floor (/ n0 r)))
        (x1 (mod n1 r))
        (y1 (floor (/ n1 r))))
    (sqrt (+ (* (- x0 x1) (- x0 x1)) (* (- y0 y1) (- y0 y1))))))

(defun som-adjust-nodes
  (&key
    (som (make-som :nodes (>> 0 '(0.2 0.3) 1 '(0.1 0.3) 2 '(0.9 0.1) 3 '(0.5 0.4))))
    (bmu 1) ;;bmu is the key of current central node (matched SOM node)
    (sample '(0.1 0.35))
    (radius 2)
    (learning-rate 0.5))
  (dolist (k (hash-keys (>> som 'nodes)))
    (let ((dist (som-nodes-distance :n0 bmu :n1 k :r (>> som 'r))))
      (when (< dist radius)
	(let ((influence (som-influence-decay :dist dist :radius radius))
	       (node (>> som `(nodes ,k))))
	  ;;(show "node: {} {}" k node)
	  (dotimes (i (length node))
	    (setf (nth i node) (+ (nth i node) (* influence learning-rate (- (nth i sample) (nth i node))))))
	  (>> som `(nodes ,k) node)))))
  ;;(show "{}" (>> som 'nodes))
  )

(defun train-som
  (&key
    (som (make-som :nodes (>> 0 '(0.2 0.3) 1 '(0.1 0.3) 2 '(0.9 0.1) 3 '(0.5 0.4))))
    (samples '((0.2 0.3) (0.7 0.2)))
    (file nil)) ;;e.g. "quicklisp/local-projects/lg/data-hrr/hrr-som.txt"
  (>> som 'epoch 1)
  (loop while (and (< (>> som 'epoch) (>> som 'epochs))
		(/= 1 (length (remove-duplicates (>> som 'recent-cates) :test 'equal))))
    do
    (let ((radius (som-radius-decay :som som));;radius & learning-rate become smaller
	   (learning-rate (som-learning-rate-decay :som som)))
      (dotimes (i (>> som 'epoch-iterations))
	(show "epoch {} iter {}" (>> som 'epoch) i)
	(let* ((sample (car (random-sample :seq samples)))
		(bmu (som-find-bmu :som som :sample sample)))
	  (som-adjust-nodes :som som :bmu bmu :sample sample :radius radius
	    :learning-rate learning-rate))))    
    (let ((cates (butlast (>> som 'recent-cates))))
      (push (mapcar #'(lambda (x) (som-find-bmu :som som :sample x)) samples) cates)
      (>> som 'recent-cates cates))
    (show "epoch {} cates {}" (>> som 'epoch) (car (>> som 'recent-cates)))
    (when file
      (let ((output (format nil "epoch ~a cates ~a" (>> som 'epoch) (car (>> som 'recent-cates)))))
	(if (= (>> som 'epoch) 1)
	  (extend-file :data output :file file :new t)
	  (extend-file :data output :file file :new nil))))
    (>> som 'epoch (+ 1 (>> som 'epoch))))
  (mapcar #'(lambda (x) (som-find-bmu :som som :sample x)) samples)) ;;return node key(s)
;;(train-som :file "quicklisp/local-projects/temp.txt")



