;;;; art.lisp

(in-package #:util)

;; do not use defparameter,defconstant,defvar, and try to copy-table, they will affect each other
(defun make-art ;;each sample should be the same length. e.g. [0.3,0.5,0.1,0.9] for 4 features
  (&key
    (max-num-categories nil)
    (vigilance 0.75)
    (learning-rate 0.2)
    (complement-code t))
  (>>
    'max-num-categories max-num-categories
    'vigilance vigilance
    'learning-rate learning-rate
    'complement-code complement-code
    'nodes (>>) ;;prototypes eg,a prototype [0.3,0.5,0.1,0.9] for 4 features, like nodes in SOM
    ))

;; complement code pairs [n, 1 - n, ...] e.g. [0.2, 0.3] -> [0.2,0.8, 0.3,0.7]
(defun complement-code (&key (data '(0.2 0.3))) 
  (flatten (mapcar #'(lambda (x) (list x (- 1 x))) data)))

;;[1,2,3],[4,5,6] -> 1*4+2*5+3*6 -> 32
(defun inner (&key (a '(1 2 3)) (b '(4 5 6)))
  (apply '+ (mapcar #'(lambda (x y) (* x y)) a b)))

;;similarity: greater the result is, more similar they are. [1,5,3],[6,3,2]->(1+3+2)/(1+5+3)=2/3
(defun fuzzy-and (&key (a '(1 5 3)) (b '(6 3 2)))
  (let ((sum-a (apply '+ a)))
    (if (= 0 sum-a)
      0
      (/ (apply '+ (mapcar #'(lambda (x y) (min x y)) a b)) sum-a))))

(defun update-art-node (&key (art (make-art)) (sample '(0.2 0.3 0.1 0.6)) (node-key 0))
  ;;move art-node towards sample, low learning-rate -> small move
  (when (>> art 'complement-code) (setf sample (complement-code :data sample)))
  (>> art `(nodes ,node-key)
    (mapcar #'(lambda (x y) (+ y (* (>> art 'learning-rate) (- x y)))) sample (>> art `(nodes ,node-key)))))

(defun categorize-art (&key (art (make-art)) (sample '(0.2 0.3 0.1 0.6)))
  (when (not (equalp (>>) (>> art 'nodes)));;existing categories
    (let ((candi-keys '()) (similarities '())
	   (sample1 (if (>> art 'complement-code) (complement-code :data sample) sample)))
      (dolist (i (hash-keys (>> art 'nodes)))
	(let ((similarity (fuzzy-and :a sample1 :b (>> art `(nodes ,i)))))
	  (when (> similarity (>> art 'vigilance))
	    (push i candi-keys)
	    (push similarity similarities))))
      (when candi-keys
	(let ((key (nth (position (apply 'max similarities) similarities) candi-keys)))
	  (update-art-node :art art :sample sample :node-key key)
	  (return-from categorize-art key))))) ;;return the key of art-node
  nil)

(defun add-new-art-category (&key (art (make-art)) (sample '(0.2 0.3 0.1 0.6)))
  (let ((key (if (= 0 (hash-table-count (>> art 'nodes))) 0 (+ 1 (apply 'max (hash-keys (>> art 'nodes))))))
	 (sample1 (if (>> art 'complement-code) (complement-code :data sample) sample)))
    (>> art `(nodes ,key) sample1)
    key)) ;;return the key of new art-node

(defun step-art (&key (art (make-art)) (sample '(0.2 0.3 0.1 0.6)))
  (let ((category (categorize-art :art art :sample sample)))
    (when (null category)
      (let ((max-num-cates (>> art 'max-num-categories)))
	(when (not (and max-num-cates (= (hash-table-count (>> art 'nodes)) max-num-cates))) ;;existing space for new category
	  (setf category (add-new-art-category :art art :sample sample)))))
    category)) ;;return the key of art-node

;;e.g. data=[[0.2,0.3,0.1,0.6],[0.1,0.7,0.3,0.5]] are samples
;;return categories eg,[0, 0, 1, 0, 1, 2] if len(data)==6  
(defun train-art (&key (art (make-art)) (data '((0.2 0.3 0.1 0.6)(0.1 0.7 0.3 0.5))))
  (mapcar #'(lambda (x) (step-art :art art :sample x)) data))

(defun test-art
  (&key (data-train '((1 1 1 1 1 0 0 0 0 0 0 0)
		       (1 1 1 0 1 0 0 0 0 0 0 0)
		       (0 0 0 0 0 0 0 1 1 1 1 1)
		       (1 1 0 1 1 0 0 0 0 0 0 0)
		       (0 0 0 0 0 0 0 1 1 1 1 0)
		       (0 1 0 1 0 1 0 1 0 1 0 1)))
    (data-test '((1 1 1 0 1 0 0 0 0 0 0 0)
		  (0 0 0 0 0 0 0 1 1 0 1 0)
		  (0 0 0 0 0 1 1 0 0 0 0 0)
		  (0 0 0 0 0 0 0 0.6 0.6 0.6 0.6 0))))
  (let* ((net (make-art))
	  (train-result (train-art :art net :data data-train)))
    (show "Training results: {}" train-result)
    (dolist (da data-test)
      (print (categorize-art :art net :sample da)))))

;;(test-art :data-train '((0.8 0.1 0.7 0.9) (0.1 0.7 0.3 0.5)) :data-test '((0.2 0.6 0.4 0.4) (0.7 0.2 0.6 0.8)))
