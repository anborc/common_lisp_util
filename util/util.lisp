;;;; util.lisp (depends on :alexandria :fft)

(in-package #:util)
;;including HRR

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
      for old-pos = 0 then (+ pos part-length)
      for pos = (search part string
		  :start2 old-pos
		  :test test)
      do (write-string string out
	   :start old-pos
	   :end (or pos (length string)))
      when pos do (write-string replacement out)
      while pos))) 

(defun c2l0 (&optional (c '(> (- 10 1) 2)))
  (let ((result '([)))
    (dolist (l c)
      (if (listp l)
	(setf result (append result (c2l0 l)))
	(setf result (append result (list l)))
	))
    (setf result (append result '(])))
    result))

(defun c2l (&optional (c '(0.01 (> (- 10 1) 2))))
  (let ((switch nil));check if we need c2l in advance
    (dolist (item c)
      (when (listp item) (setf switch t) (return)))
    (if switch
      (let ((result '()))
	(dolist (l c)
	  (if (listp l)
	    (setf result (append result (c2l0 l)))
	    (setf result (append result (list l)))))
	result)
      c)))
;;(show "{}" (c2l '(0.01 (> (- 10 1) 2))))
;;(show "{}" (c2l '(0.01 > - 10 1 2)))

(defun l2c (&optional (l '([ > [ - 10 1 ] 2 ])))
  (if (= (count '[ l) (count '] l))
    (let ((l (format nil "~a" l))) 
      (setf l (replace-all l "[" "(")) 
      (setf l (replace-all l "]" ")")) 
      (read-from-string l))
    nil))
;;(show "{}" (eval (l2c '(> [ - 10 1 ] 2))))
;;(show "{}" (eval (l2c (c2l '(+ (- 10 1) 2)))))
;;(show "{}" (eval (l2c '(> [ - 10 ] ] 2))))

(defun split (symtr &key (gap "_"))
  (let* ((str (if (symbolp symtr) (symbol-name symtr) symtr))
	  (lst (loop for i = 0 then (1+ j)
		 as j = (position (character gap) str :start i)
		 collect (subseq str i j)
		 while j)))
    (if (symbolp symtr) (mapcar #'read-from-string lst) lst)))
;;(equal '(as can be seen) (split 'as_can_be_seen))
;;(equal '(*v *size) (split '*v_*size))
;;(length (split nil))
;;(remove-if #'(lambda (x) (> (length (split x)) 1)) '(!V_!S !V_!M !S !M !L))
;;(split "fe^ku" :gap "^")

(defun join (lst &key (link "_"))
  (let ((string? t))
    (dolist (l lst)
      (when (not (stringp l))
	(setf string? nil)
	(return)))
    (setf lst (remove-if #'(lambda (x) (null x)) lst))
    (setf lst (mapcar (lambda (x) (format nil "~a" x)) lst))
    (if string?
      (format nil (concatenate 'string "~{~A~^" link "~}") lst)
      (read-from-string (format nil (concatenate 'string "~{~A~^" link "~}") lst)))))
;;(join '("as" "can" "be" "seen"))
;;(join '("as" "can" "be" "seen") :link "")
;;(equal 'as_can_be_seen (join '(as can be seen)))
;;(join (list nil 'as)) --> 'as
;;(split (join (list '!compo-size '!very_!size) :link "+") :gap "+")
;;(equal '1_2_3 (join '(1 2 3)))

;;list to symbol
(defun l2s (&optional (lst '(a (b c))))
  (if (not (listp lst))
    lst
    (join (c2l lst)))) ;(show "{}" (l2s '((a s)(d (h e) r))))

;;symbol to list
(defun s2l (&optional (symbol 'a_[_b_c_]))
  (if (not (symbolp symbol))
    symbol
    (l2c (split symbol)))) ;(show "{}" (s2l 'a_[_v_s_]))

(defun replace*_ (&key (item 'medium) (of '(/ (/ medium 2) 2)) (with 1))
  (let ((list? nil))
    (if (listp of)
      (progn
	(setf of (format nil "~a" (c2l of)))
	(setf list? t))
      (progn
	(setf of (read-from-string of))
	(setf of (format nil "~a" (c2l of)))))  
    (when (not (stringp item)) (setf item (format nil "~a" item)))
    (when (not (stringp with)) (setf with (format nil "~a" with)))
    (setf of (replace-all of item with))
    (if list?
      (l2c (read-from-string of))
      (format nil "~a" (l2c (read-from-string of))))))

(defun replace* (&key (item 'medium) (of '(/ (/ medium 2) 2)) (with 1))
  (setf item (format nil "~a" (l2s item)))
  (setf of (format nil "~a" (l2s of)))
  (setf with (format nil "~a" (l2s with)))
  (s2l (read-from-string (replace-all of item with)))
  )
;;(replace* :item 'medium :of '(/ (/ medium 2) 2) :with 1)
;;(replace* :item '(medium) :of "(/ (/ medium 2) 2)" :with 1)
;;(replace* :item '(medium r f) :of '(medium r f) :with 'odd)
;;(replace* :item '((a b)) :of '((a b) c) :with 'd)
;;(replace-all "[_a_b_]_c" "[_a_b_]" "d")

(defun len (ob)
  (cond ((hash-table-p ob) (hash-table-count ob))
    (t (length ob))))
(defun test-len ()
  (let (h (make-hash-table))
    (print (len h)))
  (print (len "asdfa;"))
  (print (len '(1 2 3))))
;(test-len)

(defun print-hash-entry (key value)
  (if (hash-table-p value) (progn (format t "KEY: ~S ~%" key) (print-hash-table value)) (format t "key: ~S value: ~S~%" key value)))
(defun print-hash-table (hash-table)
  (maphash #'print-hash-entry hash-table))

(defmacro defclass* (name &optional classes &rest slots)
  (let ((body (mapcar #'(lambda (x) (list (first x) :accessor (first x) :initarg (first x) :initform (second x))) slots)))
    (if (null classes) `(defclass ,name () ,body) `(defclass ,name ,classes ,body))))

(defun test-class ()
  (defclass* circle () (radius 1) (center (cons 0 0)))
  (defclass* wood () (texture "grid"))
  (defclass* circle2 (circle wood) (color "red"))
  (defparameter c (make-instance 'circle))
  (defparameter c2 (make-instance 'circle2))
  (defparameter c (make-instance 'circle 'radius 2 'center 0))
  (setf (radius c) 1)
  (radius c)
  (radius c2)
  (color c2)
  (texture c2)
  (center c)
  (slot-value c 'radius))
;(test-class)

#|
(defmacro >> (&optional dict key (value "empty"))
  `(cond
     ((null ',dict)(make-hash-table))
     ((null ,key)(defparameter ,dict (make-hash-table)))
     ((consp ,key)
       (if (equalp "empty" ,value)
	 (let ((v (gethash (car ,key) ,dict)))
	   (dotimes (i (- (length ,key) 1))
	     (setf v (gethash (nth (+ 1 i) ,key) v))) v)
	 (if (= 1 (length ,key))
	   (setf (gethash (car ,key) ,dict) ,value)
	   (let ((v (gethash (car ,key) ,dict)))
	     (dotimes (i (- (length ,key) 1))
	       (if (/= i (- (length ,key) 2))
		 (setf v (gethash (nth (+ 1 i) ,key) v))
		 (setf (gethash (nth (+ 1 i) ,key) v) ,value)))))))
     ;;At macro, cdr is not evaluated
     ((equalp "empty" ,value)(gethash ,key ,dict))     
     (t (setf (gethash ,key ,dict) ,value))))
|#

;;make hash-table
;;get hash-value, assign new hash-value
;;get item of a list, assign new item-value
;;get characters of a string, assign new characters to the string
(defun >> (&rest data)
  (if (null data)
    (make-hash-table)
    (if (and (not (hash-table-p (car data))) (not (stringp (car data))) (atom (car data)));;hash-table is also atom
      ;;make hash-table e.g. (>> 'a 1 'b (>> 'sd 'foo 'd 4)) -> {a:1 b:{sd:'foo d:4}}
      (let ((dic (make-hash-table)))
	(dotimes (i (length data))
	  (when (= 0 (mod i 2))
	    (setf (gethash (nth i data) dic) (nth (+ 1 i) data))))
	dic)
      (cond
	((hash-table-p (car data)) ;;get hash-value or set new hash-value
	  (let ((seq (car data)) (ik (second data)))
	    (if (listp ik) ;;including sub-keys
	      (if (= (length data) 2) ;;no new value, just get hash-value
		(let ((v (gethash (car ik) seq)))
		  (dotimes (i (- (length ik) 1))
		    (setf v (gethash (nth (+ 1 i) ik) v))) v)
		;; assign new hash-value
		(let ((value (third data)))
		  (if (= 1 (length ik))
		    (setf (gethash (car ik) seq) value)
		    (let ((v (gethash (car ik) seq)))
		      (dotimes (i (- (length ik) 1))
			(if (/= i (- (length ik) 2))
			  (setf v (gethash (nth (+ 1 i) ik) v))
			  (setf (gethash (nth (+ 1 i) ik) v) value)))))))
	      ;;single key
	      (if (= (length data) 2) ;;no new value, just get hash-value
		(gethash ik seq)
		;; assign new hash-value
		(setf (gethash ik seq) (third data))))))
	((listp (car data)) ;;get an item of a list or set new value to the item
	  (let ((seq (car data)) (ik (second data)))
	    (if (listp ik) ;;including sub-indexes
	      (if (= (length data) 2) ;;no new value, just get item
		(let ((v (nth (car ik) seq)))
		  (dotimes (i (- (length ik) 1))
		    (setf v (nth (nth (+ 1 i) ik) v))) v)
		;; assign new value
		(let ((value (third data)))
		  (if (= 1 (length ik))
		    (setf (nth (car ik) seq) value)
		    (let ((v (nth (car ik) seq)))
		      (dotimes (i (- (length ik) 1))
			(if (/= i (- (length ik) 2))
			  (setf v (nth (nth (+ 1 i) ik) v))
			  (setf (nth (nth (+ 1 i) ik) v) value)))))))
	      (if (= (length data) 2) ;;no new value, just get item
		(nth ik seq)
		(setf (nth ik seq) (third data))))))
	((stringp (car data)) ;;get an item of a string or set new value to the item
	  (let ((seq (car data)) (ik (second data)) (start nil) (end nil))
	    (if (listp ik) ;;including sub-indexes
	      (progn (setf start (car ik)) (setf end (second ik)))
	      (progn (setf start ik) (setf end (+ ik 1))))
	    (if (= (length data) 2)
	      (subseq seq start end);;no new value, just get item
	      ;; assign new value
	      ;;(setf seq (join (list (subseq seq 0 start) (third data) (subseq seq end (len seq))) :link ""))
              (setf seq (format nil "~a~a~a" (subseq seq 0 start) (third data) (subseq seq end (len seq)) :link ""))
              )))
	(t t)))))

;;(let ((str "adfefefe")) (foo str) str)
;;(>> "abcdef" 2);;c
;;(>> "abcdef" '(2 4));;"cd"
;;(let ((str "abcdef")) (>> str 2 "vf")) ;;"abvfdef";;CAUTION: string is immutable
;;(>> "abcdef" '(2 4) "v");;"abvef"
;;(>> "abcdef" '(2 4) "vvvvv");;"abvvvvvef"

;;(replace "abcdefghij" "0123456789" :start1 0 :end1 3 :start2 0 :end2 1) 
;;(let ((a "abcdefghij"))(replace a "0" :start1 4 :end1 5 :start2 0) a)
;;(let ((seq "abcdefghij") (ik '(1 3))) (setf seq (subseq seq (car ik)(second ik))) seq)
;;(subseq "abcdefghij" 1 2)
;;(len "abcdefghij")
;;(length "abcdefghij")

;;(show "a list: {}" '(a b (c d (r t y) g)))
;;(nth 0 (nth 2 (nth 2 '(a b (c d (r t y) g)))))
;;(let ((seq '(a b (c d (r t y) g)))) (>> seq '(2 2 0))) ;r
;;(let ((seq '(a b (c d (r t y) g)))) (>> seq '(2 2 0) 100) seq) ;;TODO: replace a part of list
;;(let ((seq '(a b (c d (r t y) g)))) (>> seq 2))
;;(let ((seq '(a b (c d (r t y) g)))) (>> seq 2 100) seq)

;;(show "a hash-table: {}" (>> 'a 1 'b (>> 'sd 'foo 'd 4)))
;;(let ((ab (>> 'a 1 'b (>> 'c 'foo 'd 4)))) (show "(>> ab '(b c)): {}" (>> ab '(b c)))) ;foo
;;(let ((ab (>> 'a 1 'b (>> 'c 'foo 'd 4)))) (>> ab '(b c) 100) (show "ab: {}" ab))
;;(let ((ab (>> 'a 1 'b (>> 'c 'foo 'd 4)))) (show "(>> ab 'b): {}" (>> ab 'b)))
;;(let ((ab (>> 'a 1 'b (>> 'c 'foo 'd 4)))) (>> ab 'b "empty") (show "ab: {}" ab))
;;(>>)
;;(atom (>>)) ;;hash-table is atom
;;(let ((h (>>))) (>> h 'a 100) (show "h: {}" h))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))
;;(hash-keys (>> 'a 1 'b 2 'c 3))

(defun hash-values (hash-table)
  (mapcar (lambda (key) (>> hash-table key)) (hash-keys hash-table)))
;;(show "{}" (hash-values (>> 'a 1 'b 2 'c 3 'd (>> 0 'a))))

(defun hash2string (dic)
  ;;(format t "(list") ;convert to plist function
  ;;(format t "(hash") ;convert to hash function
  (let ((strs '()))
    (push (format nil "(") strs)
    (dolist (d (hash-keys dic))
      (if (hash-table-p (>> dic d))
	(progn
	  (push (format nil "~(~a~) " d) strs)
	  (push (hash2string (>> dic d)) strs))
	(push (format nil "~(~a~) ~(~a~) " d (>> dic d)) strs)))
    (push (format nil ") ") strs)
    (setf strs (reverse strs))
    (setf strs (join strs :link ""))
    strs))
;;(hash2string (>> 'a 0 'b 2))

(defun hash2lst (dic)
  (read-from-string (hash2string dic)))

(defun plist2hash (lst)
  (dotimes (i (length lst))
    (when (and (oddp i) (listp (>> lst i))) (>> lst i (plist2hash (>> lst i)))))
  (alexandria:plist-hash-table lst))
;;(let* ((lst '(1 (2 (3 4)) a (b (c d g h) e f) d f)) (dic (plist2hash lst))) (>> dic '(a b g)))
;;(equalp (>> (read-from-string "A") 1) (>> 'a 1))
;;(join (list "qq" "! ") :link "")

(defun hash2org (&key (dic (>> 0 (>> 'a (>> 'c "Hello" 'd (>> 'qq (>> 'ww 22))) 'e '(>>)) 1 'earth 2 'baby)) (file "/home/an/Documents/temp.org") (offset "") (start t))
  (when start
    (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)) ;;create an empty file
    (setf start nil))
  (dolist (k (hash-keys dic))
    (if (and (hash-table-p (>> dic k)) (not (null (hash-keys (>> dic k)))))
      (progn
        (extend-file :data (format nil "~a* ~a~%" offset k) :file file :newline nil)
	(let ((offset-now (join (list offset "*") :link "")))
	  (hash2org :dic (>> dic k) :file file :offset offset-now :start start)))
      (let ((h-value (>> dic k)))
	(when (hash-table-p h-value) (setf h-value '(>>)))
	(extend-file :data (format nil "~a* ~a~%~a~%" offset k h-value) :file file :newline nil)))))
;;(hash2org :dic (>> 'BB_X91HJ_1 (>> 'Title "1x HI VIS HODDIE JUMPER,SAFETY WORKWEAR,2XS-4XL,KANGAROOPOCKET" 'Sizes '(XS S M L XL 2XL 3XL 4XL)) 'BB_CP01_S_1 (>> 'Title "1x WORK CARGO SHORTS,Cotton Drill CANVAS,MULTI POCKETS,TRADIE,HEAVY DUTY" 'Sizes "77cm/30inch, 82cm/32inch, 87cm/34inch, 92cm/36inch, 97cm/38inch, 102cm/40inch, 107cm/42inch" 'weight 500)))
;;(hash2org)

;;limitation 1: each content must be only one line
;;limitation 2: the structure must be strict hash-table (>> 'a (>> 'b "c" 'd "d1")) without mixing list like (>> 'a (list (>> 'c "d") 'b))
(defun org2hash (&key (file "/home/an/Documents/temp.org"))
  (let ((lst-str (with-open-file (stream file) ;;each line as an item, and make a list
                   (loop for line = (read-line stream nil) while line collect line)))
         (str "") ;;each item of lst-str will be converted to a string and be joined into str
         (level 0)) ;;eg. heading 1, heading 2,...
    (dolist (e lst-str)
      (if (equalp (>> e 0) "*") ;;heading
        (let ((lst-key (split e :gap " ")) ;;take heading as key of hash-table
               (pre-level level)) ;;pre-level for comparing current level
          (setf level (length (car lst-key)));;current level
          (let ((level-difference (- level pre-level)))
            (cond
              ((> level-difference 0) ;;sub-heading
                (setf str (join (list str (join (mapcar (lambda (x) "(") (range level-difference)) :link "") (cadr lst-key)) :link " ")))
              ((< level-difference 0) ;;return to parent-heading
                (setf str (join (list str (join (mapcar (lambda (x) ")") (range (abs level-difference))) :link "") (cadr lst-key)) :link " ")))
              (t (setf str (join (list str (cadr lst-key)) :link " ")))))) ;;same level
        (setf str (join (list str (join (list "\"" e "\"") :link "")) :link " ")))) ;;each content(a string) as value of hash-table
    (plist2hash ;;convert list to hash-table
      (read-from-string ;;convert string to list
        (join (list str (join (mapcar (lambda (x) ")") (range level)) :link "")) :link "")))))
;;(let ((dic (org2hash))) (show "{}" (hash-keys (>> dic 'BB_CP01_S_1))) (>> dic '(BB_CP01_S_1 colour) "Black / Lime") (hash2org :dic dic))
;;(let ((dic (org2hash))) (show "{}" (hash-keys dic)))
;;(read-from-string (join (list "( 12" (join (list "(" "\"" "qw" "\"" ")") :link "") ")") :link " "))

(defun csv2org (&key (csv "/home/an/Documents/temp.csv") (org "/home/an/Documents/temp.org"))
  (with-open-file (out org :direction :output :if-exists :supersede :if-does-not-exist :create))
  (let* ((lst (cl-csv:read-csv (pathname csv))) ;;(pathname csv) -> #p"/home/an/Documents/temp.csv"
          (headings (nth 0 lst))
          (r 0))
    (dolist (l lst)
      (if (= r 0)
        (progn
          (incf r))
        (dotimes (i (length (nth 0 lst)))
          (if (= i 0)
            (let ((switch (if (= r 1) nil t))) (extend-file :data (join (list "*" (nth i l)) :link " ") :file org :newline switch))
            (progn
              (extend-file :data (join (list "**" (nth i headings)) :link " ") :file org :newline t)
              (extend-file :data (nth i l) :file org :newline t)))
          (incf r))))
    lst))
;;(csv2org)
;;(csv2org :csv "/home/an/Documents/jb.csv" :org "/home/an/Documents/jb.org")

(defun csv2hash (&key (file "/home/an/Documents/temp.csv"))
  (let* ((lst (cl-csv:read-csv (pathname file))) ;;(pathname csv) -> #p"/home/an/Documents/temp.csv"
          (headings (mapcar (lambda (x) (read-from-string x)) (nth 0 lst)))
	  (num-headings (length headings))
          (r 0)
	  (dic (>>))
	  (id-now nil))
    (setf lst (cdr lst))
    (dolist (l lst)
      (setf id-now (read-from-string (nth 0 l)))
      (>> dic id-now (>>))
      (dotimes (i num-headings)
	(when (> i 0)
	  ;;(>> dic (list id-now (nth i headings)) (str2num (nth i l)));;Do not use str2num which may delete "": eg. "black, red, blue" -> black, red, blue. This makes a string wrongly become 3 items.
	  (>> dic (list id-now (nth i headings)) (nth i l))
	  )))
    dic))

(defun test-csv2hash ()
  (let ((dic (csv2hash))
	 (weights '()))
    (show "{}" dic)
    (dolist (l (hash-keys dic))
      (push (str2num (>> dic (list l 'weight))) weights))
    (apply '+ weights)))
;;(test-csv2hash)

(defun hash2csv (&key (dic (>> 1 (>> 'name "Jack" 'age "21") 2 (>> 'name "Mary" 'age "19"))) (file "temp.csv"))
  (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create))
  (let* ((keys (hash-keys (>> dic (nth 0 (hash-keys dic)))))
          (headings (append '("ID") (mapcar (lambda (x) (format nil "~a" x)) keys)))
          (row '()))
    (extend-file :data (join headings :link ",") :file file :newline nil)
    (dolist (id (hash-keys dic))
      (push (format nil "~a" id) row)
      (setf row (append row (mapcar (lambda (x) (join (list "\"" (>> dic (list id x)) "\"") :link "")) keys)))
      (extend-file :data (join row :link ",") :file file :newline t)
      (setf row '()))
    ))
;;(hash2csv)

(defun add-new-column2csv (&key (key 'status) (value "DONE") (csv "temp.csv"))
  (let ((dic (csv2hash :file csv)))
    (dolist (k (hash-keys dic)) (>> dic (list k key) value))
    (hash2csv :dic dic :file csv)))
;;(add-new-column2csv :key 'test :value "clear" :csv "/home/an/Documents/temp.csv")

(defun org2csv (&key (org "/home/an/Documents/temp.org") (csv "/home/an/Documents/temp.csv"))
  (with-open-file (out csv :direction :output :if-exists :supersede :if-does-not-exist :create))
  (hash2csv :dic (org2hash :file org) :file csv))
;;(org2csv)

;;(cl-csv:read-csv #p"/home/an/Documents/temp.csv")
;;(cl-csv:read-csv "1,2,3 | 4,5,6" :separator "|")
(defun test-sum-csv ()
  (let ((sum 0) (i-row 0) (i-col 0))
    (cl-csv:do-csv (row #P"/home/an/Documents/temp.csv")
      (if (= i-row 0) 
        (progn
          (incf i-row) ;;avoid the first row
          (setf i-col (position "age" row :test 'equalp)) (show "i-col: {}" i-col)) 
        (incf sum (str2num (nth i-col row)))))
    sum))
;;(test-sum-csv)
;;(extend-file :data (join (mapcar (lambda (x) (format nil "~a" x)) '(John male 21)) :link ",") :file "/home/an/Documents/temp.csv")

(defun show-hash (&optional (dic (>> 0 (>> 'a (>> 'c "Hello" 'd (>> 'qq (>> 'ww 22))) 'e '(>>)) 1 'earth 2 'baby)) (offset "") (start t))
  (when start (format t "(") (setf start nil))
  (dolist (k (hash-keys dic))
    (if (and (hash-table-p (>> dic k)) (not (null (hash-keys (>> dic k)))))
      (progn
	;;(format t "~%~(~a~)(~(~a~)" offset k)
        (format t "~%~a(~a" offset k)
	(let ((offset-now (join (list offset "! ") :link "")))
	  (show-hash (>> dic k) offset-now start))
	(when (equalp k (car (last (hash-keys dic))))
	  (format t ")")))
      (let ((h-value (>> dic k)))
	(when (hash-table-p h-value) (setf h-value '(>>)))
	(if (equalp k (car (last (hash-keys dic))))
	  ;;(format t "~%~(~a~)(~(~a~) ~(~a~)))" offset k h-value)
          (format t "~%~a(~a ~a))" offset k h-value)
	  ;;(format t "~%~(~a~)(~(~a~) ~(~a~))" offset k h-value)
          (format t "~%~a(~a ~a)" offset k h-value)
          )))))
;;(show-hash)
;;(maphash (lambda (key value)(write (list key value))) (>> 'a 1 'b 2))

;;(defmacro show (str &rest value) `(format t (concatenate 'string (replace-all ,str "{}" "~a") " " "~%") ,@value))
(defun show (str &rest value) 
  (let ((lst (copy-seq value))
	 (str-lst (split str :gap " ")))
    (dolist (s str-lst)
      (if (equalp "{}" s)
	(let ((l (car lst)))
	  (cond
	    ((hash-table-p l)
	      (progn
		(show-hash l)
		(format t "~%")))
	    ((and (listp l) (member t (mapcar (lambda (l) (hash-table-p l)) (flatten l))))
	      (mapcar (lambda (x) (show "{}" x)) l))
	    (t
	      (if (= (length lst) 1)
		;;(format t "~(~a~)~%" l) ;; a -> (~a~) to lowercase
		(format t "~a~%" l)
                ;;(format t "~(~a~) " l)
                (format t "~a " l)
                )))
	  (setf lst (cdr lst)))
	(if (equalp s (car (last str-lst)))
	  ;;(format t "~(~a~)~%" s)
          (format t "~a~%" s)
	  ;;(format t "~(~a~) " s)
          (format t "~a " s)
          )))))
;;(show "a {} b {}" (>> 'a 1 'b (>> 'c 'foo 'd 4)) 2)
;;(show "part1: {} part2: {}" (list 2 (>> 'a (list 1 '(2 3) (>> 'a 1) '(4 (3 4) 5)) 'b (>> 'c 'foo 'd 4))) 2)
;;(show "a {} b {}" 1 2)
;;(show "a {}" '(1 2))
;;(show "a: {} b: {} c: {} (+ a b): {}" 1 2 2 (+ 1 2))

(defun copy-table (table)
  (let ((new-table (make-hash-table
                    :test (hash-table-test table)
                    :size (hash-table-size table))))
    (maphash #'(lambda(key value)
                 (setf (gethash key new-table) value))
             table)
    new-table))

(defun test-copy-table ()
  (let ((table (make-hash-table)))
    (mapcar #'(lambda(arg argg)
		(setf (gethash arg table) argg))
      '(1 2 3 4) '(a b c d))
    (format t "~a~%" table)
    (format t "~a~%" (copy-table table)))
  )
;(test-copy-table)

(defun copy (ob)
  (cond ((hash-table-p ob) (copy-table ob))
    (t (copy-seq ob))))

;; rename a key of hash table (dic)
(defun rename-key (dic old-key new-key)
  (let ((value (>> dic old-key)))
      (remhash old-key dic)
      (>> dic new-key value)))

(defun test-hash-table ()
  (>> h) ;(defparameter h (make-hash-table))
  (>> h 'two) ;(gethash 'two h)
  ;(>> h 'one 1.0 'two 2.0 'three 3.0) ;TODO
  (>> rules)
  (>> rules 0 (>>))
  (>> rules '(0 rule) "SIZE") ;(>> (>> rules '0) 'rule "SIZE")
  (equalp (>> (>> rules 0) 'rule) "SIZE")
  (equalp (>> (>> rules 0) 'rule) (>> rules '(0 rule)))
  (>> rules '(0 rule) (>>))
  (>> rules '(0 rule sub-rule) (>>))
  (>> rules '(0 rule sub-rule sub-rule2) (list 4 5 6))
  (>> rules '(0 rule sub-rule sub-rule2))
  (setq hh (>> 'x 11 'y 12 'z 23 'color (>> 'r (>> 'r0 255 'r1 10) 'g 0 'b 100 'a 0.5)))
  (>> hh '(color r r1))
  (>> hh '(color r r2) (>> 'a 1 'b 2 'c 3))
  (>> hh '(color r r2 a))
  (>> hh '(color r r2))
  (>> hh '(color r r3) (list "VERY" "SIZE"))
  (>> hh '(color r r3))
  (show-hash hh)
  (show "")
  (= 0 (hash-table-count (make-hash-table)))
   )
;;(test-hash-table)

#|
;;convert string to symbol
(read-from-string "hello") ;no distinguish uppercase and lowercase
(intern "hello") ;distinguish uppercase and lowercase
;;convert symbol to string
(symbol-name 'hello)

(symbol-name (intern "qa")) 
(let ((h (>> 'a '(1 2) 'b 2 (intern (apply #'concatenate 'string '("V" "S"))) '(3 4))))
  ;;(>> h (intern "VS"))
  (>> h 'vs))
;; string should be uppercase
(equal 'vs-e (intern "vs-e")) ;;nil
(equal 'vs-e (intern "VS-E")) ;;t 

(equal 'my-symbol (intern (symbol-name 'my-symbol)))
(symbol-name (intern "my-symbol"))
(equal 'my (intern "my")) ;;nil
(equal 'my (read-from-string "my")) ;;t
(equal 'my (intern (string-upcase "my"))) ;;t
(string-downcase "VS-E")

(equalp '(a b c) (intern (string-upcase "(a b c)")))
(equalp '(a b c) (read-from-string "(a b c)"))
(equal '(a b c) (read-from-string (format nil "~a" '(a b c))))
(equal '(a (b d) c) (read-from-string (format nil "~a" '(a (b d) c))))
(equalp (>> 'a (>> 'b 1)) (alexandria:plist-hash-table (read-from-string (format nil "~a" (alexandria:hash-table-plist (>> 'a (>> 'b 1)))))))
|#

(defun hash2plist (&optional (h (>> 'a (>> 'b (>> "(1 2 3)" 1)) 'c 3)))
  (dolist (k (hash-keys h))
    (when (hash-table-p (>> h k))
      (>> h k (hash2plist (>> h k)))))
  (alexandria:hash-table-plist h))

(defun plist-hash-table (&optional (p (list :a 1 :b (list :d 2) :c 3)))
  (show "{}" (alexandria:plist-hash-table (hash2plist))))
;;(listp (list :a 1))
;;(consp '((a . b)))
;;(show "{}" (>> (format nil "~a" (list :a 1 :b (list :d 2) :c 3)) 1))

(defun test-hash-table2 ()
  (defparameter pairRules (>>))
  (setf k (len pairRules))
  (>> pairRules k (>>))
  (>> pairRules `(,k rule) '("SIZE0.123" "VERY"))
  (>> pairRules `(,k cate-node) 0.1)
  (>> pairRules `(,k cate-thre) 0.8)
  (>> pairRules `(,k score) 0.2)
  (setf k (len pairRules))
  (>> pairRules k (>>))
  (>> pairRules `(,k rule) '("VERY" "SIZE0.123"))
  (>> pairRules `(,k cate-node) 0.1)
  (>> pairRules `(,k cate-thre) 0.8)
  (>> pairRules `(,k score) 0.9)
  ;;(print-hash-table pairRules)
  (show "{}" pairRules)
  (loop for k in (hash-keys pairRules) do
    (when (< (>> pairRules `(,k score)) 0.6)
      (remhash k pairRules)))  
  (print "--------------")
  (show "{}" pairRules)
  )
;;(test-hash-table2)

;; plist (property list) is destructive like push
(defun p> (plst keys &optional value)
  (if (null value)
    (let ((v (getf plst (car keys))))
      (dotimes (i (- (length keys) 1))
	(setf v (getf v (nth (+ 1 i) keys)))) v)
    (if (= 1 (length keys))
      (setf (getf plst (car keys)) value)
      (let ((v (getf plst (car keys))))
      (dotimes (i (- (length keys) 1))	(if (/= i (- (length keys) 2))
	  (setf v (getf v (nth (+ 1 i) keys)))
	  (setf (getf v (nth (+ 1 i) keys)) value)))))))

(defun test-p> ()
  (setq lst2 (list 'x (list 'x0 12 'x1 13) 'y 0))
  (p> lst2 '(x x0))
  (setq dic2 (alexandria:plist-hash-table lst2))
  (show "dic2 {}" dic2)
  (>> dic2 'x)
  (setq lst0 (list :x (list :x0 12 :x1 13) :y 0))
  (setq lst1 (list :x 1 :y 1))
  (setq lst (list :l0 lst0 :l1 lst1))
  (print lst)
  (defparameter ll1 (getf lst :l1))
  (print (getf ll1 :x))
  (print "----------")
  (print (p> lst `(:l0 :x)))
  (print lst)
  (p> lst '(:l0 :x :x0) 1000)
  (print lst)
  (print (p> lst '(:l0 :x :x1)))
  )
;(test-p>)

(defmacro == (a b &key >) ;CAUTION: try to avoid using this
  (if (null >)
    `(equalp ,a ,b)
    `(equal ,a ,b)))

(defun test== ()
  (print (== "as" "As"))
  (print (== "as" "As" :> nil))
  (print (== "as" "As" :> t))
  (print (== 1 1.0))
  (print (== 1 1.0 :> nil))
  (print (== 1 1.0 :> t))
  (print (== 'as 'As))
  )
;(test==)

(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
    collect n))
;(range 10 :min 2)

(defun list-depth (lst) ;;'(((-5 -4 0)(-5 -4 0)) ((-2 4 0)(-2 4 0)) ((2 -4 0)(2 -4 0)) ((4 4 0)(4 4 0)))
  (let ((depth 0))
    (loop do
      (setf lst (car lst))
      (incf depth)
      while (listp lst))
    depth))
;;(list-depth '(1 2 3))

(defun list-to-2d-array (list)
  (make-array (list (length list)
		(length (first list)))
    :initial-contents list))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
    collect (loop for j below (array-dimension array 1)
	      collect (aref array i j))))

(defun list-dimensions (list depth)
  (loop repeat depth
    collect (length list)
    do (setf list (car list))))

(defun list-to-array (list &key (depth (list-depth list)))
  (make-array (list-dimensions list depth)
    :initial-contents list))
;;(list-to-array '(((1 2 3)(4 5 6))((7 8 9)(10 11 12))))

(defun array-to-list (array)
  (let* ((dimensions (array-dimensions array))
	  (depth      (1- (length dimensions)))
	  (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
		 do (setf (nth n indices) j)
		 collect (if (= n depth)
			   (apply #'aref array indices)
			   (recurse (1+ n))))))
      (recurse 0))))
;;(let ((arr (make-array '(4 2 3) :initial-contents '(((-5 -4 0)(-5 -4 0)) ((-2 4 0)(-2 4 0)) ((2 -4 0)(2 -4 0)) ((4 4 0)(4 4 0)))))) (equalp arr (list-to-array (array-to-list arr))))

(defun is-sub-list0 (a b) ;a='("look" "after") b='("look" "after" "them")
  (let ((result nil))
    (when (member (car a) b :test #'equal)
      (let ((ind (position (car a) b :test #'equal))
	     (match t))
	(loop for i from 1 to (- (length a) 1) do
	  (cond ((= (+ i ind) (length b)) (progn (setf match nil)(return)))
	    ((not (== (nth i a) (nth (+ i ind) b) :> t)) (progn (setf match nil)(return)))))
	(when match (setf result t))))
    result))

(defun is-sub-list (a b &key (strict nil))
  (if (equalp a b)
    (if (null strict)
      (return-from is-sub-list t) ;;is-sub-list including "== a b"
      (return-from is-sub-list nil)) 
    (let (starts '())
      (dotimes (i (length b))
	(when (equalp (car a) (nth i b))
	  (push i starts))) ;;find possible start index(es)
      (when starts
	(dolist (i starts)
	  (when (is-sub-list0 a (subseq b i))
	    (return-from is-sub-list t)))))))
;;(is-sub-list '("after" "them") '("look" "after" "them"))
;;(is-sub-list '(after them) '(look after them))
;;(is-sub-list '(after them) '(after them))
;;(is-sub-list '(1 5 6 6 4) '(1 5 6 7 4 1 5 6 6 4 6))

(defun replace-lst0 (&key (part '(7 8)) (of '(6 7 8 7 8)) (with '(9)));;only replace the first matched one
  (if (equalp part of)
    (return-from replace-lst0 with)
    (let (starts '())
      (dotimes (i (length of))
	(when (equalp (car part) (nth i of))
	  (push i starts)))
      (when starts
	(dolist (i starts)
	  (when (is-sub-list0 part (subseq of i))
	    (let* ((ind0 i)
		    (ind1 (+ (length part) ind0))
		    (lst0 '())
		    (lst1 '()))
	      (loop for i from 0 to (- (length of) 1) do
		(cond ((< i ind0) (setf lst0 (append lst0 (list (nth i of)))))
		  ((>= i ind1) (setf lst1 (append lst1 (list (nth i of)))))))
	      (let ((lst-new '()))
		(loop for l in (list lst0 with lst1) do
		  (setf lst-new (append lst-new l)))
		(return-from replace-lst0 lst-new)))))))))

(defun replace-lst (&key (part '(7 8)) (of '(6 7 8 7 8)) (with '(9)))
  (loop do
    (setf of (replace-lst0 :part part :of of :with with))
    while (is-sub-list part of))
  of)
;;(replace-lst0 :part '(1) :of '(1) :with '(0 2))
;;(replace-lst0) ;'(6 7 8 7 8) --> '(6 9 7 8) not complete
;;(replace-lst)  ;'(6 7 8 7 8) --> '(6 9 9) complete 
;;(let ((lst '(!V the !N))) (print (replace-lst :part '(the !N) :of lst :with '(!NP))) lst)
;;(replace-lst :part '(1 2) :of '(1 2 1 2 1 2) :with '(0)) ;'(1 2 1 2 1 2) --> '(0 0 0)
;;(replace-lst :part '(1 5 6 6 4) :of '(1 5 6 7 4 1 5 6 6 4 6) :with '(a))
;;(replace-lst0 :part '(obj) :of '(obj) :with '(relation obj obj)) ;;ATTENTION: if replace-lst, it will do loop forever

(defun replace-sublist (old new lst)
  ; old='(1 2) new='(9) lst='(0 1 2 3 4) -> '(0 9 3 4)
  (loop do
    (setf lst (replace-lst0 :part old :of lst :with new))
    while (is-sub-list old lst))
  lst)
;;(replace-sublist '(after them) '(afthem) '(look after them))

(defun divide-range (&key (min 0) (max 1) (num 11))
  (let ((segment (/ (- max min) (- num 1))))
    (mapcar #'(lambda (x) (+ min (* x segment))) (range num))))

(defun divide1 (&key (n 9))
  (divide-range :num n))
;;(print (divide1 :n 27))

(defun random-uniform (range) ;'(0.25 0.75)
  (+ (car range) (random (- (second range) (car range)))))
;(loop for i from 0 to 100 do (print (random-uniform '(0.25 0.75))))

(defun random-choice (ob)
  (cond ((stringp ob) (coerce (list (elt ob (random (length ob)))) 'string))
    ((listp ob) (nth (random (length ob)) ob))
    ((hash-table-p ob) (let ((key (random-choice (hash-keys ob)))) (list (>> ob key) key)))
    (t ob)))
;;(random-choice "qwer")
;;(random-choice '(0.4 0.32 0.42 0.67 0.35 0.38 0.69 0.55))
;;(random-choice '((1 2) (4 5) (9 0)))
;;(random-choice (>> 'ba 0.02 'bi 0.03 'bu 0.01))

(defun random-sample (&key (seq '(82 49 10 72 35)) (num 1) (result '()))
  (if (= 0 num)
    result
    (let ((sample (nth (random (length seq)) seq)))
      (push sample result)
      (random-sample :seq (remove sample seq) :num (- num 1) :result result))))

(defun test-random-sample ()
  (random-sample :seq '(1 2) :num 2)
  (random-sample :seq '(1 2 3 4) :num 2))
;;(test-random-sample)

(defun shuffle (seq)
  (let ((n (length seq)))
    (dotimes (i n seq)
      (rotatef (elt seq i)(elt seq (+ i (random (- n i))))))))
;;(shuffle (range 10))

;(replace-sublist '(1 2) '(9) '(0 1 2 3 4))

;(<= 0 (random 1000))
;(< (random 1000) 1000)

(defun list< (a b)
  (cond
    ((null a) (not (null b)))
    ((null b) nil)
    ((equalp (first a) (first b)) (list< (rest a) (rest b)))
    ((numberp (first a)) (< (first a) (first b)))
    ((stringp (first a)) (string< (first a) (first b)))
    ((listp (first a)) (list< (first a) (first b)))))

(defun list> (a b)
  (cond
    ((null a) (not (null b)))
    ((null b) nil)
    ((equalp (first a) (first b)) (list> (rest a) (rest b)))
    ((numberp (first a)) (> (first a) (first b)))
    ((stringp (first a)) (string> (first a) (first b)))
    ((listp (first a)) (list> (first a) (first b)))))

(defun list-len> (a b)
  (>= (length a) (length b)))

(defun list-len< (a b)
  (< (length a) (length b)))

(defun cdr-list-len> (a b)
  (>= (length (flatten (cdr a))) (length (flatten (cdr b)))))

(defun cdr-list-len< (a b)
  (< (length (flatten (cdr a))) (length (flatten (cdr b)))))

(defun test-list> ()
  (let ((lst '((0 1 1 "B") (0 1 2 "A") (0 1 2 "C") (0 0 1 "C")))
      ;;(lst2 '((0) (1) (2) (0 1 5) (0 1 3) (0 1 5) (0 3 0) (0) (1) (2 7 19) (0 0 3 0)))
	 )
    (print (sort (copy-seq lst) #'list>))
    (show "")
    (show "list {}" lst) ;((0 1 1 B) (0 1 2 A) (0 1 2 C) (0 0 1 C))
    (sort lst #'list>) ;WARNING: DO NOT sort lst and normal lst directly
    (show "list {}" lst) ;((0 1 2 A) (0 1 1 B) (0 0 1 C))
    (select-best-items lst)
    (sort (copy-seq '((2) (1) (6 3) (2 (3) 4))) #'list>)
    ))
;;(test-list>)
;;(sort '(((0 0) (2 3)) ((0 0) (1 2)) ((0 0) (3 0))) #'(lambda (x y) (and (< (second (second x)) (second (second y))) (> (car (second x)) (car (second y))))))

(defun hash2weight-lst (dic)
  (let ((lst0 (hash2lst dic))
	 (lst-new '()))
    (dotimes (i (length lst0))
      (when (= 0 (mod i 2))
	(push (list (nth (+ 1 i) lst0) (nth i lst0)) lst-new)))
    lst-new))
;;(hash2weight-lst (>> 'ba 0.02 'bi 0.03 'bu 0.01))

(defun select-best-items (lst &key (type "max")); '((0.3 0.02 "ba") (0.1 0.02 "bi") (0.3 0.02 "bu"))
  (when (not (listp lst)) ;;when hash table -> convert it to list
    (setf lst (hash2weight-lst lst)))
  (if (equalp type "max")
    (setf ls (sort (copy-seq lst) #'list>)) ;(show "select-best-items sorted lst {}" ls)
    (setf ls (sort (copy-seq lst) #'list<)))
  (remove-if-not #'(lambda (x) (equalp (butlast (car ls)) (butlast x))) ls)  
  )
;;(select-best-items '((3 0.02 "ba") (0.1 0.02 "bi") (3.0 0.02 "bu")) :type "max")
;;(select-best-items '((3 0.02 "ba") (0.1 0.02 "bi") (3.0 0.02 "bu")) :type "min")
;;(select-best-items (>> 'ba 0.02 'bi 0.03 'bu 0.01) :type "max")

(defun weighted-random-choice (lst &key (reverse? nil))
  (when (not (listp lst)) ;;when >> table -> convert it to list
    (setf lst (hash2weight-lst lst)))
  (when reverse?
    (setf lst (mapcar (lambda (x) (append (list (/ 1 (car x))) (cdr x))) lst)))
  (let* ((max (reduce #'+ (mapcar #'(lambda (x) (car x)) lst)))
          (pick (random (* 1.0 max)))
          (current 0))
    (loop for l in lst do
      (setf current (+ current (car l)))
      (when (> current pick)
        (return-from weighted-random-choice (if reverse? (append (list (/ 1 (car l))) (cdr l)) l))
	;;(return-from weighted-random-choice l)
	))))
;;(mapcar #'(lambda (x) (weighted-random-choice '((0.2 2 b)(0.3 3 c)(0.1 1 a)))) (range 20))
;;(mapcar #'(lambda (x) (weighted-random-choice '((0.2 2 b)(0.3 3 c)(0.1 1 a)) :reverse? t)) (range 20))

(defun test-weighted-random-choice-lst ()
  (let (;;(lst '((0.1 0) (0.7 1) (0.2 2)))
	 ;;(lst '((1 0) (1 1) (1 2)))
	 (lst (>> '0 0.1 '1 0.7 '2 0.2))
	 (countlst '(0 0 0)))
    (dotimes (i 10000)
      (incf (nth (second (weighted-random-choice lst)) countlst)))
    (print countlst)))
;;(test-weighted-random-choice-lst)

(defun round-to (number precision &optional (what #'round));#'floor #'ceil #'truncate #'round 
  (let ((div (expt 10 precision)))
    (/ (funcall what (* number div)) div)))
(defun round-float (number precision &optional (what #'round))
  (float (round-to number precision what)))

(defun test-round-to ()
  (float (round-to 5.555555555 3)) ;5.556
  (round-float 5.555555555 3) ;5.556
  (round-to 1234.4567 0) ;1234
  (round-to 1234.4567 1) ;2469/2
  (float (round-to 1234.4567 1)) ;1234.5
  (round-to -1234.4567 2 #'floor) ;-61723/50
  (float (round-to -1234.4567 2 #'floor)) ;-1234.46
  (round-float -1234.4567 2 #'floor) ;-1234.46
  (round-to 1234.4567 3 #'truncate) ;154307/125
  (float (round-to 1234.4567 3 #'truncate)) ;1234.456
  )

(defun levenshtein-distance (a b)
  (let* ((la  (length a))
	 (lb  (length b))
	 (rec (make-array (list (1+ la) (1+ lb)) :initial-element nil)))
     (defun leven (x y)
      (cond
	((zerop x) y)
	((zerop y) x)
	((aref rec x y) (aref rec x y))
	(t (setf (aref rec x y)
		 (+ (if (char= (char a (- la x)) (char b (- lb y))) 0 1)
		    (min (leven (1- x) y)
			 (leven x (1- y))
			 (leven (1- x) (1- y))))))))
    (leven la lb)))

(defun test-levenshtein-distance ()
  (print (levenshtein-distance "rosettacode" "raisethysword"))
  (print (levenshtein-distance "rosettacode" "rosettacode"))
  )

(defun sequence-similarity (a b) ;A,B can be string, list or number
  (let ((score 0))
    (cond ((listp a)
	    (loop for i from 0 to (- (length a) 1) do
	      (if (stringp (nth i a))
		(setf score (- score (levenshtein-distance (nth i a) (nth i b))))
		(setf score (- score (abs (- (nth i a) (nth i b))))))))
      ((stringp a)
	(setf score (- score (levenshtein-distance a b))))
      ((numberp a)
	(setf score (- score (abs (- a b))))))
    score))

(defun test-sequence-similarity ()
  (show "{}" (sequence-similarity "rosettacode" "raisethysword"))
  (show "{}" (sequence-similarity (list "asd" "dfg" "gej") (list "aad" "dfh" "gej")))
  (show "{}" (sequence-similarity (list "aafadd" "dfh" "gej") (list "aad" "dfh" "gej")))
  (show "{}" (sequence-similarity 10 11.3)))
;;(test-sequence-similarity)

(defun test-intersection ()
  (setq list1 '(1 1 2 3 4 a b c "A" "B" "C" "d")
    list2 '(1 4 5 b c d "a" "B" "c" "D"))
  (intersection list1 list2) ;=>  (C B 4 1 1)
  (intersection list1 list2 :test 'equal) ;=>  ("B" C B 4 1 1)
  )
;;(intersection '(1 2 3) '(1 4 5))

#||
CL-USER> (defparameter lst '(3 2 1 5))
LST
CL-USER> lst
(3 2 1 5)
CL-USER> (sort lst #'<)
(1 2 3 5)
CL-USER> lst
(2 3 5)  ;;CAUTION 1 lost ;better using (sort (copy-seq lst) #'<) rather than (sort lst #'<)
CL-USER> (defparameter lst '(3 2 1 5))
LST
CL-USER> lst
(3 2 1 5)
CL-USER> (setf lst (sort lst #'<))
(1 2 3 5)
CL-USER> lst
(1 2 3 5)
||#

(defun str2num (str)
  (with-input-from-string (in str) (read in)))
;;(= (str2num "3.14") 3.14)

(defun test-until ()
  (loop with val = 0
    do (print (incf val)) (print "hello")
    until (= 0 (mod val 6)))
  )

(defun test-while ()
  (setq val 0)
  (loop do
    (incf val)
    (print val)
    while (/= 0 (mod val 6)))
  )
;(test-while)

(defun exagger-size ()
  (defparameter variance 0.15)
  (defparameter sizes (list 0.309 0.468 0.651))
  ;;(defparameter sizes (list 0.257 0.454 0.633))
  (defparameter sizes-min '())
  (defparameter sizes-max '())
  (defparameter very-variance (* 0.5 variance))
  (defparameter very-sizes '())
  (defparameter very-sizes-min '())
  (defparameter very-sizes-max '())
  (defparameter very-sizes-min2 '())
  (defparameter very-sizes-max2 '())
  (loop for s in sizes do
    ;(push (round-float (+ s (reduce #'+ (mapcar #'(lambda (x) (/ (- s x) 4)) sizes))) 3) very-sizes)
    (push (round-float (+ s (/ (reduce #'+ (mapcar #'(lambda (x) (- s x)) sizes)) 4)) 3) very-sizes)
    (setf sizes-min (mapcar #'(lambda (x) (round-float (- x variance) 3)) sizes))
    (setf sizes-max (mapcar #'(lambda (x) (round-float (+ x variance) 3)) sizes)))
  (loop for s in sizes-min do
    (push (round-float (+ s (reduce #'+ (mapcar #'(lambda (x) (/ (- s x) 4)) sizes-min))) 3) very-sizes-min))
  (loop for s in sizes-max do
    (push (round-float (+ s (reduce #'+ (mapcar #'(lambda (x) (/ (- s x) 4)) sizes-max))) 3) very-sizes-max))
  (loop for s in very-sizes do
    (setf very-sizes-min2 (mapcar #'(lambda (x) (round-float (- x very-variance) 3)) very-sizes))
    (setf very-sizes-max2 (mapcar #'(lambda (x) (round-float (+ x very-variance) 3)) very-sizes))
    )
  (print (reverse very-sizes))
  (print sizes-min)
  (print sizes-max)
  (print (reverse very-sizes-min))
  (print (reverse very-sizes-max))
  (print (reverse very-sizes-min2)) 
  (print (reverse very-sizes-max2)) 
  )
;;(exagger-size)

;;https://en.wikipedia.org/wiki/Sine_wave
;;http://baike.baidu.com/item/%E6%AD%A3%E5%BC%A6%E6%9B%B2%E7%BA%BF
(defun sine-curve (&key (x 1/10) (x1 4/5) (A 1/2) (w (* pi 1)) (p (/ pi -2)) (D 1/2))
  ;;x1 is x-value of wave top (prefered similarity); x is similarity
  (when (< x1 1/2) ;;to mirror
    (setf x (+ x (- 1 (* 2 x1))))  
    (setf x1 (- 1 x1)))
  (setf w (/ pi x1))
  (+ (* A (sin (+ (* w x) p))) D))

(defun test-sine-curve ()
  (with-session ()
    (command "set xrange [0:1]")
    (command "set yrange [0:1]")
    (command "set view map")
    (plot ()
      (data (mapcar #'(lambda (x) (list x (sine-curve :x x :x1 0.0))) (range 1 :step 0.01)) "title 'peak-pos 0.0'" "with lines" "lt rgb 'red' lw 1.2")
      (data (mapcar #'(lambda (x) (list x (sine-curve :x x :x1 0.2))) (range 1 :step 0.01)) "title '0.2'" "with lines" "lt rgb 'dark-pink' lw 1.2")
      (data (mapcar #'(lambda (x) (list x (sine-curve :x x :x1 0.4))) (range 1 :step 0.01)) "title '0.4'" "with lines" "lt rgb 'brown' lw 1.2")
      (data (mapcar #'(lambda (x) (list x (sine-curve :x x :x1 0.5))) (range 1 :step 0.01)) "title '0.5'" "with lines" "lt rgb 'black' lw 1.2")
      (data (mapcar #'(lambda (x) (list x (sine-curve :x x :x1 0.6))) (range 1 :step 0.01)) "title '0.6'" "with lines" "lt rgb 'sea-green' lw 1.2")
      (data (mapcar #'(lambda (x) (list x (sine-curve :x x :x1 0.8))) (range 1 :step 0.01)) "title '0.8'" "with lines" "lt rgb 'dark-violet' lw 1.2")
      (data (mapcar #'(lambda (x) (list x (sine-curve :x x :x1 1.0))) (range 1 :step 0.01)) "title '1.0'" "with lines" "lt rgb 'blue' lw 1.2")  ;lw 1.5 is line thickness
      )))
;;(test-sine-curve)

(defun wundt-curve (&key (novelty 0.7) (r-max 0.85) (p-max 0.95) (r-novel 0.2) (p-novel (+ r-novel 0.25)) (pr 2.5) (pp 2.5) (scale 10))
  ;;r-max is the max value of reward, p-max is the max value of punishment. p-max > r-max -> interesting value could be negative
  ;;r-novel and p-novel determine the peak-point, the distance between them determine the shape of peak: greater distance -> flat, small distance -> sharp
  ;;greater pr and pp decrease curve smoothness. smaller pr and pp flatten curve
  (let* ((novelty (* novelty scale))
	  (r-novel (* r-novel scale))
	  (p-novel (* p-novel scale))
	  (r0 (/ r-max (+ 1 (exp (* pr r-novel)))))
	  (p0 (/ p-max (+ 1 (exp (* pp p-novel)))))
	  (r (/ r-max (+ 1 (exp (- (* pr (- novelty r-novel)))))))
	  (p (/ p-max (+ 1 (exp (- (* pp (- novelty p-novel))))))))
    ;; regulate y(0) = 0
    (setf r (- r r0))
    (setf p (- p p0))
    (>> 'rp (- r p) 'r r 'p p)))
;;(show "{}" (wundt-curve :novelty 0))

(defun test-wundt-curve ()
  (let ((x-range 1.5))
    (with-session ()
      (command (format nil "set xrange [0:~a]" x-range))
      (command "set yrange [-1:1]")
      (command "set view map")
      (plot ()
	(data (mapcar #'(lambda (x) (list x (>> (wundt-curve :novelty x) 'rp))) (range x-range :step 0.01)) "title 'The Wundt Curve'" "with lines" "lt rgb 'black' lw 1.2") ;lw 1.2 is line thickness
	(data (mapcar #'(lambda (x) (list x (>> (wundt-curve :novelty x) 'r))) (range x-range :step 0.01)) "title 'reward'" "with lines" "lt rgb 'blue' lw 1.2")
	(data (mapcar #'(lambda (x) (list x (- (>> (wundt-curve :novelty x) 'p)))) (range x-range :step 0.01)) "title 'punish'" "with lines" "lt rgb 'red' lw 1.2")
	))))
;;(test-wundt-curve)

;; cumulative distribution function
;; http://www.johndcook.com/blog/python_phi/
(defun phi (x)
  (let ((a1  0.254829592)
	 (a2 -0.284496736)
	 (a3  1.421413741)
	 (a4 -1.453152027)
	 (a5  1.061405429)
	 (p   0.3275911  )
	 (sign (if (< x 0) -1 1))
	 )
    (setf x (/ (abs x) (sqrt 2.0)))
    (let* ((z (/ 1.0 (+ 1.0 (* p x))))
	    (y (- 1.0 (* (+ (* (+ (* (+ (* (+ (* a5 z) a4) z) a3) z) a2) z) a1) z (exp (* -1 x x))))))
      (* 0.5 (+ 1.0 (* sign y))))))
;;(phi 0)

(defun test-phi ()
  (with-session ()
    (command "set xrange [-4:4]")
    (command "set yrange [0:1]")
    (command "set view map")
    (plot ()
      (data (mapcar #'(lambda (x) (list x (phi x))) (range 4 :min -4 :step 0.01)) "title 's-cdf phi u:0.0 sigma:1'" "with lines" "lt rgb 'red' lw 1.2")
      )))
;;(test-phi)

;; probability density function of the standard normal distribution
(defun s-pdf (x)
  (/ (exp (* -0.5 x x)) (sqrt (* 2 pi))))

(defun test-s-pdf ()
  (with-session ()
    (command "set xrange [-4:4]")
    (command "set yrange [0:1]")
    (command "set view map")
    (plot ()
      (data (mapcar #'(lambda (x) (list x (s-pdf x))) (range 4 :min -4 :step 0.01)) "title 's-pdf u:0.0 sigma:1'" "with lines" "lt rgb 'red' lw 1.2")
      )))
;;(test-s-pdf)

;; probability density function of the truncated normal distribution
(defun t-pdf (x &key (u 0.0) (sigma 1.0) (a 0.0) (b 1.0))
  ;; less sigma, greater y(result)
  (/ (/ (s-pdf (/ (- x u) sigma)) sigma) (- (phi (/ (- b u) sigma)) (phi (/ (- a u) sigma)))))

(defun new-sigma (&key (u0 0.5) (sigma0 0.1) (u 0.125) (min 0.0) (max 1.0) (tolerance 0.001))
  (cond ((= u0 u)
	  sigma0)
    ((or (= 0 u) (= 1 u))
      (* 2 sigma0))
    (t
      (let* ((y (t-pdf u0 :u u0 :sigma sigma0))
	      (sigma (/ (+ min max) 2.0))
	      (y2 (funcall 't-pdf u :u u :sigma sigma)))
	(loop do
	  (if (> y2 y)
	    (setf min sigma)
	    (setf max sigma)
	    )
	  (setf sigma (/ (+ min max) 2.0))
	  (setf y2 (funcall 't-pdf u :u u :sigma sigma))
	  while (> (abs (- y2 y)) tolerance))
	sigma))))
;;(new-sigma)

(defun test-t-pdf ()
  (let* ((node 0.5)
	  (nodes-size (range 1.5 :step 0.5))
	  (nodes-size-size (range 1.125 :step 0.125))
	  (nodes-very-size (range 1.5 :step 0.5))
	  (sigma-size 0.15)
	  (sigma-size-size 0.05) ;0.05
	  (sigma-very-size 0.015)
	  (sigmas-size (mapcar #'(lambda (x) (new-sigma :u0 node :sigma0 sigma-size :u x)) nodes-size))
	  (sigmas-size-size (mapcar #'(lambda (x) (new-sigma :u0 node :sigma0 sigma-size-size :u x)) nodes-size-size))
	  (sigmas-very-size (mapcar #'(lambda (x) (new-sigma :u0 node :sigma0 sigma-very-size :u x)) nodes-very-size))
	  (names-size '(" S" " M" " L"))
	  (names-size-size '("SS" "MS" "LS" "SM" "MM" "LM" "SL" "ML" "LL")) ;0.05
	  (names-very-size '("VS" "VM" "VL"))
	  )
    (with-session ()
      (command "set xrange [0:1]")
      (command "set yrange [0:27]")
      (command "set view map")
      (plot* ;; a list of data 
	(let ((lst '()))
	  (dotimes (i 3)
	    (push (data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth i nodes-size) :sigma (nth i sigmas-size)))) (range 1.01 :step 0.01)) (format nil "title ' ~a u:~3$ sigma:~6$'"(nth i names-size) (nth i nodes-size) (nth i sigmas-size)) "with lines" "lt rgb 'red' lw 1.2") lst))
	  (dotimes (i 3)
	    (push (data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth i nodes-very-size) :sigma (nth i sigmas-very-size)))) (range 1.01 :step 0.001)) (format nil "title '~a u:~3$ sigma:~6$'"(nth i names-very-size) (nth i nodes-very-size) (nth i sigmas-very-size)) "with lines" "lt rgb 'blue' lw 1.2") lst))
	  (dotimes (i 9)
	    (push (data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth i nodes-size-size) :sigma (nth i sigmas-size-size)))) (range 1.01 :step 0.001)) (format nil "title '~a u:~3$ sigma:~6$'"(nth i names-size-size) (nth i nodes-size-size) (nth i sigmas-size-size)) "with lines" "lt rgb 'green' lw 1.2") lst))
	  (reverse lst))      
	))))
;;(test-t-pdf)

(defun exagger-node (node nodes &key (n 4))
  (round-float (+ node (/ (reduce #'+ (mapcar #'(lambda (x) (- node x)) nodes)) n)) 5))
;;(exagger-node 0 '(0 0.1 1)) --> -0.275
;;(exagger-node 0 '(0 0.5 1)) --> -0.375
;;(exagger-node 0.2 '(0 0.2 1) :n 4)

(defun test-t-pdf-dynamic ()
  (let* ((node 0.5)
	  (nodes-size '(0.309 0.468 0.651)) ;(0.257 0.454 0.633)
	  (nodes-very-size (mapcar #'(lambda (x) (min 1 (max 0 (exagger-node x nodes-size :n 4)))) nodes-size)) ;:n 4
	  (nodes-size-size (range 1.125 :step 0.125))
	  (sigma-size 0.15) ;0.15
	  (sigma-size-size 0.05) ;0.05
	  (sigma-very-size 0.015) ;0.015
	  (sigmas-size (mapcar #'(lambda (x) (new-sigma :u0 node :sigma0 sigma-size :u x)) nodes-size))
	  (sigmas-very-size (mapcar #'(lambda (x) (new-sigma :u0 node :sigma0 sigma-very-size :u x)) nodes-very-size))
	  (sigmas-size-size (mapcar #'(lambda (x) (new-sigma :u0 node :sigma0 sigma-size-size :u x)) nodes-size-size))
	  (names-size '(" S" " M" " L"))
	  (names-size-size '("SS" "MS" "LS" "SM" "MM" "LM" "SL" "ML" "LL")) 
	  (names-very-size '("VS" "VM" "VL"))
	  )
    ;; size-size
    (setf (nth 1 nodes-size-size) (min 1 (max 0 (exagger-node (nth 0 nodes-size) nodes-size :n 20)))) ;:n 20
    (setf (nth 4 nodes-size-size) (min 1 (max 0 (exagger-node (nth 1 nodes-size) nodes-size :n 4))))
    (setf (nth 7 nodes-size-size) (min 1 (max 0 (exagger-node (nth 2 nodes-size) nodes-size :n 20))))
    (dotimes (i 2)
      (let ((distan (/ (- (nth 4 nodes-size-size) (nth 1 nodes-size-size)) 3)))
	(setf (nth (+ 2 i) nodes-size-size) (+ (* (+ 1 i) distan) (nth 1 nodes-size-size)))
	(setf (nth 0 nodes-size-size) (- (nth 1 nodes-size-size) distan)))
      (let ((distan2 (/ (- (nth 7 nodes-size-size) (nth 4 nodes-size-size)) 3)))
	(setf (nth (+ 5 i) nodes-size-size) (+ (* (+ 1 i) distan2) (nth 4 nodes-size-size)))
	(setf (nth 8 nodes-size-size) (+ distan2 (nth 7 nodes-size-size)))))
    (setf sigmas-size-size (mapcar #'(lambda (x) (new-sigma :u0 node :sigma0 sigma-size-size :u x)) nodes-size-size))
    ;; very-size
    (when (< (car sigmas-very-size) (car nodes-very-size)) (setf (car sigmas-very-size) (car nodes-very-size)))
    (when (< (third sigmas-very-size) (- 1 (third nodes-very-size))) (setf (third sigmas-very-size) (- 1 (third nodes-very-size))))
    
    (with-session ()
      (command "set xrange [0:1]")
      (command "set yrange [0:27]")
      (command "set view map")
      (plot* ;; a list of data 
	(let ((lst '()))
	  (dotimes (i 3)
	    (push (data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth i nodes-size) :sigma (nth i sigmas-size)))) (range 1.01 :step 0.01)) (format nil "title ' ~a u:~3$ sigma:~6$'"(nth i names-size) (nth i nodes-size) (nth i sigmas-size)) "with lines" "lt rgb 'red' lw 1.2") lst))
	  (dotimes (i 3)
	    (push (data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth i nodes-very-size) :sigma (nth i sigmas-very-size)))) (range 1.01 :step 0.001)) (format nil "title '~a u:~3$ sigma:~6$'"(nth i names-very-size) (nth i nodes-very-size) (nth i sigmas-very-size)) "with lines" "lt rgb 'blue' lw 1.2") lst))
	  (dotimes (i 9)
	    (push (data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth i nodes-size-size) :sigma (nth i sigmas-size-size)))) (range 1.01 :step 0.001)) (format nil "title '~a u:~3$ sigma:~6$'"(nth i names-size-size) (nth i nodes-size-size) (nth i sigmas-size-size)) "with lines" "lt rgb 'green' lw 1.2") lst))
	  (reverse lst))      
	))))
;;(test-t-pdf-dynamic)

(defun test-t-pdf2 ()
  (let* ((node 0.5)
	  (nodes-size (range 1.5 :step 0.5))
	  (nodes-size-size (range 1.125 :step 0.125))
	  (nodes-very-size (range 1.5 :step 0.5))
	  (sigma-size 0.1)
	  (sigma-size-size 0.05) ;0.05
	  (sigma-very-size 0.015)
	  (sigmas-size (mapcar #'(lambda (x) (new-sigma :u0 node :sigma0 sigma-size :u x)) nodes-size))
	  (sigmas-size-size (mapcar #'(lambda (x) (new-sigma :u0 node :sigma0 sigma-size-size :u x)) nodes-size-size))
	  (sigmas-very-size (mapcar #'(lambda (x) (new-sigma :u0 node :sigma0 sigma-very-size :u x)) nodes-very-size))
	  (names-size '(" S" " M" " L"))
	  (names-size-size '("SS" "MS" "LS" "SM" "MM" "LM" "SL" "ML" "LL")) ;0.05
	  (names-very-size '("VS" "VM" "VL"))
	  )
    (with-session ()
      (command "set xrange [0:1]")
      (command "set yrange [0:27]")
      (command "set view map")
      (plot ()
	;;S :u 0.0 :sigma 0.2
	(data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth 0 nodes-size) :sigma (nth 0 sigmas-size)))) (range 1.01 :step 0.01)) (format nil "title '  S u:~3$ sigma:~6$'" (nth 0 nodes-size) (nth 0 sigmas-size)) "with lines" "lt rgb 'red' lw 1.2")      
	;;M :u 0.5 :sigma 0.2
	(data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth 1 nodes-size) :sigma (nth 1 sigmas-size)))) (range 1.01 :step 0.01)) (format nil "title '  M u:~3$ sigma:~6$'" (nth 1 nodes-size) (nth 1 sigmas-size)) "with lines" "lt rgb 'red' lw 1.2")
	;;L :u 1.0 :sigma 0.2
	(data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth 2 nodes-size) :sigma (nth 2 sigmas-size)))) (range 1.01 :step 0.01)) (format nil "title '  L u:~3$ sigma:~6$'" (nth 2 nodes-size) (nth 2 sigmas-size)) "with lines" "lt rgb 'red' lw 1.2")
	;;VS :u 0.0 :sigma 0.03
	(data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth 0 nodes-very-size) :sigma (nth 0 sigmas-very-size)))) (range 1.01 :step 0.001)) (format nil "title ' VS u:~3$ sigma:~6$'" (nth 0 nodes-very-size) (nth 0 sigmas-very-size)) "with lines" "lt rgb 'blue' lw 1.2")
	;;VM :u 0.5 :sigma 0.03
	(data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth 1 nodes-very-size) :sigma (nth 1 sigmas-very-size)))) (range 1.01 :step 0.001)) (format nil "title ' VM u:~3$ sigma:~6$'" (nth 1 nodes-very-size) (nth 1 sigmas-very-size)) "with lines" "lt rgb 'blue' lw 1.2")
	;;VL :u 1.0 :sigma 0.03
	(data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth 2 nodes-very-size) :sigma (nth 2 sigmas-very-size)))) (range 1.01 :step 0.001)) (format nil "title ' VL u:~3$ sigma:~6$'" (nth 2 nodes-very-size) (nth 2 sigmas-very-size)) "with lines" "lt rgb 'blue' lw 1.2")
	;;SS-line :u 0.0 :sigma 0.1
	;;(data (mapcar #'(lambda (x) (list x (t-pdf 0 :u (nth 0 nodes-size-size) :sigma (nth 0 sigmas-size-size)))) (range 1.01 :step 1)) (format nil "title ' SS u:~3$ sigma:~6$'" (nth 0 nodes-size-size) (nth 0 sigmas-size-size)) "with lines" "lt rgb 'black' lw 1")
	;;SS :u 0.0 :sigma 0.1
	(data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth 0 nodes-size-size) :sigma (nth 0 sigmas-size-size)))) (range 1.01 :step 0.001)) (format nil "title ' SS u:~3$ sigma:~6$'" (nth 0 nodes-size-size) (nth 0 sigmas-size-size)) "with lines" "lt rgb 'green' lw 1.2")
	;;MM :u 0.5 :sigma 0.1
	(data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth 4 nodes-size-size) :sigma (nth 4 sigmas-size-size)))) (range 1.01 :step 0.001)) (format nil "title ' MM u:~3$ sigma:~6$'" (nth 4 nodes-size-size) (nth 4 sigmas-size-size)) "with lines" "lt rgb 'green' lw 1.2")
	;;LL :u 1.0 :sigma 0.1
	(data (mapcar #'(lambda (x) (list x (t-pdf x :u (nth 8 nodes-size-size) :sigma (nth 8 sigmas-size-size)))) (range 1.01 :step 0.001)) (format nil "title ' LL u:~3$ sigma:~6$'" (nth 8 nodes-size-size) (nth 8 sigmas-size-size)) "with lines" "lt rgb 'green' lw 1.2")))))
;;(test-t-pdf2)

(defun plot-animation ()
    (with-session ()
      ;;(command "set xrange [0:1]")
      ;;(command "set yrange [0:27]")
      ;;(command "set view map")
      (dotimes (i 200)
	(plot () (data
		   (mapcar #'(lambda (x) (list x (sin (/ x 10.0)))) (range (+ 1 i) :step 1))
		   ;;(mapcar #'(lambda (x) (list x (expt x 2))) (range (+ 1 i) :step 1))
		   (format nil "title '~a time:~3$'" "function" i ) "with lines" "lt rgb 'green' lw 1.2")))))
;;(plot-animation)

;;(mapcar #'(lambda (x y) (list x y)) '(a b c) '(1 2 3)) ;result: ((A 1) (B 2) (C 3))
;;(>> 'a '(1 2) 'b 2 (intern (apply #'concatenate 'string '("V" "S"))) '(3 4))

(defun random-word ()
  (let ((consonants "bcdfghjklmnpqrstvwxyz")
	 (vowels "aeiou")
	 (len (+ 1 (random 2))) ; 1 or 2 
	 (word ""))
    (dotimes (i len)
      (setf word (concatenate 'string word (concatenate 'string (random-choice consonants) (random-choice vowels)))))
    word))
;;(random-word)

;; (consonants "bcdfghjklmnpqrstvwxyz") (vowels "aeiou")
(defun random-utter (&key (c0 "bcdfghjklmnpqrstvwxyz") (c1 "aeiou") (length 2))
  (let ((utter '()))
    (dotimes (i length)
      (if (= (mod i 2) 0)
	(push (aref c0 (random (length c0))) utter)
	(if c1
	  (push (aref c1 (random (length c1))) utter)
	  (push (aref c0 (random (length c0))) utter))))
    (coerce (reverse utter) 'string)))
;;(random-utter)
;;(random-utter :length 3)
;;(random-utter :c1 nil :length 3)

;;(loop for i from 1 to 100 collect (1+ (* (sqrt (* -2 (log (random 1.0)))) (cos (* 2 pi (random 1.0))) 0.5)))

(defun normal-random (&key (mean 0.5) (deviation 0.5))
  ;;s-random: mean = 1.0 standard-deviation = 0.5
  (let ((s-random (1+ (* (sqrt (* -2 (log (random 1.0)))) (cos (* 2 pi (random 1.0))) 0.5)))) 
    (- (+ 1 (* (- s-random 1) (/ deviation 0.5))) (- 1 mean))))

(defun test-normal-random ()
  (let* ((u 0) ;mean
	  (devi 0.5) ;deviation
	  (total 10000) ;the amount of total testing data
	  (data (mapcar #'(lambda (x) (normal-random :mean u :deviation devi)) (range total)))
	  (number0 0) ; the amount of values less than one standard deviation away from the mean 
	  (number1 0) ; the amount of values less than two standard deviation away from the mean 
	  (number2 0)); the amount of values less than three standard deviation away from the mean 
    (loop for n in data do
      (when (and (< n (+ u devi)) (> n (- u devi)))
	(incf number0)))
    (loop for n in data do
      (when (and (< n (+ u (* 2 devi))) (> n (- u (* 2 devi))))
	(incf number1)))
    (loop for n in data do
      (when (and (< n (+ u (* 3 devi))) (> n (- u (* 3 devi))))
	(incf number2)))
    (show "")
    (show "rate in one standard deviation: {}" (/ (* 1.0 number0) total))    ;68 %
    (show "rate in two standard deviations: {}" (/ (* 1.0 number1) total))   ;95 %
    (show "rate in three standard deviations: {}" (/ (* 1.0 number2) total)) ;99.7 %
    (with-session ()
      (plot ()
	(data (mapcar #'(lambda (x) (list x (nth x data))) (range total :step 1)) )
	))))
;;(test-normal-random)

(defun iota (n) (loop for i from 1 to n collect i))       ;helper
(destructuring-bind ((a &optional (b 'bee)) one two three)
  `((alpha) ,@(iota 3))
  (list a b three two one)) ;=>  (ALPHA BEE 3 2 1)

(defun pingpong (&key (n (>> 'value 0 'direction '+)) (min 0) (max 1) (step 0.1))
  (cond
    ((and (< (>> n 'value) max) (equal (>> n 'direction) '+)) ;increase
      (let ((value0 (>> n 'value)))
	(>> n 'value (+ value0 step))))
    ((>= (>> n 'value) max) ;reach max
      (progn
	(>> n 'direction '-)
	(>> n 'value (- max step))))
    ((and (> (>> n 'value) min) (equal (>> n 'direction) '-)) ;decrease
      (let ((value0 (>> n 'value)))
	(>> n 'value (- value0 step))))
    (t ;reach min
      (progn
	(>> n 'direction '+)
	(>> n 'value (+ min step))))
    )
  n)

;;(defun test-matrix ()
;;  (lisp-matrix:M*
;;    (lisp-matrix:ones 2 2 :implementation :foreign-array)
;;    (lisp-matrix:ones 2 2 :implementation :foreign-array))
;;
;;  (lisp-matrix:M*
;;    (lisp-matrix:ones 2 2 :implementation :lisp-array)
;;    (lisp-matrix:ones 2 2 :implementation :lisp-array))
;;
;;  (lisp-matrix:M*
;;    (lisp-matrix:ones 2 2 :implementation :lisp-array)
;;    (lisp-matrix:ones 2 2 :implementation :foreign-array))
;;  )

(defun scale-to-01 (&key (x 1) (n 100))
  (let ((result 0))
    (setf x (min n (max (/ 1 n) x))) ;;ensure [1/n, n] n>1 center: 1
    (if (>= x 1)
      (setf result (+ 0.5 (/ (* 0.5 (- x 1)) (- n 1))))
      (progn
	(setf x (/ 1 x))
	(setf result (- 0.5 (/ (* 0.5 (- x 1)) (- n 1))))))
    (min 1 (max 0 result)) ;;ensure [0, 1] center: 0.5
    ))
;;(scale-to-01) ;0.5

(defun scale-to-n (&key (x 0.5) (n 100))
  (let ((result 0))
    (setf x (min 1 (max 0 x))) ;;ensure [0, 1] center: 0.5
    (if (>= x 0.5)
      (setf result (+ 1 (* (- n 1) (- (* 2 x) 1))))
      (progn
	(setf x (- 1 x))
	(setf result (/ 1 (+ 1 (* (- n 1) (- (* 2 x) 1)))))))
    (min n (max (/ 1 n) result)) ;;ensure [1/n, n] n>1 center: 1
    ))
;;(scale-to-n) ;1

(defun scale-plot ()
  (let* ((step-sample 0.1)
	  (n 5)
	  (larger (range (+ n (/ step-sample 2)) :min (+ 1 step-sample) :step step-sample))
	  (smaller (reverse (mapcar #'(lambda (x) (/ 1 x)) larger)))
	  (samples (append smaller (list 1) larger))
	  (rangex 1)
	  (ys (range (+ rangex 0.000001) :min 0 :step (/ rangex (- (length samples) 1))))
	  (xys (mapcar #'(lambda (x y) (list x y)) samples ys))
	  (xys2 (mapcar #'(lambda (x) (list x (scale-to-01 :x x :n n))) samples))
	  (yxs (mapcar #'(lambda (x y) (list x y)) ys samples))
	  (yxs2 (mapcar #'(lambda (x) (list x (scale-to-n :x x :n n))) ys))	  
	  )
    (with-session ()
      (plot ()
	(data xys (format nil "title ' ~a'" "xys") "with lines" "lt rgb 'red' lw 1.2")
	(data xys2 (format nil "title ' ~a'" "xys2") "with lines" "lt rgb 'blue' lw 1.2")
	(data yxs (format nil "title ' ~a'" "yxs") "with lines" "lt rgb 'brown' lw 1.2")
	(data yxs2 (format nil "title ' ~a'" "yxs2") "with lines" "lt rgb 'green' lw 1.2")
	))
    (print xys)
    (print xys2)
    (print yxs)
    (print yxs2)
    ))
;;(scale-plot)

#| the algorithm of combine
(((A C) (B C)) + (D E F) (G)) -->
(((A C D) (A C E) (A C F) (B C D) (B C E) (B C F)) + (G)) -->
(((A C D G) (A C E G) (A C F G) (B C D G) (B C E G) (B C F G))) 
|#
(defun combine (&key (lst '((a b) (c) (d e f) (g))))
  (let* ((n (length lst)))
    (loop do
      (let ((ls0 (car lst))
	     (ls1 (second lst))
	     (temp '()))
	(setf lst (cddr lst))
	(dolist (l0 ls0)
	  (dolist (l1 ls1)
	    (if (listp l0)
	      (push (append l0 (list l1)) temp)
	      (push (list l0 l1) temp))))
	(setf temp (reverse temp))
	(push temp lst)) ;;(show "lst: {}" lst)
      (setf n (length lst))
      while (> n 1))
    (car lst)))
;;(combine)
;;(print (combine :lst '((!sun !cloud !land !bird !boat !fish) (!in !on) (!sky !sea))))

;;Euclidean Squared distance does not take the square root as Euclidean distance.
(defun squared-distance (&key (v0 '(3 5)) (v1 '(1 2)))
  (reduce #'+ (mapcar #'(lambda (x y) (expt (- x y) 2)) v0 v1))
  )
;;(squared-distance :v0 '(1 2 3) :v1 '(2 3 5.2))

(defun flatten (lst)
  (cond ((null lst) nil)
    ((atom lst) (list lst))
    (t (loop for a in lst appending (flatten a)))))
;;(flatten '(a b c (d (h h k) e) ((f) g)))

(defun list2set (lst)
  (let ((set '()))
    (dolist (x lst)
      (when (not (member x set :test 'equal))
	(push x set)))
    (reverse set)))
;;(list2set '(= H Y))

(defun list2hash (lst)
  (alexandria:plist-hash-table lst))
;;(show "{}" (list2hash (list 'x (list 'x0 12 'x1 13) 'y 0)))

;; r,g,b values are from 0 to 1
;; h = [0,360], s = [0,1], v = [0,1]
;;		if s == 0, then h = -1 (undefined)
(defun rgb2hsv (&key (rgb '(0 0 0)) (rgb-range-max 1) (h-range-max 360) (sv-range-max 1))
  (when (/= rgb-range-max 1)
    (setf rgb (mapcar (lambda (x) (float (/ x rgb-range-max))) rgb)))
  (let* ((min (apply 'min rgb))
	  (max (apply 'max rgb))
	  (h nil) (s nil) (v max)
	  (delta (- max min))
	  (r (car rgb))
	  (g (second rgb))
	  (b (third rgb)))
    (when (= 0 delta) ;;r = g = b = 1 white --> s = 0, v = 1; r = g = b = 0 black --> s = 0, v = 0
      (setf h 0) ;;(setf h -1)
      (setf s 0)      
      (when (/= sv-range-max 1)
	(setf s (* s sv-range-max))
	(setf v (* v sv-range-max)))
      (return-from rgb2hsv (list h s (float v))))    
    (setf s (/ delta max))
    (let ((r (car rgb)) (g (second rgb)) (b (third rgb)))
      (cond
	((= r max) (setf h (/ (- g b) delta))) ;between yellow & magenta
	((= g max) (setf h (+ 2 (/ (- b r) delta)))) ;between cyan & yellow
	(t (setf h (+ 4 (/ (- r g) delta)))))) ;between magenta & cyan
    (setf h (* h 60)) ;degrees
    (when (< h 0) (setf h (+ h 360)))
    (when (/= h-range-max 360)
      (setf h (* h (/ h-range-max 360))))
    (when (/= sv-range-max 1)
      (setf s (* s sv-range-max))
      (setf v (* v sv-range-max)))
    (mapcar (lambda (x) (float x)) (list h s v))))
;;(rgb2hsv :rgb '(200/255 200/255 200/255) :rgb-range-max 1 :h-range-max 360 :sv-range-max 100)

(defun hsv2rgb (&key (hsv '(0 0 0)) (h-range-max 360) (sv-range-max 1) (rgb-range-max 1))
  (let ((h (car hsv)) (s (second hsv)) (v (third hsv)))
    (when (/= h-range-max 360)
      (setf h (* h (/ 360 h-range-max))))
    (when (/= sv-range-max 1)
      (setf s (/ s sv-range-max))
      (setf v (/ v sv-range-max)))
    (when (= 0 s)
      (let ((color v))
	(when (/= rgb-range-max 1)
	  (setf color (* color rgb-range-max)))
	(return-from hsv2rgb (list color color color)))) ;achromatic (grey)
    (setf h (/ h 60.0)) ;sector 0 to 5
    (let* ((i (floor h))
	    (f (- h i))
	    (p (* v (- 1 s)))
	    (q (* v (- 1 (* s f))))
	    (e (* v (- 1 (* s (- 1 f)))))
	    (r nil)(g nil)(b nil))
      (case i
	(0 (setf r v) (setf g e) (setf b p))
	(1 (setf r q) (setf g v) (setf b p))
	(2 (setf r p) (setf g v) (setf b e))
	(3 (setf r p) (setf g q) (setf b v))
	(4 (setf r e) (setf g p) (setf b v))
	(otherwise (setf r v) (setf g p) (setf b q))) ;case 5
      (let ((rgb (list r g b)))
	(when (/= rgb-range-max 1)
	  (setf rgb (mapcar (lambda (x) (* x rgb-range-max)) rgb)))
	rgb))))
;;(hsv2rgb :hsv '(0.5 1 1) :h-range-max 1 :sv-range-max 1 :rgb-range-max 1)

(defun generate-bitree (&key (operators '(< = > - + and or)) (variables '(xo yo ro rc rs rt h)))
  (if (= 0 (random-choice (range 5)))
    (list (random-choice operators) (generate-bitree) (generate-bitree))
    (list (random-choice operators) (random-choice variables) (random-choice variables))
  ))
;;(show "generate-bitree: {}" (generate-bitree)) ;eg. (OR (+ YO RC) (= (< XO RS) (- H YO)))

(defun remove-one (item sequence)
  (let ((switch t)
	 (new-sequence '()))
    (dotimes (i (length sequence))
      (if (and switch (equalp item (nth i sequence)))
	(setf switch nil)
	(push (nth i sequence) new-sequence)))
    (reverse new-sequence)))
;;(remove-one 1 '(1 1 1 2))

(defun simplify-equal (expression)
  (let ((arguments (list2set (cdr expression))))
    (if (= (length arguments) 1)
      t
      (append (list (car expression)) (mapcar #'simplify0 arguments)))))

(defun simplify-addition (expression)
  (let ((result (remove-if #'(lambda (x) (equalp 0 x)) expression)))
    (cond
      ((= (length result) 1) 0)
      ((= (length result) 2) (simplify0 (second result)))
      (t (append '(+) (mapcar #'simplify0 (cdr result))))
      )))

(defun simplify-subtraction (expression)
  (let ((remaining (remove-if #'(lambda (x) (equalp 0 x)) (cddr expression))))
    (if remaining
      (if (equalp (list (second expression)) remaining)
	0
	(if (member (second expression) remaining :test 'equal)
	  (append '(- 0) (mapcar #'simplify0 (remove-one (second expression) remaining)))
	  (append '(-) (mapcar #'simplify0 (append (list (second expression)) remaining)))))
      (simplify0 (second expression)))))

(defun simplify-multiplication (expression)
  (if (member 0 expression :test 'equal)
    0
    (progn
      (setf expression (remove-if #'(lambda (x) (equalp 1 x)) expression))
      (cond
	((= (length expression) 1) 1)
	((= (length expression) 2) (second expression))
	(t (append '(*) (mapcar #'simplify0 (cdr expression))))))))

(defun simplify-division (expression)
  (if (equalp 0 (second expression))
    0
    (let ((remaining (remove-if #'(lambda (x) (equalp 1 x)) (cddr expression))))
      (if remaining
	(if (equalp (list (second expression)) remaining)
	  1
	  (append '(/) (mapcar #'simplify0 (append (list (second expression)) remaining))))
	(simplify0 (second expression))))))

(defun simplify-and (expression)
  (let ((remaining (remove-if #'(lambda (x) (or (atom x) (member (car x) '(- + * /) :test 'equal))) (list2set (cdr expression)))))
    (if remaining
      (cond
	((= 1 (length remaining)) (simplify0 (car remaining)))
	(t (append '(and) (mapcar #'simplify0 remaining))))
      '())))

(defun simplify0 (expression)
  (if (atom expression)
    expression
    (case (car expression)
      (= (simplify-equal expression))
      (- (simplify-subtraction expression))
      (+ (simplify-addition expression))
      (* (simplify-multiplication expression))
      (/ (simplify-division expression))
      (and (simplify-and expression))
      (otherwise (mapcar #'simplify0 expression))
      )))

(defun simplify (expression)
  (let ((expression0 (copy-seq expression)))
    (setf expression (simplify0 expression))
    (if (or (not (listp expression)) (equalp expression0 expression))
      expression
      (simplify expression))))

(defun intersect  (A B)
  (if (eq A ())
    A
    (if (member (car A) B)
      (cons (car A) (intersect (cdr A) B))
      (intersect (cdr A) B))))
;;(intersect '(1 2 3 4) '(0 1 2))

;;(show "{}" (simplify0 (simplify0 '(- 1 (/ 5 5) 6 (/ 1 0 1) 6))))
;;(show "{}" (simplify '(- 1 (/ 5 5) 6 (and (> x 3) (= y y) -23) (/ 1 0 1) 6)))
;;(simplify '(- 1 1))
;;(simplify '(> (+ R R) q (* 1 q w 1)))
;;(c2l (list (simplify (car (l2c '([ > [ - R R ] [ AND [ > Y R ] H ] ]))))))
;;(c2l (list (simplify (car (l2c '([ = H Y ]))))))
;;(simplify '(= H Y))
;;'(< (- Y R) H (+ Y R))
;;(simplify '(and))

;;;; lispbuilder and opengl -----------------------------------------------------------------

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
     (progn ,@body)
     (continue () :report "Continue")))

#|
(burgled-batteries:startup-python)
(burgled-batteries:run "1+1")
(py:run "import numpy as np")
(py:run "import scipy as sp")
(py:run "np.fft.fft(np.array([1, 2, 3]))")
(py:run "from shapely.geometry import Point")
(py:run "patch = Point(0.0, 0.0).buffer(10.0)")
(py:run "patch.area")
(py:run "np.arange(15).reshape(3,5)")
(burgled-batteries:shutdown-python)
|#

;;;; test graph-db ------***CAUTION: memory overflow!!!***-----------------------------------------------------------
#|
(progn
(defparameter *graph-name* :test-graph)
(defparameter *graph-path* "/home/an/test-graph/")
;;(defparameter *graph* (graph-db:make-graph *graph-name* *graph-path*)) (print *graph*) ;CAUTION: memory overflow!!!
)
|#

;;;; test cl-graph ---------------------------------------------------------------------------------------
#|
(let ((g (make-container 'graph-container :default-edge-type :directed)))  
  (loop for (a b) in '((a b) (b c) (b d) (d e) (e f) (d f) (g)) do  
    (add-edge-between-vertexes g a b))  
  (graph->dot g t)
  (print (vertexes g))
  (print (edges g))
  )
|# 

#|
"digraph G {  
E []  
C []  
B []  
A []  
D []  
F []  
E->F []  
B->C []  
B->D []  
A->B []  
D->E []  
D->F []  
}" 
|# 
 
;;(cl-geometry:bentley-ottmann '((0 0 1 1)(1 1 3 4)(3 4 7 2)))

(defun insert (lst index new-item)
  (push new-item (cdr (nthcdr index lst))))
;;(let ((lst '(a c d))) (insert lst 0 'b) lst)   => (A B C D)

(defun pushl (new-item lst)
  (insert lst (- (length lst) 1) new-item))
;;(let ((lst '(a c d))) (pushl 'b lst) lst)   => (A C D B)

(defun norm (&optional (lst '(1 2 3)))
  (sqrt (apply #'+ (mapcar (lambda (x) (* x x)) lst))))
;;(norm '(1 2 3))

(defun normalize (&optional (lst '(1 2 3)))
  (let ((nrm (norm lst)))
    (when (> nrm 0)
      (setf lst (mapcar (lambda (x) (/ x nrm)) lst))))
  lst)
;;(normalize '(1 2 3)) -> (0.26726124 0.5345225 0.8017837)

(defun randomize (&optional (n 1024))
  (let* ((sd (/ 1 n))
	  (v (mapcar (lambda (x) (* (normal-random :mean 0 :deviation 1) sd)) (range n))))  
    (normalize v)))
;;(randomize 8)

(defun convolve-correlate0 (&key (lst0 (randomize)) (lst1 (randomize)) (type 'convolve))
  (let ((x lst0)
	 (y lst1)
	 (number (length lst0))
	 (new-lst '())
	 (sublst '()))
    (dotimes (i number)
      (setf sublst '())
      (dotimes (k number)
	(if (equal type 'convolve)
	  (setf sublst (append sublst (list (* (nth k x) (nth (mod (+ (- i k) number) number) y))))) ;;z[i] += x[k]*y[(i-k+self.number)%self.number]
	  (setf sublst (append sublst (list (* (nth k x) (nth (mod (+ i k) number) y))))))) ;;z[i] += x[k]*y[(i+k)%self.number]
      (setf new-lst (append new-lst (list (apply #'+ sublst)))))
    new-lst))

(defun convolve0 (&optional (lst0 (randomize)) (lst1 (randomize)))
  (convolve-correlate0 :lst0 lst0 :lst1 lst1 :type 'convolve0))

(defun convolve (&optional (lst0 (randomize)) (lst1 (randomize)))
  (let ((vector0 (make-array (list (length lst0)) :initial-contents lst0))
	 (vector1 (make-array (list (length lst1)) :initial-contents lst1)))
    (mapcar (lambda (x) (realpart x)) (array-to-list (fft:ifft (list-to-array (mapcar (lambda (x y) (* x y)) (array-to-list (fft:fft vector0)) (array-to-list (fft:fft vector1)))))))))
;;(show "convolve0 {}" (convolve0 (randomize 32) (randomize 32)))
;;(show "convolve {}" (convolve (randomize 32) (randomize 32)))
;;(* (fft:fft (randomize 8)) (fft:fft (randomize 8)))
;;(mapcar (lambda (x y) (* x y)) (array-to-list #(1 2 3)) (array-to-list #(3 4 8)))
;;(mapcar (lambda (x) (realpart x)) (array-to-list (fft:ifft (list-to-array (mapcar (lambda (x y) (* x y)) (array-to-list (fft:fft (randomize 8))) (array-to-list (fft:fft (randomize 8))))))))

(defun add0 (&optional (lst0 (randomize)) (lst1 (randomize)))
  ;;(mapcar (lambda (x y) (+ x y)) lst0 lst1)
  (mapcar #'+ lst0 lst1))

(defun add (&rest lsts)
  (cond
    ((< (length lsts) 2) (car lsts))
    ((= (length lsts) 2) (add0 (car lsts) (second lsts)))
    (t (let ((result (car lsts)))
	 (dolist (lst (cdr lsts))
	   (setf result (add0 result lst)))
	 result))))
;;(add)

(defun subtract0 (&optional (lst0 (randomize)) (lst1 (randomize)))
  (mapcar #'- lst0 lst1))

(defun subtract (&rest lsts)
  (cond
    ((< (length lsts) 2) (car lsts))
    ((= (length lsts) 2) (subtract0 (car lsts) (second lsts)))
    (t (let ((result (car lsts)))
	 (dolist (lst (cdr lsts))
	   (setf result (subtract0 result lst)))
	 result))))

(defun interleave00 (&optional (lsts (list (randomize) (randomize))) (weights '()))
  (let ((lst '()) (index nil))
    (if (null weights)
      (setf weights (mapcar (lambda (x) (list 1 x)) (range (length lsts))))
      (setf weights (mapcar (lambda (x y) (list x y)) weights (range (length weights)))))
    (dotimes (i (length (car lsts)))
      (setf index (second (weighted-random-choice weights)))
      (setf lst (append lst (list (nth i (nth index lsts)))))
      )
    lst))
;;(interleave00 '((1 2 3 4) (a b c d) (! @ $ %)))

;; simple interleaving algorithm
;;(mapcar (lambda (x y) (random-choice (list x y))) '(1 2 3 4) '(a b c d))

(defun interleave0 (&key (lsts (list (randomize) (randomize))) (weight? nil) (order? nil))
  ;;if weight?, the first one of each HRR-lst is weight
  (let ((final-weight nil))
    (when (numberp (car lsts));;the first one is the weight of the future interleaved HRR-lsts 
      (setf final-weight (car lsts))
      (setf lsts (cdr lsts)))
    (let ((lst '()) (indexes (range (length lsts))) (index nil))
      (cond
	(weight?
	  (let ((weights (mapcar (lambda (x y) (list (car x) y)) lsts indexes))) ;;the first one is the weight of each HRR-lst
	    (show "weights {}" weights)
	    (setf lsts (mapcar (lambda (l) (cdr l)) lsts))
	    (dotimes (i (length (car lsts)))
	      (setf index (second (weighted-random-choice weights)))
	      (setf lst (append lst (list (nth i (nth index lsts))))))))
	(order?
	  (let* ((len (length lsts))
		  (weights (mapcar (lambda (x) (list (/ len (+ x 1)) x)) indexes)))
	    ;;(show "weights {}" weights)
	    (dotimes (i (length (car lsts)))
	      (setf index (second (weighted-random-choice weights)))
	      (setf lst (append lst (list (nth i (nth index lsts))))))))
	(t
	  (dotimes (i (length (car lsts)))
	    (setf index (random-choice indexes))
	    (setf lst (append lst (list (nth i (nth index lsts))))))))
      (if final-weight (push final-weight lst) lst))))
;;(interleave0 :lsts '((1 2 3 4) (a b c d) (! @ $ %)))
;;(interleave0 :lsts '(0.1 (1 2 3 4) (a b c d) (! @ $ %)))
;;(interleave0 :lsts '(0.1 (0.1 1 2 3 4) (0.9 a b c d) (0.2 ! @ $ %)) :weight? t)

(defun interleave (&key (lst (list (randomize) (list (randomize) (list (randomize) (randomize)) (randomize)))) (weight? nil) (order? nil))
  (interleave0 :lsts (mapcar (lambda (l) (if (and (listp l) (listp (cadr l))) (interleave :lst l :weight? weight? :order? order?) l)) lst) :weight? weight? :order? order?)) ;;order? could be not so important because (in sun sky) and (in sky sun) can be different by randomly interleaving their items. Even the same (in sun sky) can be different when interleaving their items next time due to random-choice. 
;;(interleave)
;;(interleave :lst '(((1 2 3 4) (a b c d)) (! @ $ %)))
;;(interleave :lst '((0.1 1 2 3 4) (0.2 (0.9 a b c d) (0.2 ! @ $ %))) :weight? t)

(defun correlate0 (&optional (lst0 (randomize)) (lst1 (randomize)))
  (convolve-correlate0 :lst0 lst0 :lst1 lst1 :type 'correlate))
;;(show "correlate: {}" (correlate0))

(defun correlate (&optional (lst0 (randomize)) (lst1 (randomize)))
  (convolve-correlate0 :lst0 lst0 :lst1 lst1 :type 'correlate))

(defun cosine-similar (&optional (lst0 (randomize)) (lst1 (randomize)))
  (let ((scale (* (norm lst0) (norm lst1))))
    ;;(show "scale {}" scale)
    (if (= scale 0) ;this can be used to avoide the error: division by zero 
      0
      (let* ((AB (apply #'+ (mapcar (lambda (a b) (* a b)) lst0 lst1)))
	      (A1 (sqrt (apply #'+ (mapcar (lambda (a) (* a a)) lst0))))
	      (B1 (sqrt (apply #'+ (mapcar (lambda (b) (* b b)) lst1))))
	      (cosine-simi (/ AB (* A1 B1))))
	;;(/ cosine-simi scale);ATTENTION: this makes two same lst only 0.25
	cosine-simi
	))))
;;(cosine-similar)
;;(cosine-similar '(1 1 1 1) '(-1 -1 -1 -1)) ;-> -1 <- mirror same
;;(cosine-similar '(1 1 1 1) '(-1 -1 1 1)) ;-> 0 <- most difference
;;(cosine-similar '(1 1 1 1) '(-1 -100 1 1)) ;-> -0.5
;;(cosine-similar '(1 100 -1 -1) '(-1 -100 1 1)) ;-> -1 
;;(cosine-similar '(1 1 1 1) '(1 1 1 1)) ;-> 1 <- same
;;(cosine-similar '(1 1 1 1) '(0 0 0 0)) ;-> 0 
;;(cosine-similar '(-1 -1 -1 -1) '(-1 -1 -1 -1)) ;-> 1

(defun cosine-distan (&optional (lst0 (randomize)) (lst1 (randomize)))
  (- 1 (cosine-similar lst0 lst1)))

(defun test-fft ()
  (defparameter *buf* #2A((1 2 3 4)(5 6 7 8)))
  (defparameter *buf* #(1 2 3 4))
  (let ((transformed (fft:fft *buf*)))
    (show "transformed {}" transformed)
    (fft:ifft transformed)))
;;(test-fft)

(defun test-hrr3 (&optional (a (make-agent)))
  (let* ((minus (randomize 1024)) ;;normal random distribution
	  (plus (randomize 1024))
	  (int (randomize 1024))
	  ;;(tin (interleave :lst (list minus int)))
	  ;;(in (interleave :lst (list minus tin)))
	  ;;(on (interleave :lst (list plus int)))
	  ;;(to (interleave :lst (list plus on)))
	  ;;(tin (add minus int))
	  ;;(in (add minus tin))
	  ;;(on (add plus int))
	  ;;(to (add plus on))
	  (tin (convolve minus int))
	  (in (convolve minus tin))
	  (on (convolve plus int))
	  (to (convolve plus on))
	  (center (randomize 1024))
	  (up (interleave :lst (list center (randomize 1024)))) ;;1/2 
	  (down (interleave :lst (list center (randomize 1024)))) ;;-1/2 
	  (right (interleave :lst (list center (randomize 1024)))) ;;0 
	  (left (interleave :lst (list center (randomize 1024)))) ;;1(-1)
	  ;;(up (add center (randomize 1024)))
	  ;;(down (add center (randomize 1024)))
	  ;;(right (add center (randomize 1024)))
	  ;;(left (add center (randomize 1024)))
	  )
    (show "(cosine-distan minus plus) {}" (cosine-distan minus plus))
    (show "(cosine-distan in tin) {}" (cosine-distan in tin))
    (show "(cosine-distan in int) {}" (cosine-distan in int))
    (show "(cosine-distan in on) {}" (cosine-distan in on))
    (show "(cosine-distan in to) {}" (cosine-distan in to))
    (show "(cosine-distan on to) {}" (cosine-distan on to))
    (show "(cosine-distan on int) {}" (cosine-distan on int))
    (show "(cosine-distan center down) {}" (cosine-distan center down))
    (show "(cosine-distan up down) {}" (cosine-distan up down))
    (show "(cosine-distan up right) {}" (cosine-distan up right))
    (show "(cosine-distan right left) {}" (cosine-distan right left))
    ))
;;(test-hrr3)
#|
;;convolve
(cosine-distan in tin) 1.0303804975503852d0
(cosine-distan in int) 1.0163713689393568d0
(cosine-distan in on) 0.9979821201319221d0
(cosine-distan in to) 1.0323446154760128d0
(cosine-distan on to) 1.0266872213802203d0
;;add
(cosine-distan in tin) 0.050678129376840464d0
(cosine-distan in int) 0.5479537072722132d0
(cosine-distan in on) 0.6711512409787739d0
(cosine-distan in to) 0.7866972285056416d0
(cosine-distan on to) 0.05170462550020338d0
;;interleave
(cosine-distan in tin) 0.24035893161412392d0
(cosine-distan in int) 0.7379591971752149d0
(cosine-distan in on) 0.8419249365669045d0
(cosine-distan in to) 0.9020269625993512d0
(cosine-distan on to) 0.23211606726386724d0
|#

(defun test-hrr ()
  (let* ((size (randomize 1024)) 
	  (zero (randomize 1024)) 
	  (plus (randomize 1024))
	  (minus (randomize 1024))
	  (color (randomize 1024))
	  (small (interleave :lst (list size minus)))
	  (large (interleave :lst (list size plus)))
	  (medium (interleave :lst (list size zero)))
	  (medium-v (interleave :lst (list medium zero)))
	  (medium-vv (interleave :lst (list medium-v zero)))
	  (dark (interleave :lst (list color minus)))
	  (bright (interleave :lst (list color plus)))
	  (gray (interleave :lst (list color zero)))
	  (small--large-dark (interleave :lst (list small (list large dark))))
	  (small-large--dark (interleave :lst (list (list small large) dark)))
	  (small-large-dark (interleave :lst (list small large dark))))
    (show "(cosine-distan small dark) {}" (cosine-distan small dark))
    (show "(cosine-distan small large) {}" (cosine-distan small large))
    (show "(cosine-distan small bright) {}" (cosine-distan small bright))
    (show "(cosine-distan medium medium-v) {}" (cosine-distan medium medium-v))
    (show "(cosine-distan medium-v medium-vv) {}" (cosine-distan medium-v medium-vv))
    (show "(cosine-distan medium medium-vv) {}" (cosine-distan medium medium-vv))
    (show "(cosine-distan small--large-dark small-large--dark) {}" (cosine-distan small--large-dark small-large--dark))
    (show "(cosine-distan small-large-dark small-large--dark) {}" (cosine-distan small-large-dark small-large--dark))
    ))
;;(test-hrr)
#|
result using interleave
(cosine-distan small dark) 0.7017338580380739d0
(cosine-distan small large) 0.7154819440988287d0
(cosine-distan small bright) 0.9431260971503276d0
(cosine-distan medium medium-v) 0.2606417139859103d0
(cosine-distan medium-v medium-vv) 0.10015508182257804d0
(cosine-distan medium medium-vv) 0.364422967276706d0

result using add
(cosine-distan small dark) 0.5205128429537831d0
(cosine-distan small large) 0.5149542089845056d0
(cosine-distan small bright) 1.044998050375362d0
(cosine-distan medium medium-v) 0.05061853428072516d0
(cosine-distan medium-v medium-vv) 0.009946045946157d0
(cosine-distan medium medium-vv) 0.10425487601688d0

result using convolve
(cosine-distan small dark) 1.005185421864453d0
(cosine-distan small large) 1.0122568729239734d0
(cosine-distan small bright) 1.0648462266674172d0
(cosine-distan medium medium-v) 1.0886302963214323d0
(cosine-distan medium-v medium-vv) 1.0848495986912403d0
(cosine-distan medium medium-vv) 0.9255655153642363d0
|#

(defun count-items (&optional (lst '(1 1 1 0 3 3))) ;;-> ((3 2) (1 3) (0 1)) 
  (let* ((items (remove-duplicates lst))
	  (result (mapcar (lambda (item) (list item (count item lst))) items)))
    (sort result #'list>))) ;;result '((biggest-item its-amount) (item amount) (item amount) ...)
;;(show "{}" (count-items))

(defun max-depth (&optional (lst '((on up) (large tria) (and ((tin down) (medium rect) (large rect)) ((in up) (small rect) (large rect)) ((to left) (small rect) (medium rect))))) (count 0) (start 1))
  (let ((output? nil))
    (when start (setf start nil) (setf output? t))
    (let ((counts (list count)))
      (dolist (l lst)
	(when (listp l) (push (max-depth l (+ count 1) start) counts)))
      (if output?
	(progn
	  ;;(show "{}" counts)
	  ;;(list (apply #'max counts) (count-items counts))
	  (count-items counts)
	  )
	(apply #'max counts)))))
;;(max-depth)
;;(max-depth '((in up) ((intersect left) (triangle right small) (circle up medium)) (rectangle up large))) ;((2 1) (1 2) (0 1))
;;(max-breadth '((in up) ((intersect left) (triangle right small) (circle up medium)) (rectangle up large))) ;((3 3) (2 1)) 

;;count the greatest number of items in the list or its sublists
(defun max-breadth (&optional (lst '((on up) (large tria) (and ((tin down) (medium rect) (large rect)) ((in up) (small rect) (large rect)) ((to left) (small rect) (medium rect))))) (start 1))
  (let ((output? nil))
    (when start (setf start nil) (setf output? t))
    (let ((counts (list (length lst))))
      (dolist (l lst)
	(when (listp l) (push (max-breadth l start) counts)))
      (if output?
	(progn
	  ;;(show "{}" counts)
	  ;;(list (apply #'max counts) (count-items counts))
	  (count-items counts)
	  )
	(apply #'max counts)))))
;;(show "{}" (max-depth));((3 1) (1 2) (0 1));1 part with max-depth 3
;;(show "{}" (max-breadth));((4 1) (3 1) (2 2));1 part with max-breadth 4
;;(show "{}" (max-depth '((on left) (rectangle up medium) (triangle right small))));((1 3) (0 1));3 parts with max-depth 1
;;(show "{}" (max-breadth '((on left) (rectangle up medium) (triangle right small))));((3 3) (2 1));3 parts with max-breadth 3
;;(show "{}" (max-depth '(rectangle up medium)));((0 1));1 part with max-depth 0 <-- the whole is depth 0
;;(show "{}" (max-breadth '(rectangle up medium)));((3 1));1 part with max-breadth 3 <-- the whole's breadth is 3
;;(show "{}" (max-depth '((on left) boat sea)));((1 1) (0 1));1 part with max-depth 1, the whole with depth 0
;;(show "{}" (max-breadth '((on left) boat sea)));((3 1) (2 1));1 part (the whole) with max-breadth 3, 1 part with breadth 2

(defun max-list (&optional (lsts '((3 ((3 1) (2 1))) (3 ((3 3) (2 1))) (3 ((3 3))) (3 ((3 3) (2 1) (1 1))))))
  (let* ((lsts-fla (mapcar (lambda (l) (flatten l)) lsts))
	  (max-len (apply #'max (mapcar (lambda (l) (length l)) lsts-fla)))
	  (winner nil))
    (dotimes (i max-len)
      (let ((items (mapcar (lambda (lst-i) (list (nth i (nth lst-i lsts-fla)) lst-i)) (range (length lsts-fla)))))
	(setf items (sort (copy-seq items) #'list>))
	(if (or (= (length items) 1) (/= (car (car items)) (car (second items)))
	      (= i (- max-len 1)));;if the last one item still has one more winners who are the same, select the first one. 
	  (progn
	    (setf winner (nth (second (car items)) lsts))
	    (return))
	  (let ((candi-indexes '()) (candi-lsts '()))
	    (dolist (item items)
	      (if (= (car item) (car (car items)))
		(push (second item) candi-indexes)
		(return)))
	    (dotimes (index (length lsts))
	      (when (and (member index candi-indexes);eg. to avoid (3 ((2 9))) being selected as winner -> (3 ((2 0)))
		      (/= (- (length (nth index lsts-fla)) 1) i));eg. to avoid (3 ((3 3) (2 1))) no items for next iteration 
		(push (nth index lsts) candi-lsts)))
	    (setf lsts (copy-seq candi-lsts))
	    (setf lsts-fla (mapcar (lambda (l) (flatten l)) lsts))))))
    winner))
;;(max-list)
;;(max-list '((3 ((3 1) (2 1))) (3 ((3 3) (2 1))) (3 ((3 4))) (3 ((3 3) (2 1) (1 1)))))
;;(max-list '((3 ((3 1) (2 1))) (3 ((3 3) (2 1))) (3 ((2 9))) (3 ((3 3) (2 1) (1 1)))))
;;(max-list '((3 ((3 1) (2 1))) (3 ((3 3) (2 1))) (3 ((2 9))) (3 ((3 3) (2 1) (1 1))) (3 ((3 3) (2 1) (1 1)))))

;; keep common lisp data type -------------------------------------------------------------------
(defun save-file (&key (data "hello") (file "temp.txt"))
  (with-open-file (out file
		    :direction :output
		    :if-exists :supersede
                    :if-does-not-exist :create)
    (with-standard-io-syntax
      (print data out))))
;;(save-file :data (>> 'a 1 'b 2 'c 4000000) :file "/home/an/Documents/temp.txt")

(defun load-file (&optional (file "temp.txt"))
  (with-open-file (in file)
    (with-standard-io-syntax
      ;;(let ((data (read in))) data)
      (read in)
      )))
;;(let ((data (load-file "/home/an/Documents/temp.txt"))) (show "{}" data))
;;------------------------------------------------------------------------------------------------

;;change data type to strings ---------------------------------------------------------------------
(defun extend-file (&key (data "hello") (file "temp.txt") (newline t) (new nil))
  ;;new means deleting the existing data of the file in advance
  (when new (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)))
  (with-open-file (stream file :direction :output :if-exists :append :if-does-not-exist :create)
    (when (not (listp data)) (setf data (list data)))
    (dolist (d data)
      (if newline
        (format stream "~%~a" d)
        (format stream "~a" d)))
    ))
;;(extend-file :data "just" :file "/home/an/Documents/temp.txt" :new t)
;;(extend-file :data "just" :file "/home/an/Documents/temp.txt" :newline nil)
;;(extend-file :data (list "a=1" "b=2" "c=a+b" 111 'keyy) :file "/home/an/Documents/temp.txt" :new nil)
;;(save-file :data (list "a=1" "b=2" "c=a+b" 111 'keyy) :file "/home/an/Documents/temp.txt")

(defun read-file (&key (file "/home/an/Documents/temp.org"))
  (with-open-file (stream file)
    (do ((line (read-line stream nil)
           (read-line stream nil)))
      ((null line))
      (format t "~a~%" line))))
;;(read-file :file "/home/an/Documents/temp.py")

;;read the contents of a file into a list
(defun get-file (&key (file "/home/an/Documents/temp.org"))
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
      while line
      collect line)))
;;(car (get-file :file "/home/an/Documents/result"))

;;-------------------------------------------------------------------------------------------------

(defun print-hash (hashtable output-stream)
  (with-standard-io-syntax
    (print (hash-table-count hashtable) output-stream)
    (maphash #'(lambda (k v)
		 (print  k output-stream)
		 (print  v output-stream))
      hashtable)))

(defun read-hash (input-stream)
  (let ((hnew (make-hash-table))
	 (n       0))
    (with-standard-io-syntax
      (setf n (read input-stream))
      (dotimes (i n)
	(setf (gethash (read input-stream) hnew) (read input-stream))))
    hnew))

(defun strengthen (&key (weight 0.01) (rate 0.1));s=a+(1-a)s ;s: weight, a: learningRate (rate)
  (round-float (+ rate (* (- 1 rate) weight)) 3))
;;(strengthen)

(defun weaken (&key (weight 0.01) (rate 0.1));s=(1-a)s ;s: weight, a: learningRate (rate)
  (round-float (* (- 1 rate) weight) 3))
;;(weaken)

(defun simplify-list (&optional (lst '((((down)) ((left))))))
  (if (= (length lst) 1)
    (progn (setf lst (car lst))
      (when (listp lst) (setf lst (simplify-list lst))))
    (setf lst (mapcar (lambda (l) (if (listp l) (simplify-list l) l)) lst)))
  lst)
;;(simplify-list '(down (right))) ;;'((((down)) (left))) -> '(down left)
;;(simplify-list '(down))

(defun pair-items (&optional (lst '(1 2 3 4))) ;-> '((1 2) (3 4))
  (let ((new-lst '()))
    (dotimes (i (length lst))
      (when (evenp i)
	(push (list (nth i lst) (nth (+ i 1) lst)) new-lst)))
    (reverse new-lst)))
;;(pair-items)

(defun average (&optional (lst '(1 2 3 4 5 6)))
  (/ (apply #'+ lst) (length lst)))
;;(average)

(defun order-magnitude (&optional (x 10.3))
  (let ((n 0))
    (loop do
      (if (> x 10)
	(progn
	  (setf x (/ x 10))
	  (setf n (+ n 1)))
	(progn
	  (setf x (* x 10))
	  (setf n (- n 1))))
      while (or (< x 1) (> x 10)))
    n))
;;(order-magnitude 3000)

(defun axis-segments (&optional (x 3))
  (let* ((segment (/ x 10.0))
	  (om (order-magnitude segment))
	  (candies (mapcar (lambda (m) (list (- m segment) m)) (list (float (* 2 (expt 10 om))) (float (* 5 (expt 10 om)))))))
    (setf candies (remove-if (lambda (n) (< (car n) 0)) candies))
    (setf segment (second (car (sort candies #'list<))))
    (range (+ x segment) :step segment)
    ))
;;(show "{}" (axis-segments))
;;(show "{}" (axis-segments 30000))

(defun lst2set (&key (lst '(1 2 3 (a s) 4 4 (a s))))
  (remove-duplicates lst :test 'equal))

(defun make-sublists (&key (num '(4 3 2)) (value 0))
  (if (= (length num) 1)
    (mapcar #'(lambda (x) value) (range (car num)))
    (mapcar #'(lambda (x) (make-sublists :num (cdr num) :value value)) (mapcar #'(lambda (y) '()) (range (car num))))))

(defun list2csv (&key (lst '((1 2 3) (4 5 6) (7 8 9))) (file "temp.csv"))
  (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create))
  (if (listp (car lst))
    (dotimes (i (length lst))
      (if (= i 0)
	(extend-file :data (format nil (concatenate 'string "~{~A~^" "," "~}") (nth i lst)) :file file :newline nil)
	(extend-file :data (format nil (concatenate 'string "~{~A~^" "," "~}") (nth i lst)) :file file :newline t)))
    (extend-file :data (format nil (concatenate 'string "~{~A~^" "," "~}") lst) :file file :newline nil)))

(defun get-value-range (&key (value 4) (tolerance 1/5) (values (range 11)))
  ;;if tolerance==0.0, get value itself
  ;;if tolerance==1, get everything
  (let ((value-range (>>))
	 (base (- (car (last values)) (car values))))
    (dolist (v values)
      (let ((score (sine-curve :x (/ (- v (car values)) base) :x1 (/ (- value (car values)) base))));;adjust x, x1 in range [0,1]
	(when (>= score (- 1 tolerance))
	  (>> value-range v (/ score 10)))))
    value-range)) 
;;(get-value-range :value 4 :tolerance 1/5 :values (range 11))

(defun get-consonants ()
  (list "b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"))

(defun get-vowels ()
  (list "a" "e" "i" "o" "u"))

(defun key-with-value (&key (dic (>> 'a 2 'b 3 'c 1)) (type 'max)) ;;type 'max 'min
  (let ((keys (hash-keys dic))
	 (values (hash-values dic)))
    (nth (position (apply type values) values) keys)))
;;(key-with-value :dic (>> 'a 2 'b 3 'c 1) :type 'max)

;;consistency '(a b c d e) -> 0
;;consistency '(a a a a a) -> 1
(defun consistency (&key (lst '(a b c d e)))
  (let ((result (gensym)))
    (if (< (length lst) 2)
      (setf result 1)
      (let ((lst0 (remove-duplicates lst :test 'equal)))
	(setf result (- 1 (/ (- (length lst0) 1) (- (length lst) 1))))))
    result))

(defun divide-string (&key (str "jinikaci") (length 1))
  (let ((lst '()))
    (dotimes (i (length str))
      (when (= (mod i length) 0) (push (>> str (list i (min (length str) (+ i length)))) lst))) (reverse lst)))
;;(divide-string :str "jinikaci" :length 2) -> ("ji" "ni" "ka" "ci")
;;(divide-string :str "jinikaci" :length 3) -> ("jin" "ika" "ci")
;;(divide-string :str "jinikaci" :length 10) -> ("jinikaci")
;;(divide-string :str "jinikaci" :length 1) -> ("j" "i" "n" "i" "k" "a" "c" "i")

(defun to-radian (&key (angle 45))
  (* (/ angle 180) pi))

