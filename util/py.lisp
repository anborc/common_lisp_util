;;;; py.lisp

(in-package #:util)

(defun py2cl_list (&optional (lst "[1, 2, 3]"))
  (l2c (replace-all (replace-all lst "," " ") "'" "\"")))
;;(car (py2cl_list "[\"1\", 2, 3]"))
;;(car (py2cl_list (car (get-file :file "/home/an/Documents/result"))))

(defun run-python2result ()
  ;;(pyprint "Good morning")
  ;;(asdf:RUN-SHELL-COMMAND "python /home/an/Documents/temp.py") ;;this does not work
  (sb-ext:run-program "python" '("/home/an/Documents/temp.py") :search t :wait t)
  (let* ((ddd (read-file :file "/home/an/Documents/result")))
    ;;(format t "~a~%" ddd)
    ;;(print ddd)
    ;;(print (equalp ddd 1))
    ))
;;(run-python2result)

(defun py_cap (&optional (str "aa bb hi!do!fsa_a") (cap "!")) ;;-> "aa bb hiDoFsa_a"
  (let* ((lst (split str :gap " ")) (lst_new '()))
    (dolist (l lst)
      (if (position (coerce cap 'character) l)
	(let ((sub_lst (split l :gap cap)) (i 0) (sub_lst_new '()))
	  (dolist (sl sub_lst)
	    (when (> i 0) (setf sl (>> sl 0 (string-upcase (>> sl 0)))))
	    (push sl sub_lst_new)
	    (incf i))
	  (push (join (reverse sub_lst_new) :link "") lst_new))
	(push l lst_new)))
    (join (reverse lst_new) :link " ")))
;;(py_cap "aa bb hi_do_fsa!a" "_") ;;"aa bb hiDoFsa!a"
;;(string-capitalize "do_fsa!a") ;;-> "Do_Fsa!A" not "Do_fsa!a"

(defun py (&key (data '(print["hello world"])) (level 0) (file "/home/an/Documents/temp.py") (new t))
  (when new (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)))  
  (with-open-file (stream file :direction :output :if-exists :append :if-does-not-exist :create)
    (labels
      ((py0 (&key (data '(print["hello world"])) (level 0))
	 (dolist (d data)
	   (if (listp d)
	     (cond
	       ((equalp (car d) 'list) 
		 (if (member 'for d)
		   ;;convert e.g. (list i for i in range(10)) to [i for i in range (10) ]
		   (progn
		     (format stream "[")
		     (dotimes (i (length d))
		       (when (> i 0)
			 (if (stringp (nth i d)) (format stream "\"~a\" " (nth i d)) (format stream "~(~a~) " (nth i d)))))
		     (format stream "]"))
		   ;;convert list to python list [..,..,...]
		   (progn
		     (format stream "[")
		     (dotimes (i (length d))
		       (cond
			 ((= i (- (length d) 1))
			   (if (stringp (nth i d)) (format stream "\"~a\"" (nth i d)) (format stream "~(~a~)" (nth i d))))
			 ((> i 0)
			   (if (stringp (nth i d)) (format stream "\"~a\", " (nth i d)) (format stream "~(~a~), " (nth i d))))
			 (t nil)))
		     (format stream "]"))))
	       (t (progn
		    (format stream "~%~v@{~A~:*~}" (* 2 level) " ")
		    (py0 :data d :level (+ 1 level)))))
	     (if (stringp d)
	       (format stream "\"~a\"" d)
	       (let ((l (format nil "~(~a~)" d)))
		 (setf l (replace-all l "[" "("))
		 (setf l (replace-all l "]" ")"))
		 (setf l (py_cap l))
		 (format stream "~a " l)))))))
      (py0 :data data :level level))))

(defun test_py ()
  (let*
    ((i "balabala")
      (n 100)
      (code_file "/home/an/Documents/temp.py")
      (result_file "/home/an/Documents/result")
      (data
	`((import math)
	   (a = math.pi)
	   (b = (list 1 2 ,i 4 5))
	   (c = (list i for i in range(,n)))
	   (def foo[]\:
	     (print["Hello world"])
	     (print[a])
	     (print[b])
	     (print[c])
	     (def foo3[]\:
	       (print["embed function1"])
	       (def foo5[]\:
		 (pass)))
	     (return[(list a b c)]))
	   (f = open[ ,result_file \,"w"])
	   (f.write["{}\\n".format[foo[]]])
	   (f.close[])
	   )))
    (py :data data :file code_file)
    (sb-ext:run-program "python" `(,code_file) :search t :wait t)
    (let ((result (py2cl_list (car (get-file :file result_file)))))
      (print result)
      (print (car result))
      (print (equalp '(1 2 "balabala" 4 5) (second result))))
    ))
;;(test_py)

(defun test_cadquery ()
  (let*
    ((code_file "/home/an/Documents/test_cadquery.py")
      (length 80.0)
      (height 60.0)
      (thickness 10.0)
      (center_hole_dia 25.5)
      (data
	`((import cadquery)
	   (from !helpers import show)
	   (result = cadquery.!workplane["XY"].box[ ,length \, ,height \, ,thickness ].faces[">Z"].workplane[].hole[ ,center_hole_dia ])
	   (show[result]))))
    (py :data data :file code_file)))
;;(test_cadquery)

;;temp.py -----------------------------------
;;lst = []
;;for i in xrange(100):
;;  lst.append(i)
;;
;;f = open("/home/an/Documents/result",'w')
;;f.write('{}\n'.format(lst))
;;#f.write('hi there yes or no\n') 
;;f.close()
;;-------------------------------------------

(defun program-stream (program &optional args)
  (let ((process (sb-ext:run-program program args
		   :input :stream
		   :output :stream
		   :wait nil
		   :search t)))
    (when process
      (make-two-way-stream (sb-ext:process-output process)
	(sb-ext:process-input process)))))

(defun test-program-stream ()
  (defparameter *stream* (program-stream "python" '("-i")))
  (loop while (read-char-no-hang *stream*))
  ;;(format *stream* "1+2~%")
  (format *stream* "import math~%")
  (format *stream* "math.pi~%")
  (finish-output *stream*)
  ;;(read-line *stream*)
  (format t "~a~%" (read-line *stream*))
  ;;(format t "~a~%" (read *stream*))
  ;;(format t "~a~%" (read *stream*))
  (close *stream*))

;;(sb-ext:run-program "python" '("/home/an/Documents/temp.py") :search t :wait t)
;;(sb-ext:process-exit-code *)
;;(let ((asd (sb-ext:process-exit-code (sb-ext:run-program "python" '("/home/an/Documents/temp.py") :search t :wait t)))) (show "{}" asd))
