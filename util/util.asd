;;;; util.asd

(asdf:defsystem #:util
  :description "Describe util here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (:external-program
		:alexandria
		:cl-csv
                :asdf
		:fft
		:cffi
		:lispbuilder-sdl
		:lispbuilder-sdl-image
		;;:lispbuilder-sdl-gfx ;rotate surface
		:cl-opengl
		:cl-glut
		:cl-glu
		:cl-glut-examples
		:vgplot		
		)
  :serial t
  :components ((:file "package")
		(:file "gnuplot") 
		(:file "util") ;;depends on "gnuplot"
		(:file "som")
		(:file "art") ;;depends on "util"
		(:file "graph")
		(:file "turtle")
		(:file "visual")
		(:file "gng")
                (:file "py")
		(:file "temp")))

