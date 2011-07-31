(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf asdf:*central-registry* '("/home/martin/0505/mma/"))
  (require :gui))
(defpackage :r
  (:use :cl :gl))

(in-package :r)


(defun get-little-ub32 (a off)
  (+ (* 256
	(+ (* 256
	      (+ (* 256 (aref a (+ 3 off)))
		 (aref a (+ 2 off))))
	   (aref a (+ 1 off))))
     (aref a off)))

(defun get-little-sb16 (a off)
  (declare (type (simple-array unsigned-byte 1) a)
	   (type fixnum off)
	   (values (signed-byte 16) &optional))
  (let ((v (+ (* 256 (aref a (+ 1 off)))
	      (aref a (+ 0 off)))))
    (if (< v 32768)
	v
	(- v 65535))))

;; https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
(defun read-wav (fn)
  (declare (values (simple-array (signed-byte 16) 1)
		   (simple-array (signed-byte 16) 1)
		   &optional))
  (with-open-file (s fn :direction :input
		     :element-type 'unsigned-byte)
    (let ((head (make-array 44 
			    :element-type
			    'unsigned-byte)))
      (read-sequence head s)
      (let* ((n (get-little-ub32 head 40))
	     (a (make-array n
			    :element-type 'unsigned-byte)))
	(read-sequence a s)
	(let ((left 
	       (make-array (/ n 4) 
			   :element-type '(signed-byte 16)))
	      (right 
	       (make-array (/ n 4) 
			   :element-type '(signed-byte 16))))
	  (dotimes (i (/ n 4))
	    (let ((j (* 4 i)))
	     (setf (aref left i) (get-little-sb16 a j)
		   (aref right i) (get-little-sb16 a (+ 2 j)))))
	  (values left right))))))

(defparameter *points* nil)
#+nil
(time
 (setf *points*
       (multiple-value-list
	(read-wav
	 "/home/martin/Downloads/youscope-wave.wav"))))
#+nil
(subseq (second *points*) 0 100)

(let ((count 0))
 (defun draw-screen ()
   (enable :blend)
   (blend-func :src-alpha :one-minus-src-alpha)
   (color 1 1 1 .1)
   (point-size 3)
   (clear :color-buffer-bit)
   (let ((sx .009)
	 (sy .009)
	 (n (floor 48000 60)))
    (destructuring-bind (left right) *points*
      (translate 400 300 0)
      (scale sx (- sy) 1)
      (incf count n)
      (with-primitive :points
	(unless (< (+ count n) (min (length left)
				    (length right)))
	  (setf count 0))
	(dotimes (j n)
	  (vertex (aref right (+ count j)) 
		  (aref left (+ count j)))))))))
;; 60 Hz framerate
;; 48000 Hz 

#+nil
(sb-thread:make-thread
 #'(lambda ()
     (unless *points*
       (setf *points*
	     (multiple-value-list
	      (read-wav
	       "/home/martin/Downloads/youscope-wave.wav"))))
     (gui:with-gui (800 600)
       (draw-screen)))
 :name "camera-display")
