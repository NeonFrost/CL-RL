(setf main-map (make-array '(100 100) :initial-element "."))
(setf item-array (make-array '(100 100)))
(setf enemy-array (make-array '(100 100)))
(setf (aref item-array 15 30) (make-leather-cuirass))
(setf (aref enemy-array 30 46) (make-socra))

(loop for y below (car (array-dimensions main-map))
   do (loop for x below (cadr (array-dimensions main-map))
	 do (if (or (eq y 0)
		    (eq y (1- (car (array-dimensions main-map))))
		    )
		(setf (aref main-map y x) "#")
		(if (or (eq x 0)
			(eq x (1- (cadr (array-dimensions main-map))))
			)
		(setf (aref main-map y x) "#")
		))
	   ))

(loop for n below 30
   do (setf (aref main-map
		  (1+ (random (- (car (array-dimensions main-map)) (+ n 1))))
		  (1+ (random (- (cadr (array-dimensions main-map)) (+ n 1))))
		  )
	    "O"))

#|(loop for y below (car (array-dimensions main-map))
   do (loop for x below (cadr (array-dimensions main-map))
	 do (princ (aref main-map y x))
	   )
     (fresh-line))|#
