(defvar direction-list '((north (0 -1)) ;north
			 (east (1 0)) ;east
			 (south (0 1)) ;south
			 (west (-1 0)) ;west
			 (north-west (-1 -1)) ;nw
			 (north-east (1 -1)) ;ne
			 (south-east (1 1)) ;se
			 (south-west (-1 1)) ;sw
			 ))

(defgeneric entity-move (e &key direction)
  (:documentation "moves an entity")
  )

(defmethod entity-move ((e entity) &key direction)
  (reset-messages)
  (reset-text-buffer message-buffer)
  (if (not (player-character-p e))
      (reset-text-buffer enemy-buffer))
  (let* ((movement (+ (entity-agility e) (entity-agility-mod e)))
	 (move-x (* movement (car (cadr (assoc direction direction-list)))))
	 (move-y (* movement (cadr (cadr (assoc direction direction-list)))))
	 (space-x (+ (cadr (assoc :x (entity-position e))) move-x))
	 (space-y (+ (cadr (assoc :y (entity-position e))) move-y))
	 (object (cond ((string= (aref main-map space-y space-x) (cadr (assoc 'tree-trunk symbols-list)))
			"tree")
		       ((string= (aref main-map space-y space-x) (cadr (assoc 'water symbols-list)))
			"water")
		       ((string= (aref main-map space-y space-x) (cadr (assoc 'wall symbols-list)))
			"wall")))
	 )    
    (if (not (or (string= (aref main-map space-y space-x) (cadr (assoc 'tree-trunk symbols-list)))
		 (string= (aref main-map space-y space-x) (cadr (assoc 'water symbols-list)))
		 (string= (aref main-map space-y space-x) (cadr (assoc 'wall symbols-list)))))
	(progn (if (or (and (> (cadr (assoc :x (entity-position e))) 1)
			    (< move-x 0))
		       (and (< (cadr (assoc :x (entity-position e))) (- (cadr (array-dimensions main-map)) 2))
			    (> move-x 0)))
		   (incf (cadr (assoc :x (entity-position e))) move-x))
	       (if (or (and (> (cadr (assoc :y (entity-position e))) 1)
			    (< move-y 0))
		       (and (< (cadr (assoc :y (entity-position e))) (- (car (array-dimensions main-map)) 2))
			    (> move-y 0))
		       )
		   (incf (cadr (assoc :y (entity-position e))) move-y))
	       )
	(push-message (concatenate 'string "There is a " object " in your way."));;;;render-message: there is a [TOKEN] in your way
	))
  (create-message)
  (if (eq e player)
      (setf (cursor-x cursor) (cadr (assoc :x (entity-position player)))
	    (cursor-y cursor) (cadr (assoc :y (entity-position player))))
      ))

