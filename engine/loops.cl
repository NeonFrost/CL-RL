(defun title-loop ()
  )

(defun level-loop ()
  (if (or (eq sub-state 'enemy-information)
	  (eq sub-state 'item-information))
      (render-information)
      (render-level))
  (if moving
      (progn (entity-move player :direction moving) 
#|	       (north (decf (cadr (assoc :y (entity-position player))) 1))
	       (south (incf (cadr (assoc :y (entity-position player))) 1))
	       (west (decf (cadr (assoc :x (entity-position player))) 1))
	       (east (incf (cadr (assoc :x (entity-position player))) 1))
	       )|#
	     (setf moving nil)
	     ))
  )

(defun inventory-loop ()
  (if (eq sub-state 'item-information)
      (render-item-information)
      (render-inventory)
      )
  )

(defun credits-loop ()
  )

(defun game-over-loop ()
  )
