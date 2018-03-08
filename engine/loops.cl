(defun title-loop ()
  (render-title-screen)
  )

(defun level-loop ()
  (if (or (eq sub-state 'enemy-information)
	  (eq sub-state 'item-information))
      (render-information)
      (render-level))
  (if moving
      (progn (entity-move player :direction moving) 
	     (setf moving nil)
	     ))
  )

(defun equip-loop ()
  (render-equip-screen)
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
