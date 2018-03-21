(defun title-loop ()
  (render-title-screen)
  )

(defun start-screen-loop ()
  (render-start-screen)
  (if (not room-buffer)
      (stage-room-buffer)))

(defun level-loop ()
  (if (or (eq sub-state 'enemy-information)
	  (eq sub-state 'item-information))
      (render-information)
      (render-level))
  (if start-screen-buffer
      (progn (remove start-screen-buffer buffers)
	     (reset-text-buffer start-screen-buffer)))
  (if moving
      (progn (entity-move player :direction moving) 
	     (setf moving nil)))
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
