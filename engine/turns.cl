;;;;skeleton system for turn based combat
;;;;Speed difference is the difference between the current and next entity (i.e. Vanya-speed - socra-speed)
;;;;the difference is taken away from the current entity's speed and tested against the next entity's speed
(defun end-turn ()
#|  (if (> current-turn (1- (length turn-order)))
      (setf current-turn 0)
      (incf current-turn 1))|#
  (setf sub-state 'top)
  )

#|
(defun test-turn (entity-1 entity-2)
  (if 
  (setf current-turn (nth entities entity-1))
      (setf current-turn (nth entities entity-2))
      )
  )
|#
