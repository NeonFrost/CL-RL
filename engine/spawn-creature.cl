﻿(defvar spawn-accumulator 0)
(defvar default-spawn-rate 100)

(defun test-spawn ()
  "The spawn accumulator increases by a certain number (1 or >1) at the start of every player (note: not `creatures`) turn.
spawn-accumulator is increased before test-spawn is called."
  (if (> spawn-accumulator default-spawn-rate) ;;;;TODO: affect by the level **and** areas spawn rate
      (progn (spawn-creature (nth (random (length (area-creatures (level-areas (player-level user)))))
				  (area-creatures (level-areas (player-level user))))
			     (list (1+ (random (- (cadr (array-dimensions main-map)) 2)))
				   (1+ (random (- (car (array-dimensions main-map)) 2)))))
	     (push-message "You feel as though something was born nearby.")
	     (setf spawn-accumulator 0))
      )
  )

(defun spawn-creature (creature-structure target-point)
  (let ((x (car target-point))
	(y (cadr target-point)))
    (if (and (> y 0)
	     (< y (car (array-dimensions main-map)))
	     (> x 0)
	     (< x (cadr (array-dimensions main-map))))
	(progn (setf creatures (append creature-structure creatures)
		     (aref enemy-array y x) (last creatures)
		     (cadr (assoc :x (entity-position (last creatures)))) x
		     (cadr (assoc :y (entity-position (last creatures)))) y)
	       )
	(spawn-creature creature-structure (list (1+ (random (- (cadr (array-dimensions main-map)) 2)))
						 (1+ (random (- (car (array-dimensions main-map)) 2))))
			))))