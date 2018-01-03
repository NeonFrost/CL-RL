(defvar weapon-strings '((:sword "slash")
			 (:hammer "bash")
			 (:axe "hack")
			 (:gun "fire")
			 ))

(defmacro define-command (command-name doc &body body)
  `(defun ,command-name ()
     ,doc
     ,@body)
  ) ;;;;primarily for use with using a 'console' (i.e. shift+c (C) brings up command prompt (>[]) to type in an action (eat) then what object or direction it needs/wants (you wish to eat what?/you wish to attack where?)
(defmacro define-item-action (action-name doc &body body)
  `(defun ,action-name (item)
     ,doc
     (let ((x (cadr (assoc :x (entity-position player))))
	   (y (cadr (assoc :y (entity-postiion player))))
	   (ends-turn t))
       ,@body
     ;;;;(if ends-turn
     ;;;;    (end-turn))
       )))

(defmacro define-action (action-name doc &body body)
  `(defun ,action-name (entity &key (dir 'player))
     ,doc
     (let ((x (cadr (assoc :x (entity-position entity))))
	   (y (cadr (assoc :y (entity-position entity))))
	   (ends-turn t)
	   )
       (reset-messages)
       ,@body
       (create-message)
       (if ends-turn
	   (end-turn))
       )))

(defmacro define-map-action (action-name doc &body body)
  `(defun ,action-name (map &key item)
     ,doc
     (let ((ends-turn t))
       ,@body
;;;;       (if ends-turn (end-turn))
       )))

#|(defmacro add-item (item)
    "Adds an item to the player's inventory"
    `(setf (inventory-items (player-inventory player))
	   (append (list ,item) (inventory-items (player-inventory player)))
	   ))|#

(define-action attack
    "entity attacks in a direction"
  (if (eq (entity-current-action entity) 'defend)
      (setf (entity-defense-mod entity) 0))
  (setf (entity-current-action entity) 'attack)
  (flet ((attack-message (action-name weapon-name direction) (push-message (concatenate 'string
											  "You " action-name
											  " your " weapon-name
											  " to the " direction ".")))
	 )
    (if (not (or (eq dir 5) (eq dir 'p) (eq dir 'player)))
	(progn (attack-message (cadr (assoc (item-class (entity-weapon player)) weapon-strings))
			       (item-name (entity-weapon player))
			       (write-to-string dir))
	       (test-attack player :target-point (list (+ (car (cadr (assoc dir direction-list))) x)
						       (+ (cadr (cadr (assoc dir direction-list))) y)))
	       )
	(push-message "Why attack yourself?")
	)
    (reset-text-buffer enemy-buffer)
#|    (case dir
      ((or 1 sw south-west))
      ((or 2 s south) ())
      ((or 3 se south-east) ())

      ((or 4 w west) ())
      ((or 5 p player) (create-message "Why attack yourself?"))
      ((or 6 e east) ()) ;;;;(+ x 1)

      ((or 7 nw north-west) ())
      ((or 8 n north) ())
      ((or 9 ne north-east) ())
    ;;;;push (monster at x/y +/- 1 depending on dir)
      )|#
    ))

(define-action fire-projectile
    "Entity fires weapon in a direction"
  (let ((can-fire nil))
    (if (player-characterp entity)
	(if (not (player-character-ranged-weapon entity))
	    (progn (push-message "You do not have a weapon equipped")
		   (setf ends-turn nil))
	    (setf can-fire t)
	    )
	(setf can-fire t))
    (if can-fire
	(let ((dir ))
	       (if (eq (entity-current-action entity) 'defend)
		   (setf (entity-defense-mod entity) 0))
	       (if (not (or (eq dir 5) (eq dir 'p) (eq dir 'player)))
		   (push-message (concatenate 'string "You fire " (item-name (player-charcter-ranged-weapon entity)) " to the " (write-to-string dir) "."))
		   (push-message "This isn't Full Metal Jacket."))
	       (test-ranged-attack player :target-point (list (cursor-x cursor) (cursor-y)))
	       (reset-text-buffer enemy-buffer)
	       ))))
	  

(define-action defend
    "Entity defends against an attack"
  (setf (entity-current-action entity) 'defend)
  (setf (entity-defense-mod entity) 10)
  )

(define-action jump
    "Entity jumps in dir, affected by agility (speed, dodge, etc.)"
  (setf (entity-current-action entity) 'jump)
  )

(define-item-action recharge
    "Uses a battery"
  (if (eq (item-class item) :battery)
      (incf (player-energy player) (item-restore item))
      )
  )

(define-item-action eat
    "Eats an item"
  (if (eq (item-class item) :food)
      (progn (render-message (concatenate 'string "You eat the " (item-name item) "."))
	      (subtract-item item))
      (push-message (concatenate 'string "Why are you trying to eat the " (item-name item) "?"))
      ))

(define-map-action grab-item
    "get player-x and player-y (z if necessary)
    Check the tile at (player-x, y and z)
    if there is an item at the players feet and if the item can be picked up (including weight), then it is put into the players inventory"
  (let* ((x (entity-x player))
	 (y (entity-y player))
	 (tile (aref map y x))
	 )
    (if (assoc tile items)
	(let ((item (cadr (assoc tile items)))
	      )
	  (if (< (+ (item-weight item) (iventory-weight (player-inventory player))) (inventory-max-weight (player-inventory player)))
	      (progn (add-item item)
		     (setf (aref map y x) "."))
	      (push-message "You are carrying too much weight, you cannot add the item to your inventory.")
	      ))))
  )
