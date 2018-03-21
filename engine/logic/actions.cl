(defvar weapon-strings '((:sword "slash")
			 (:hammer "bash")
			 (:axe "hack")
			 (:knife "stab")
			 (:gun "fire")
			 (:spit "spits")
			 (:claws "swipes")
			 (:teeth "bites")
			 (:talons "slashes")
			 (:drill "drills")
			 (:tusks "heaves")
			 (:horns "rams")
			 ))

(defmacro define-command (command-name doc &body body)
  `(defun ,command-name ()
     ,doc
     ,@body)
  ) ;;;;primarily for use with using a 'console' (i.e. shift+c (C) brings up command prompt (>[]) to type in an action (eat) then what object or direction it needs/wants (you wish to eat what?/you wish to attack where?)
(defmacro define-item-action (action-name doc &body body)
  `(defun ,action-name (item)
     ,doc
     (let ((x (entity-x player));;(cadr (assoc :x (entity-position player))))
	   (y (entity-y player));;(cadr (assoc :y (entity-postiion player))))
	   (ends-turn t))
       (if (player-character-p entity)
	   (reset-messages))
       ,@body
       (if ends-turn (end-turn))
       )))

(defmacro define-action (action-name doc &body body)
  `(defun ,action-name (entity &key (dir 'player))
     ,doc
     (let ((x (cadr (assoc :x (entity-position entity))))
	   (y (cadr (assoc :y (entity-position entity))))
	   (ends-turn t))
       (if (player-character-p entity)
	   (reset-messages))
       ,@body
       (if (and messages
		(not (string= messages "")))
	   (create-message))
       (if (not (player-character-p entity))
	   (reset-text-buffer enemy-buffer))
       (if ends-turn (end-turn))
       )))

(defmacro define-map-action (action-name doc &body body)
  `(defun ,action-name (entity &key item)
     ,doc
     (let ((area-map (area-array (gethash (player-area user) (level-rooms (player-level user)))))
	   (ends-turn t))
       (if (player-character-p entity)
	   (reset-messages))
       ,@body
       (create-message)
       (if ends-turn (end-turn))
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
											(if (player-character-p entity)
											    "You "
											    (concatenate 'string "The " (entity-name entity) " "))
											action-name
											(if (player-character-p entity)
											    " your "
											    " its ")
											weapon-name
											" to the " direction "."))))
    (if (not (or (eq dir 5) (eq dir 'p) (eq dir 'player)))
	(progn (attack-message (if (player-character-p entity)
				   (cadr (assoc (item-class (entity-weapon entity)) weapon-strings))
				   (cadr (assoc (entity-weapon entity) weapon-strings)))
			       (if (player-character-p entity)
				   (item-name (entity-weapon entity))
				   (write-to-string (entity-weapon entity)))
			       (write-to-string dir))
	       (if (test-attack player :target-point (list (+ (car (cadr (assoc dir direction-list))) x)
							   (+ (cadr (cadr (assoc dir direction-list))) y)))
		   (setf ends-turn t))
	       (setf ends-turn t)
	       )
	(progn (setf ends-turn nil)
	       (if (eq (car current-turn) 'p)
		   (push-message "Why attack yourself?")))
	)))

(define-action fire-projectile
    "Entity fires weapon in a direction"
  (let ((can-fire nil))
    (if (player-character-p entity)
	(if (not (player-character-ranged-weapon entity))
	    (progn (push-message "You do not have a ranged weapon equipped.")
		   (setf ends-turn nil))
	    (setf can-fire t)
	    )
	(setf can-fire t))
    (if can-fire
	(let ((dir ))
	  (if (eq (entity-current-action entity) 'defend)
	      (setf (entity-defense-mod entity) 0))
	  (if (not (or (eq dir 5) (eq dir 'p) (eq dir 'player)))
	      (progn (push-message (concatenate 'string "You fire " (item-name (player-charcter-ranged-weapon entity)) " to the " (write-to-string dir) "."))
;;;;		     (test-ranged-attack player :target-point (list (cursor-x cursor) (cursor-y)))
		     )
	      (push-message "This isn't Full Metal Jacket."))
	  ))))


(define-action defend
    "Entity defends against an attack"
  (setf (entity-current-action entity) 'defend)
  (setf (entity-defense-mod entity) 10)
  )

(define-action jump
    "Entity jumps in dir, affected by agility (speed, dodge, etc.)"
  (setf ends-turn nil)
  ;;(setf (entity-current-action entity) 'jump)
  (push-message "You jump in the air...to no avail. You're not superman after all!")
  )

(define-item-action use
    "uses an item"
  (case (item-type item)
    (:battery (incf (player-energy player) (item-restore item)))
    (:food (push-message (concatenate 'string "You eat the " (item-name item) "."))
	   (subtract-item item))
    ((:potion :vial) ())
    (:weapon (setf ends-turn nil)
	     (push-message "You cannot //use// a weapon, however you can (e)quip it."))
    (:armor (setf ends-turn nil)
	    (push-message "You cannot //use// armor, however you can (E)quip it."))
    (otherwise (setf ends-turn nil)
	       (push-message "Why are you trying to use that?"))
    ))

(define-map-action grab-item
    "get player-x and player-y (z if necessary)
    Check the tile at (player-x, y and z)
    if there is an item at the players feet and if the item can be picked up (including weight), then it is put into the players inventory"
  (let* ((x (entity-x player))
	 (y (entity-y player))
	 (tile (aref item-array y x))
	 )
    (if (assoc tile items)
	(let ((item (cadr (assoc tile items))))
	  (if (< (+ (item-weight item) (iventory-weight (player-inventory player))) (inventory-max-weight (player-inventory player)))
	      (progn (add-item item)
		     (setf (aref item-array y x) 0))
	      (push-message "You are carrying too much weight, putting the item into your bag will create a temporary temporal shift at local coordinates with forces so strong that it would collapse local space.")
	      )))))
