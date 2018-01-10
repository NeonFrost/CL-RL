#|
File for the 'entity' structures
|#
(defstruct player
  (name "Nika")
  (level "Forest")
  (room 1)
  (inventory (make-inventory))
  )

(defstruct entity
  name
  (attack 10)
  (attack-mod 0)
  (defense 0)
  (defense-mod 0)
  (range-attack 10)
  (range-attack-mod 0)
  (magic-attack 10)
  (magic-attack-mod 0)
  (magic-defense 10)
  (magic-defense-mod 0)
  (agility 1)
  (agility-mod 0)
  (speed 1) ;;;;in Rogue-like engines, this will be how many spaces an entity can move
  (speed-mod 0)
  (dodge 0)
  (dodge-mod 0)
  (hp 40)
  (max-hp 40)
  (mp 40)
  (max-mp 40)
  (xp 0)
  (level 1)
  elemental
  (status 'alive)
  extra-effect ;;;;i.e.
  current-action
  (symbol "E")
  (line-of-sight 10) ;meaning, it can see x amount of tiles away
  (position '((:x 0)
	      (:y 0)))
  weapon
  armor
  (bg-color +black+)
  (symbol-color +white+)
  )

(defmacro entity-x (entity)
  `(cadr (assoc :x (entity-position ,entity))))
(defmacro entity-y (entity)
  `(cadr (assoc :y (entity-position ,entity))))

(defstruct (creature (:include entity (elemental 'creature) (symbol "c") (weapon 'claws)))
  (tasks '(move))
  (stance 'neutral)
  abilities
  (information "A creature. No further information available."))

(defstruct (player-character (:include entity (elemental 'player) (symbol "@")))
  (energy 10)
  (max-energy 10)
  ranged-weapon
  )

(defvar user (make-player))

(defvar player (make-player-character :position '((:x 35) (:y 35)) :bg-color +navy-blue+ :symbol-color +yellow-zinc+))
(setf (cursor-x cursor) (cadr (assoc :x (entity-position player)))
      (cursor-y cursor) (cadr (assoc :y (entity-position player))))
(defweapon carbonium-axe "Carbon-Cobalt Alloy Axe" :axe 30 -5 5 400 :information "An axe made from an alloy of carbon and cobalt. It's edge is durable and sharp enough to cut through even the densest of diamonds. It is used both in the battlefield and for cutting gems.")
(setf (entity-weapon player) (make-carbonium-axe))


(defvar enemy (make-entity :position '((:x 40) (:y 31)) :bg-color +chalk-white+ :symbol-color +navy-blue+))
(defstruct (socra (:include creature (name "Socra") (symbol "S") (weapon 'spit) (information "A fucking pile of slime"))))
