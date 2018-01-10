(let ((items (inventory-items (player-inventory player)))
      (weapons (inventory-weapons (player-inventory player)))
      (armor (inventory-armor (player-inventory player)))
      )
  
  (defun add-item (item &key (amount 1))
    (case (item-type item)
      (item (if (position item items)
		(incf (item-amount (nth (position item items) (inventory-items (player-inventory player)))) amount)
		(progn (setf (inventory-items (player-inventory player)) (append items (list item)))
		       (incf (item-amount (nth (position item items) (inventory-items (player-inventory player)))) (1- amount)))
		))
      (weapon (if (position item weapons)
		  (incf  (item-amount (nth (position item weapons) (inventory-weapons (player-inventory player)))) amount)
		  (progn (setf (inventory-weapons (player-inventory player)) (append weapons (list item)))
			 (incf  (item-amount (nth (position item weapons) (inventory-weapons (player-inventory player)))) (1- amount)))
		  ))
      (armor (if (position item armor)
		 (incf (item-amount (nth (position item armor) (inventory-armor (player-inventory player)))) amount)
		 (progn (setf (inventory-armor (player-inventory player)) (append armor (list item)))
			(incf (item-amount (nth (position item armor) (inventory-armor (player-inventory player)))) (1- amount)))
      ))))

  (defun subtract-item (item)
    (case (item-type item)
      (item (decf (item-amount (nth (position item items) (inventory-items (player-inventory player)))) 1)
	    (if (eq (item-amount (nth (position item items) items)) 0)
		(progn (setf (nth (position item items) (inventory-items (player-inventory player))))
		       (remove-nil))))
      (weapon (decf (item-amount (nth (position item weapons) (inventory-weapons (player-inventory player)))) 1)
	      (if (eq (item-amount (nth (position item weapons) weapons)) 0)
		  (progn (setf (nth (position item weapons) (inventory-weapons (player-inventory player))) nil)
			 (remove-nil))))
      (armor (decf (item-amount (nth (position item armor) (inventory-armor (player-inventory player)))) 1)
	     (if (eq (item-amount (nth (position item armor) armor)) 0)
		 (progn (setf (nth (position item armor) (inventory-armor (player-inventory player))))
			(remove-nil))))
      )
    )

  (defun remove-nil ()
    (setf (inventory-items (player-inventory player)) (remove nil items))
    (setf (inventory-weapons (player-inventory player)) (remove nil weapons))
    (setf (inventory-armor (player-inventory player)) (remove nil armor))
    )
  
  ); Closes the items, weapons, armor binding

(defmacro restore (entity item)
  `(case (item-class ,item)
     (health (setf (entity-hp ,entity) (+ (item-restore ,item) (entity-hp ,entity)))
	     (if (> (entity-hp ,entity) (entity-max-hp ,entity))
		 (setf (entity-hp ,entity) (entity-max-hp ,entity))))
     (magic (setf (entity-mp ,entity) (+ (item-restore ,item) (entity-mp ,entity)))
	    (if (> (entity-mp ,entity) (entity-max-mp ,entity))
		(setf (entity-mp ,entity) (entity-max-mp ,entity))))
     (heal-magic (setf (entity-hp ,entity) (+ (item-restore ,item) (entity-hp ,entity)))
		 (if (> (entity-hp ,entity) (entity-max-hp ,entity))
		     (setf (entity-hp ,entity) (entity-max-hp ,entity)))
		 (setf (entity-mp ,entity) (+ (item-restore ,item) (entity-mp ,entity)))
		 (if (> (entity-mp ,entity) (entity-max-mp ,entity))
		     (setf (entity-mp ,entity) (entity-max-mp ,entity))))
     (revive (setf (entity-hp ,entity) (ceiling (/ (entity-max-hp ,entity) 2)))
	     (setf (entity-status ,entity) 'alive))
     (party-health (setf (entity-hp ,entity) (+ (entity-hp ,entity) (item-restore ,item)))
		   (if (> (entity-hp ,entity) (entity-max-hp ,entity))
		       (setf (entity-hp ,entity) (entity-max-hp ,entity))))
     (party-magic (setf (entity-mp ,entity) (+ (item-restore ,item) (entity-mp ,entity)))
		  (if (> (entity-mp ,entity) (entity-max-mp ,entity))
		      (setf (entity-mp ,entity) (entity-max-mp ,entity))))
     (party-heal-magic (setf (entity-hp ,entity) (+ (item-restore ,item) (entity-hp ,entity)))
		       (if (> (entity-hp ,entity) (entity-max-hp ,entity))
			   (setf (entity-hp ,entity) (entity-max-hp ,entity)))
		       (setf (entity-mp ,entity) (+ (item-restore ,item) (entity-mp ,entity)))
		       (if (> (entity-mp ,entity) (entity-max-mp ,entity))
			   (setf (entity-mp ,entity) (entity-max-mp ,entity))))
     ))


(defun use-item (item target)
  (let ((i-t (item-type item)))
    (if (or (eq i-t 'party-health)
	    (eq i-t 'party-magic)
	    (eq i-t 'party-heal-magic))
	(loop for party-member in party
	   do (restore party-member item))
	(restore target item))
    )
  #| (case (item-type item)
  (heal (restore-health target item))
  (magic (restore-magic target item))
  (heal-magic (restore-health target item)
  (restore-magic target item))
  (revive (revive-entity target item))
  ))|#
  (subtract-item item)
  )

(defmacro heal-hp (entity amt)
  `(incf (entity-hp ,entity) ,amt))
(defmacro heal-mp (entity amt)
  `(incf (entity-mp ,entity) ,amt))

(defun heal-party ()
  (loop for m in party
     do (heal-hp m (entity-max-hp m))
       (heal-mp m (entity-max-mp m))
       ))
#|
(defun add-to-inventory (item)
      (cond ((and (eq ((item-type item) 'weapon)) (< (list-length (inventory-weapons (player-inventory player))) 50))
	     (push item (inventory-weapons (player-inventory player)))
	     (setf (item-number item) (+ (length (inventory-weapons (player-inventory player))) 1))
	     )
	    ((and (eq ((item-type item) 'armor)) (< (length (inventory-armor (player-inventory player))) 50))
	     (push item (inventory-armor (player-inventory player)))
	     (setf (item-number item) (+ (length (inventory-armor (player-inventory player))) 1))
	     )
	    ((and (eq ((item-type item) 'item)) (< (length (inventory-items (player-inventory player))) 100))
	     (push item (inventory-items (player-inventory player)))
	     (setf (item-number item) (+ (length (inventory-items (player-inventory player))) 1))
	     )
	    ))
|#
