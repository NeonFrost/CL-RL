(defvar inventory-buffer nil)
(defvar inventory-menus nil)
(defvar inventory-state 'items)
(defvar players-inventory nil)

(defpotion potion-of-curing "Potion of Curing" :target 'single :class 'heal-hp :restore 20 :cost 0)
(defpotion potion-of-health "Potion of Health" :target 'single :class 'heal-hp :restore 50 :cost 0)
(defpotion cure-all "Cure all" :target 'single :class 'heal-hp-remove-effects :restore 100 :cost 0)
(defarmor leather-cuirass "Leather Cuirass" 0 5 1 200 'warrior :symbol 21 :information "A Cuirass made from chipmunk leather.
A rather...unique item.")
(defhammer silver-hammer "Silver War Hammer" 20 5 600)

(defun set-players-inventory ()
  (setf players-inventory (make-inventory))
  (push (make-potion-of-curing) (inventory-items players-inventory))
  (push (make-potion-of-curing) (inventory-items players-inventory))
  (push (make-potion-of-health) (inventory-items players-inventory))
  (push (make-cure-all) (inventory-items players-inventory))
  (push (make-leather-cuirass) (inventory-armor players-inventory))
  (push (make-silver-hammer) (inventory-weapons players-inventory))
  )
(set-players-inventory)

(define-menu inventory-menu-items inventory-menus
  0 0
  *screen-width* *screen-height*
  +pastel-grey+ +black+)

(define-screen inventory-screen inventory-menus)

(defun render-inventory ()
  (inventory-screen)
  (render-box (menu-x inventory-menu-items) (+ (* selection (cadr character-size)) (round (/ (cadr character-size) 2)) (menu-y inventory-menu-items)) (- (menu-width inventory-menu-items) 8) (cadr character-size) :color +dark-natural-green+)
  (let ((inventory (case inventory-state
		     (items (inventory-items players-inventory))
		     (armor (inventory-armor players-inventory))
		     (weapons (inventory-weapons players-inventory)))))
    (let ((*string-color* (case inventory-state
			    (items +yellow-zinc+)
			    (armor +cobalt+)
			    (weapons +purple-jade+))))
      (if (not inventory-buffer)
	  (let ((str ""))
	    (loop for item in inventory
	       do (setf str (concatenate 'string str (item-name item)))
		 (setf str (with-output-to-string (stream)
			     (write-string str stream)
			     (terpri stream)))
		 )
	    (setf inventory-buffer (create-text-buffer str
						       0 0
						       :width (menu-width inventory-menu-items)
						       :height (menu-height inventory-menu-items)
						       :to-texture t
						       :string-case 'text))))
      (render-buffer inventory-buffer inventory-menu-items :color *string-color*))))
