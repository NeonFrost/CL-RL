(defvar equipment-menus nil)
(defvar equipment-buffer nil)
(defvar equipment-title-buffer nil)
(define-menu equipment-title equipment-menus
  0 0
  *screen-width*
  48
  +pastel-grey+
  +black+)
(define-menu equipment-items equipment-menus
  0 56
  *screen-width*
  (- *screen-height* 48)
  +pastel-grey+
  +black+)
(define-screen equipment-screen equipment-menus)

(defun render-equip-screen ()
  (equipment-screen)
  (let ((name (case equip-state
		(weapons "Weapons")
		(armor "Armor")))
	(len (case equip-state
	       (weapons (length "Weapons"))
	       (armor (length "Armor")))))
    (if (not equipment-title-buffer)
	(setf equipment-title-buffer (create-text-buffer name 0 0
							 :width (* (length name) (car character-size))
							 :height (cadr character-size) #|(menu-height equipment-title)|# :to-texture t :string-case 'text)))
    (tex-blit equipment-title-buffer
	      :src (sdl2:make-rect 0 0
				   (sdl2:texture-width equipment-title-buffer)
				   (sdl2:texture-height equipment-title-buffer))
	      :dest (sdl2:make-rect (- (round (- (/ *screen-width* 2) (/ (length name) 2))) 20)
				    (menu-y equipment-title)
				    (+ (sdl2:texture-width equipment-title-buffer) 40)
				    (menu-height equipment-title))))
  (let ((inventory (case equip-state
		     (armor (inventory-armor players-inventory))
		     (weapons (inventory-weapons players-inventory)))))
    (let ((string-color (case equip-state
			  (armor +cobalt+)
			  (weapons +purple-jade+))))
      (if (not equipment-buffer)
	  (let ((str ""))
	    (loop for item in inventory
	       do (setf str (concatenate 'string str (item-name item)))
		 (setf str (with-output-to-string (stream)
			     (write-string str stream)
			     (terpri stream))))
	    (setf equipment-buffer (create-text-buffer str
						       0 0
						       :width (menu-width inventory-menu-items)
						       :height (menu-height inventory-menu-items)
						       :to-texture t
						       :string-case 'text))))
      (render-buffer equipment-buffer equipment-items :color string-color))))
