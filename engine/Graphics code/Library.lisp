(defstruct level
  name
  type 
  music
;;;;Not in use with 'non-graphical' rogue-likes  sheet ;A path
  (battle-chance 0)
  monster-list
;;;;  world-xy
  sheet-surface
  (rooms (make-hash-table)))

(defstruct area
  name
  array
  texture
  transitions
;;;;  chests
;;;;  shop
  connected-areas
  connected-level 
  spring)

(defmacro define-level (var &key (name "Level") (type 'dungeon) (music "default-song.ogg") (battle-chance 0) (monster-list nil))
  (let ((type (write-to-string type)))
    `(defvar ,var (make-level :name ,name :type (intern ,type) :music ,music :battle-chance ,battle-chance :monster-list ,monster-list))
    ))

(defmacro define-area (hash-symbol level &key (name "Area") (array (make-array '(10 10))) (transitions (list 0 0)) (connected-areas nil) (connected-level nil) (spring (list 5 5)))
  (let ((area (make-area :name name :array array
			 :transitions transitions :spring spring
			 :connected-areas connected-areas
			 :connected-level connected-level)))
  `(setf (gethash ,hash-symbol (level-rooms ,level)) ,area)))
								
#|(defstruct chest
  x
  y
  cell
  contents
  opened
  )
(defmacro defchest (level area x y cell contents)
  `(if (area-chests (gethash ,area (level-areas ,level)))
       (setf (area-chests (gethash ,area (level-areas ,level)))
	     (append (list (make-chest :x ,x :y ,y :cell ,cell :contents (list ,contents)) (area-chests (gethash ,area (level-areas ,level))))))
       (setf (area-chests (gethash ,area (level-areas ,level)))
	     (list (make-chest :x ,x :y ,y :cell ,cell :contents (list ,contents))))
       ))|#
#|
======================================================================
                             INVENTORY
                               ITEMS
======================================================================
|#

(defstruct inventory
  weapons
  armor
  items
  weight
  max-weight)

(defun ascii-to-string (code)
 (if (integerp code)
      (concatenate 'string "" (list (code-char code)))
      (concatenate 'string "" (string code)))
  )

(defstruct item
  name
  type 
  (target 'single)
  class
  (can-equip nil)
  (attack 0)
  (defense 0)
  (agility 0)
  (restore 0) 
  (cost 0)
  (amount 0)
  (weight 1)
  (information "")
  (symbol (ascii-to-string 21))
  ;;;;cell
  )

(defmacro defweapon (weapon name class attack defense agility cost &key (symbol 24) (information ""))
  `(defstruct (,weapon (:include item (name ,name) (type 'weapon) (class ,class) (attack ,attack) (defense ,defense) (agility ,agility) (cost ,cost) (symbol (ascii-to-string ,symbol)) (information ,information)))))
(defmacro defarmor (armor name attack defense agility cost class &key (symbol "[") (information ""))
  `(defstruct (,armor (:include item (name ,name) (type 'armor) (class ,class) (attack ,attack) (defense ,defense) (agility ,agility) (cost ,cost) (symbol (ascii-to-string ,symbol)) (information ,information)))))

(defmacro defpotion (potion name &key (target 'single) (class 'healing) (type 'potion) (restore 5) (cost 50))
  (let ((type (write-to-string type))
	(class (write-to-string class))
	(target (write-to-string target)))
    `(defstruct (,potion (:include item (name ,name) (target (intern ,target)) (type (intern ,type)) (class (intern ,class)) (restore ,restore) (cost ,cost))))
    ))

(defmacro defsword (sword name attack defense agility cost)
  `(defstruct (,sword (:include item (name ,name) (type 'weapon) (class 'earth) (attack ,attack) (defense ,defense) (agility ,agility) (cost ,cost)))))
(defmacro defhammer (hammer name attack defense cost)
  `(defstruct (,hammer (:include item (name ,name) (type 'weapon) (class 'metal) (attack ,attack) (defense ,defense) (cost ,cost)))))
(defmacro deffist (fist name attack defense agility cost)
  `(defstruct (,fist (:include item (name ,name) (type 'weapon) (class 'warrior) (attack ,attack) (defense ,defense) (agility ,agility) (cost ,cost)))))
(defmacro defchain (chain name attack defense agility cost)
  `(defstruct (,chain (:include item (name ,name) (type 'weapon) (class 'ice) (attack ,attack) (defense ,defense) (agility ,agility) (cost ,cost)))))

#|(defmacro defitem-cell (item cell)
  `(setf (item-cell ,item) ,cell))|#
#|
======================================================================
                                 NPC
                               PLAYER
                              MONSTERS
                               ENTITY
======================================================================
|#

(defun spawn-creature (creature-structure target-point)
  (let ((x (car target-point))
	(y (cadr target-point)))
    (if (and (> y 0)
	     (< y (car (array-dimensions main-map))))
	(if (and (> x 0)
		 (< x (cadr (array-dimensions main-map))))
	    (setf creatures (append creature-structure creatures)
		  (aref enemy-array y x) (last creatures)
		  (cadr (assoc :x (entity-position (last creatures)))) x
		  (cadr (assoc :y (entity-position (last creatures)))) y)
	    (spawn-creature creature-structure (list (1+ (random (- (cadr (array-dimensions main-map)) 2)))
						     y)))
	(spawn-creature creature-structure (list x
						 (1+ (random (- (car (array-dimensions main-map)) 2)))))
	)))

#|
======================================================================
                               SPELLS
======================================================================
|#

(defstruct spell
  (name nil)
  (cost nil)
  (level 1)
  (target 'single)
  (effect 'attack))

(defmacro defspell (var spell-name cost element lvl-req &key (target 'single) (effect 'attack) (spell-lvl 1))
  (let ((eff (write-to-string effect))
	(tar (write-to-string target)))
  `(progn (defparameter ,var (make-spell :name ,spell-name :cost ,cost :target (intern ,tar) :effect (intern ,eff) :level ,spell-lvl))
	  (setf (gethash ,lvl-req (gethash ,element *legal-spells*)) ,var))
  ))


#|
==============================================================================
                                    BATTLE
==============================================================================
|#

(defmacro decrease-hp (target amount)
  `(decf (entity-hp ,target) ,amount))
(defmacro increase-hp (target amount)
  `(incf (entity-hp ,target) ,amount))
(defmacro decrease-mp (target amount)
  `(decf (entity-mp ,target) ,amount))
(defmacro increase-mp (target amount)
  `(incf (entity-mp ,target) ,amount))
(defmacro change-status (target status)
  `(setf (entity-status ,target) ,status))
(defmacro add-xp (entity amount)
  `(incf (entity-xp ,entity) ,amount))

#|
==============================================================================
                                      GFX
==============================================================================
|#

(defvar tile-buffer nil)

(defstruct sprite-sheet ;used with characters, tiles, etc
  width
  height
  cells
  file
  surface
  texture)

#|
Now for some 'helper' functions
|#

(defmacro optimize-sheet (var)
  `(setf (sprite-sheet-texture ,var) (sdl2:create-texture-from-surface renderer (sprite-sheet-surface ,var)))
  )

(defmacro set-sheet-width (sheet width)
  `(setf (sprite-sheet-width ,sheet) ,width)
  )

(defmacro set-sheet-height (sheet height)
  `(setf (sprite-sheet-height ,sheet) ,height)
  )

(defmacro set-cells (sheet tile-size)
  `(let ((cells (loop for y from 0 to (sprite-sheet-height ,sheet) by (cadr ,tile-size)
		   append (loop for x from 0 to (sprite-sheet-width ,sheet) by (car ,tile-size)
			     collect (list x y (car ,tile-size) (cadr ,tile-size))))
	   ))
     (setf (sprite-sheet-cells ,sheet) cells)
     ))

(defmacro set-sheet-surface (sheet surface)
  `(setf (sprite-sheet-surface ,sheet) ,surface))

(defmacro load-sheet (sheet cell-size)
  `(let* ((filename (sprite-sheet-file ,sheet))
	  (surface (sdl2-image:load-image filename))
	  )
     (set-sheet-height ,sheet (sdl2:surface-height surface))
     (set-sheet-width ,sheet (sdl2:surface-width surface))
     (set-cells ,sheet ,cell-size)
     (set-sheet-surface ,sheet surface)
     (optimize-sheet ,sheet)
     ))

(defmacro defsheet (entity file cell-size)
  `(progn (setf (entity-sheet-surface ,entity) (make-sprite-sheet :file ,file))
	  (load-sheet (entity-sheet-surface ,entity) ,cell-size)
	  ))

(defmacro tex-blit (tex &key (src (cffi:null-pointer)) dest color)
  `(progn (if ,color
	      (sdl2:set-texture-color-mod ,tex (car ,color) (cadr ,color) (caddr ,color))
	      )
	  (sdl2:render-copy renderer
			    ,tex
			    :source-rect ,src
			    :dest-rect ,dest)
	  ))

(defmacro draw-cell (sheet cell x y &key color)
  `(let* ((cells (sprite-sheet-cells ,sheet))
	  (src-rect (sdl2:make-rect (nth 0 (nth ,cell cells))
				    (nth 1 (nth ,cell cells))
				    (nth 2 (nth ,cell cells))
				    (nth 3 (nth ,cell cells))))
	  (tsx (nth 2 (nth ,cell cells)))
	  (tsy (nth 3 (nth ,cell cells)))
	  (dest-rect (sdl2:make-rect ,x
				     ,y
				     tsx
				     tsy))
	  )
     ;;;;(clip-blit (sprite-sheet-surface ,sheet) src-rect screen-surface dest-rect)
     (tex-blit (sprite-sheet-texture ,sheet) :src src-rect :dest dest-rect :color ,color)
     (sdl2:free-rect src-rect)
     (sdl2:free-rect dest-rect)
     )
  )

(defmacro free-sheet (sheet)
  `(progn (sdl2:destroy-texture (sprite-sheet-texture ,sheet))
	  (setf ,sheet nil)))

(defmacro render-box (x y w h &key color)
  `(let* ((color (if (not ,color)
		     '(0 0 0 255)
		     ,color))
	  (r (car color))
	  (g (cadr color))
	  (b (caddr color))
	  (a (cadddr color))
	  (rect (sdl2:make-rect ,x ,y ,w ,h))
	  )
     (sdl2:set-render-draw-color renderer r g b a)
     (sdl2:render-fill-rect renderer rect)
     (sdl2:free-rect rect)
     ))

(defmacro blit (src-surface src-rect dest-surface dest-rect)
  `(sdl2:blit-surface ,src-surface ,src-rect ,dest-surface ,dest-rect)
  )

(defun create-tile-buffer (surface sheet tile x y)
  (let ((src-rect (sdl2:make-rect (nth 0 (nth tile cells))
				  (nth 1 (nth tile cells))
				  (nth 2 (nth tile cells))
				  (nth 3 (nth tile cells))))
	(tsx (nth 2 (nth tile cells)))
	(tsy (nth 3 (nth tile cells)))
	(dest-rect (sdl2:make-rect x
				   y
				   tsx
				   tsy))
	)
  (blit (sprite-sheet-surface sheet) src-rect surface dest-rect)
  ))

(defmacro reset-text-buffer (buffer)
  `(if ,buffer
       (progn (sdl2:destroy-texture ,buffer)
	      (setf ,buffer nil)))
  )

(defun render-buffer (buffer menu &key color)
  (sdl2:set-texture-color-mod buffer (car color) (cadr color) (caddr color))
  (sdl2:render-copy renderer
		    buffer
		    :source-rect (sdl2:make-rect 0
						 0
						 (sdl2:texture-width buffer)
						 (sdl2:texture-height buffer)
						 )				    
		    :dest-rect (sdl2:make-rect (+ (menu-x menu) 8)
					       (+ (menu-y menu) 8)
					       (- (menu-width menu) 8)
					       (- (menu-height menu) 8)
					       ))
  )
		      
;;;;Tile buffer is a texture, create-tile-buffer creates clipped surfaces and then 'renders' them to the texture, which is then used as a single image

#|
==============================================================================
                                    BATTLE
==============================================================================
|#

(defmacro draw-line (x y x2 y2 color)
  `(let ((r (car color))
	 (g (cadr color))
	 (b (caddr color))
	 (a 255)
	 )
     (sdl2:set-render-draw-color screen-surface r g b a)
     (sdl2:render-draw-line ,x ,y ,x2 ,y2)
     ))
(defmacro draw-box (x y w h color)
  `(let ((rect (sdl2:make-rect ,x ,y ,w ,h))
	 (r (car color))
	 (g (cadr color))
	 (b (caddr color))
	 (a 255)
	 )
     (sdl2:set-render-draw-color screen-surface r g b a)
     (sdl2:render-fill-rect screen-surface rect)
;;;; (blit rect nil screen-surface nil)
     )
  )

(defmacro draw-rectangle (x y w h color)
  "Draws the 'outline' of a rectangle"
  `(let ((rect (sdl2:make-rect ,x ,y ,w ,h))
	 (r (car color))
	 (g (cadr color))
	 (b (caddr color))
	 (a 255)
	 )
     (sdl2:set-render-draw-color screen-surface r g b a)
     (sdl2:render-draw-rect screen-surface rect)
;;;;;     (blit rect nil screen-surface nil)
     )
  )

#|(defmacro draw-battle-string (str x y)
;;;;  `(sdl:draw-string-at-* ,str ,x ,y))
  `(let* ((surface (sdl2-ttf:render-text-solid *font* ,str (car *font-color*) (cadr *font-color*) (caddr *font-color*) 0))
	  (texture (sdl2:create-texture-from-surface surface)))
     (free-surface surface)
     (render-copy renderer
		  texture
		  :source-rect (cffi:null-pointer)
		  :dest-rect (make-rect ,x ,y
					(texture-width texture)
					(texture-height texture)))
     (destroy-texture texture)
     ))|#

(defmacro draw-battle-menu (x y w h color)
  `(progn (draw-box ,x ,y ,w ,h ,color)
	  (draw-rectangle ,x ,y ,w ,h *white*))  ;TODO: draw a rectangle, then fill it
  )
#|(defmacro draw-icon (icon-cell x y) ;TODO: update
  `(sdl:draw-surface-at-* *icon-sheet* ,x ,y :cell ,icon-cell))
(defmacro rend-monster (x y monster)
  `(if (> (monster-hp ,monster) 0)
       (sdl:draw-surface-at-* *monsters* (- (round ,x) 64) ,y :cell (monster-cell ,monster))
       ))

#|
==============================================================================
                                    DIALOG
==============================================================================
|#
(defmacro draw-d-box (db)
  `(sdl:draw-surface-at-* ,db 0 (- screen-height 32))
  )
|#
#|(defmacro draw-d-string (str y)
  `(let* ((surface (sdl2-ttf:render-text-solid *font* ,str (car *font-color*) (cadr *font-color*) (caddr *font-color*) 0))
	  (texture (sdl2:create-texture-from-surface surface)))
     (free-surface surface)
     (render-copy renderer
		  texture
		  :source-rect (cffi:null-pointer)
		  :dest-rect (make-rect 64 ,y
					(texture-width texture)
					(texture-height texture)))
     (destroy-texture texture)
     ))|#
;;;;(sdl:draw-string-at-* ,str 4 (- screen-height ,y))
#|
==============================================================================
				     MENUS
==============================================================================
|#
#|
(defmacro display-menu (cell x y)
  `(sdl:draw-surface-at-* panes ,x ,y :cell ,cell))
|#
#|
==============================================================================
				     LEVELS
==============================================================================
|#
#|(defmacro draw-tile (tile-set tile x y)
"Draws a tile at x and y"
`(if (eq state 'area)
(sdl:draw-surface-at-* ,tile-set (+ (* tile-size (- ,x (player-local-x player))) offset-x) (+ (* tile-size (- ,y (player-local-y player))) offset-y) :cell ,tile)
(sdl:draw-surface-at-* ,tile-set (+ (* tile-size (- ,x (player-world-x player))) offset-x) (+ (* tile-size (- ,y (player-world-y player))) offset-y) :cell ,tile)
))|#

#|
(defmacro draw-box-a (x y w h c a)
  `(sdl:draw-box-at-* ,x ,y ,w ,h :color ,c :alpha ,a))
|#
