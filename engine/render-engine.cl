(defvar room-buffer nil)
(defvar player-buffer nil)
(defvar enemy-buffer nil)
(defvar item-buffer nil)
(defvar status-buffer nil)
(defvar cursor-buffer nil)
(defvar information-buffer nil)
(defvar message-buffer nil)

(defstruct level
  name
  type 
  music
;;;;Not in use with 'non-graphical' rogue-likes  sheet ;A path
  (battle-chance 0)
  monster-list
  world-xy
  sheet-surface
  (rooms (make-hash-table)))

(defstruct area
  name
  array
  transitions
  chests
  shop
  connected-areas
  connected-level 
  spring)

(defun render-map (x y max-characters)
  (render-box (+ (menu-x map-menu) (/ (car character-size) 2))
	      (+ (menu-y map-menu) (/ (cadr character-size) 2))
	      (- (menu-width map-menu) (/ (car character-size) 2))
	      (- (menu-height map-menu) (/ (cadr character-size) 2))
	      :color (list 0 20 0 255))
  (tex-blit room-buffer
	    :src (sdl2:make-rect x
				 y
				 (* (car max-characters) (car character-size))
				 (* (cadr max-characters) (cadr character-size)))
	    :dest (sdl2:make-rect (+ (menu-x map-menu) (/ (car character-size) 2))
				  (+ (menu-y map-menu) (/ (cadr character-size) 2))
				  (- (menu-width map-menu) (/ (car character-size) 2))
				  (- (menu-height map-menu) (/ (cadr character-size) 2)))
	    ))
#|  (let ((row-start (if (< (- (cadr (assoc :y (entity-position player))) (/ (cadr mc) 2) 0))
0
(if (> (+ (cadr (assoc :y (entity-position player))) (/ (cadr mc) 2)) (- (car (array-dimensions main-map)) (/ (cadr mc) 2)))
    (- (car (array-dimensions main-map)) (cadr mc))
    (- (cadr (assoc :y (entity-position player))) (/ (cadr mc) 2)))))
(row-end (if (< (- (cadr (assoc :y (entity-position player))) (/ (cadr mc) 2) 0))
	     (cadr mc)
	     (if (> (+ (cadr (assoc :y (entity-position player))) (/ (cadr mc) 2)) (- (1- (car (array-dimensions main-map))) (/ (cadr mc) 2)))
		 (1- (car (array-dimensions main-map)))
		 (+ (cadr (assoc :y (entity-position player))) (/ (cadr mc) 2)))))
(column-start (if (< (- (cadr (assoc :x (entity-position player))) (/ (car mc) 2) 0))
		  0
		  (if (> (+ (cadr (assoc :x (entity-position player))) (/ (car mc) 2)) (- (1- (cadr (array-dimensions main-map))) (/ (car mc) 2)))
		      (- (cadr (array-dimensions main-map)) (car mc))
		      (- (cadr (assoc :x (entity-position player))) (/ (car mc) 2)))))
(column-end (if (< (- (cadr (assoc :x (entity-position player))) (/ (car mc) 2) 0))
		(car mc)
		(if (> (+ (cadr (assoc :x (entity-position player))) (/ (car mc) 2)) (- (1- (cadr (array-dimensions main-map))) (/ (car mc) 2)))
		    (1- (cadr (array-dimensions main-map)))
		    (+ (cadr (assoc :x (entity-position player))) (/ (car mc) 2)))))
)		 
(loop for r from row-start to row-end
   for dx from 0 to (car mc)
   do (loop for c from column-start to column-end
	 for dy from 0 to (cadr mc)
	 do (render-box (* c (car character-size))
			(* r (cadr character-size))
			(car character-size)
			(cadr character-size)
			:color +natural-green+)
	   (tex-blit room-buffer
		     :src (sdl2:make-rect (* c (car character-size))
					  (* r (cadr character-size))
					  (car character-size)
					  (cadr character-size))
		     :dest (sdl2:make-rect (+ (menu-x map-menu) 8 (* dx (car character-size)))
					   (+ (menu-y map-menu) 8 (* dy (cadr character-size)))
					   (car character-size)
					   (cadr character-size)
					   )
		     :color (list 186 134 90 255)    
		     )
	   )
     )
))|#

(defun set-status-string (str)
  (start-string str
		(concatenate 'string "H: " (write-to-string (entity-hp player)) "/" (write-to-string (entity-max-hp player)))
		(concatenate 'string "E: " (write-to-string (player-character-energy player)) "/" (write-to-string (player-character-max-energy player)))
		(concatenate 'string "X: " (write-to-string (entity-xp player)))
		(concatenate 'string "L: " (write-to-string (entity-level player))))
  )

(defun render-stats ()
  (let ((x (menu-x status-menu))
	(y (menu-y status-menu))
	(width (menu-width status-menu))
	(height (menu-height status-menu)))
    (if (not status-buffer)
	(let ((str ""))
	  (setf str (set-status-string str))
	  (setf status-buffer (create-text-buffer str 0 0 :width (+ width (* (car character-size) 2)) :height (+ (cadr character-size) height) :to-texture t :string-case 'text))
	  ))
    (sdl2:render-copy renderer
		      status-buffer
		      :source-rect (sdl2:make-rect 0
						   0
						   (sdl2:texture-width status-buffer) 
						   (sdl2:texture-height status-buffer))
		      :dest-rect (sdl2:make-rect (+ x (/ (car character-size) 2))
						 (+ y (/ (cadr character-size) 2))
						 width (- height 16))
		      )))

(defun render-item-information ()
  (item-screen)
  (let* ((item (case inventory-state
		 (items (nth selection (inventory-items players-inventory)))
		 (armor (nth selection (inventory-armor players-inventory)))
		 (weapons (nth selection (inventory-weapons players-inventory)))))
	 (info (item-information item))
	 (name (item-name item)))
    (if (not information-buffer)
	(setf information-buffer (create-text-buffer (start-string name "" info)  0 0
						     :width (menu-width information-menu)
						     :height (menu-height information-menu)
						     :to-texture t :string-case 'text)))
    (if information-buffer
      (tex-blit information-buffer
		:src (sdl2:make-rect 0 0
				     (sdl2:texture-width information-buffer)
				     (sdl2:texture-height information-buffer))
		:dest (sdl2:make-rect (+ (menu-x information-menu) (/ (car character-size) 2))
				      (+ (menu-y information-menu) (/ (cadr character-size) 2))
				      (- (menu-width information-menu) (/ (car character-size) 2))
				      (- (menu-height information-menu) (/ (cadr character-size) 2)))
		))
    )
  )

(defun render-information ()
  (let ((info (if (eq sub-state 'enemy-information)
		  (creature-information (aref enemy-array (cursor-y cursor) (cursor-x cursor)))
		  (item-information (aref item-array (cursor-y cursor) (cursor-x cursor)))
		  ))
	(name (if (eq sub-state 'enemy-information)
		  (creature-name (aref enemy-array (cursor-y cursor) (cursor-x cursor)))
		  (item-name (aref item-array (cursor-y cursor) (cursor-x cursor)))
		  ))
	) 
    (if (not information-buffer)
	(setf information-buffer (create-text-buffer (start-string name "" info)  0 0
						     :width (menu-width information-menu)
						     :height (menu-height information-menu)
						     :to-texture t :string-case 'text))))
  (item-screen)
  (if information-buffer
      (tex-blit information-buffer
		:src (sdl2:make-rect 0 0
				     (sdl2:texture-width information-buffer)
				     (sdl2:texture-height information-buffer))
		:dest (sdl2:make-rect (+ (menu-x information-menu) (/ (car character-size) 2))
				      (+ (menu-y information-menu) (/ (cadr character-size) 2))
				      (- (menu-width information-menu) (/ (car character-size) 2))
				      (- (menu-height information-menu) (/ (cadr character-size) 2)))
		))
  )

(defun render-message ()
  (tex-blit message-buffer
	    :dest (sdl2:make-rect (menu-x message-menu)
				  (menu-y message-menu)
				  (menu-width message-menu)
				  (menu-height message-menu))
	    )
  )

(defun render-level () 
  "This is temporary"
  (room-screen)
  (render-stats)
  (if message-buffer
      (render-message))
  (if (or (eq sub-state 'look)
	  (eq sub-state 'target))
      (if (not cursor-buffer)
	  (progn (get-cursor-information)
		 (setf cursor-buffer (create-text-buffer cursor-information 0 0 :width (* 20 16) :height (* 5 16) :to-texture t :string-case 'text)))
	  (tex-blit cursor-buffer
		    :src (sdl2:make-rect 0 0
					 (sdl2:texture-width cursor-buffer)
					 (sdl2:texture-height cursor-buffer))
		    :dest (sdl2:make-rect (+ (menu-x cursor-menu) (/ (car character-size) 2))
					  (+ (menu-y cursor-menu) (/ (cadr character-size) 2))
					  (- (menu-width cursor-menu) (/ (car character-size) 2))
					  (- (menu-height cursor-menu) (/ (cadr character-size) 2)))
		    )))
  (let* ((x-offset (ceiling (/ (- (menu-width map-menu) (/ (car character-size) 2)) (1+ (car max-characters)))))
	 (y-offset (ceiling (/ (- (menu-height map-menu) (/ (cadr character-size) 2)) (1+ (cadr max-characters)))))
	 )
    (if (not room-buffer)
	(let ((str ""))
	  (loop for y from 0 to (1- (car (array-dimensions main-map)))
	     do (loop for x from 0 to (1- (cadr (array-dimensions main-map)))
		   do (setf str (concatenate 'string str (aref main-map y x)))
		     )
	       (setf str (with-output-to-string (stream)
			   (write-string str stream)
			   (terpri stream)
			   ))
		   ;;;;(setf str (concatenate 'string str "\n"))
	       )
	  (setf room-buffer (create-text-buffer str 0 0 :width (* (cadr (array-dimensions main-map)) (car character-size)) :height (* (car (array-dimensions main-map)) (cadr character-size)) :to-texture t))
	  )
	)
    (if (not player-buffer)
	(progn (setf player-buffer (create-text-buffer (entity-symbol player) 0 0 :width (car character-size) :height (cadr character-size) :to-texture t))
	       (sdl2:set-texture-color-mod player-buffer (car (entity-symbol-color player)) (cadr (entity-symbol-color player)) (caddr (entity-symbol-color player)))
	))
    (if (not enemy-buffer)
	(let ((str ""))
	  (loop for y from 0 to (1- (car (array-dimensions enemy-array)))
	     do (loop for x from 0 to (1- (cadr (array-dimensions enemy-array)))
		   do (if (eq (aref enemy-array y x) 0)
			  (setf str (concatenate 'string str (write-to-string (aref enemy-array y x))))
			  (setf str (concatenate 'string str (entity-symbol (aref enemy-array y x))))
		     ))
	       (setf str (with-output-to-string (stream)
			   (write-string str stream)
			   (terpri stream)
			   ))
	       )
	  (setf enemy-buffer (create-text-buffer str 0 0
						  :width (* (cadr (array-dimensions enemy-array)) (car character-size))
						  :height (* (car (array-dimensions enemy-array)) (cadr character-size)) :to-texture t))
	  (sdl2:set-texture-color-mod enemy-buffer 0 205 0))
	)
    (if (not item-buffer)
	(progn (setf item-buffer (create-text-buffer (item-symbol (aref item-array 15 30)) (* (car character-size) 30) (* (cadr character-size) 15)
						     :width (* (cadr (array-dimensions main-map)) (car character-size))
						     :height (* (car (array-dimensions main-map)) (cadr character-size)) :to-texture t))
	       (sdl2:set-texture-color-mod item-buffer 255 0 0))
	)
    (let ((x (if (< (cadr (assoc :x (entity-position player))) (/ (car max-characters) 2))
		 0
		 (if (> (cadr (assoc :x (entity-position player))) (- (cadr (array-dimensions main-map)) (/ (car max-characters) 2)))
		     (* (- (cadr (array-dimensions main-map)) (car max-characters)) (car character-size))
		     (* (- (cadr (assoc :x (entity-position player))) (/ (car max-characters) 2)) (car character-size))
		     )))
	  (y (if (< (cadr (assoc :y (entity-position player))) (/ (cadr max-characters) 2))
		 0
		 (if (> (cadr (assoc :y (entity-position player))) (- (car (array-dimensions main-map)) (/ (cadr max-characters) 2)))
		     (* (- (car (array-dimensions main-map)) (cadr max-characters)) (cadr character-size))
		     (* (- (cadr (assoc :y (entity-position player))) (/ (cadr max-characters) 2)) (cadr character-size))
		     ))))
      (render-map x y max-characters)
      #|    (tex-blit room-buffer
      :src (sdl2:make-rect x
      y
      (* (car max-characters) (car character-size))
      (* (cadr max-characters) (cadr character-size)))
      :dest (sdl2:make-rect (+ (menu-x map-menu) (/ (car character-size) 2))
      (+ (menu-y map-menu) (/ (cadr character-size) 2))
      (- (menu-width map-menu) (/ (car character-size) 2))
      (- (menu-height map-menu) (/ (cadr character-size) 2)))
      :color (list 186 134 90 255)
      )|#
      #|    (sdl2:render-copy renderer
      room-buffer
      :source-rect (sdl2:make-rect x
      y
      (* (car max-characters) (car character-size))
      (* (cadr max-characters) (cadr character-size))
      )				    
      :dest-rect (sdl2:make-rect (+ (menu-x map-menu) (/ (car character-size) 2))
      (+ (menu-y map-menu) (/ (cadr character-size) 2))
      (- (menu-width map-menu) (/ (car character-size) 2))
      (- (menu-height map-menu) (/ (cadr character-size) 2)))
      )|#
      (sdl2:render-copy renderer
			item-buffer
			:source-rect (sdl2:make-rect x
						     y
						     (* (car max-characters) (car character-size))
						     (* (cadr max-characters) (cadr character-size))
						     )				    
			:dest-rect (sdl2:make-rect (+ (menu-x map-menu) (/ (car character-size) 2))
						   (+ (menu-y map-menu) (/ (cadr character-size) 2))
						   (- (menu-width map-menu) (/ (car character-size) 2))
						   (- (menu-height map-menu) (/ (cadr character-size) 2)))
			)
      (sdl2:render-copy renderer
			enemy-buffer
			:source-rect (sdl2:make-rect x
						     y
						     (* (car max-characters) (car character-size))
						     (* (cadr max-characters) (cadr character-size))
						     )				    
			:dest-rect (sdl2:make-rect (+ (menu-x map-menu) (/ (car character-size) 2))
						   (+ (menu-y map-menu) (/ (cadr character-size) 2))
						   (- (menu-width map-menu) (/ (car character-size) 2))
						   (- (menu-height map-menu) (/ (cadr character-size) 2)))
			)
      #|	 (loop for r
      from (- (cadr (assoc :y (entity-position player))) (/ (cadr max-characters) 2))
      to (+ (cadr (assoc :y (entity-position player))) (/ (cadr max-characters) 2))
      do (if (and (> r 0)
      (< r (1- (car (array-dimensions main-map)))))
      (loop for c
      from (- (cadr (assoc :y (entity-position player))) (/ (car max-characters) 2))
      to (+ (cadr (assoc :y (entity-position player))) (/ (car max-characters) 2))
      do (if (and (> c 0)
      (< c (1- (cadr (array-dimensions main-map)))))
      (progn (if (not (eq (aref item-array r c) 0))
      (progn (sdl2:set-texture-color-mod item-buffer 255 0 0)
      (sdl2:render-copy renderer
      item-buffer
      :source-rect (sdl2:make-rect 0 0
      (car max-characters)
      (cadr max-characters))
      :dest-rect (sdl2:make-rect (+ (- (+ c (menu-x map-menu)) (cadr (assoc :x (entity-position player)))) (car max-characters))
      120;(- (+ r (menu-y map-menu)) (* (cadr (assoc :y (entity-position player))) (cadr max-characters)))
      (car max-characters)
      (cadr max-characters))
      ))
      )
      (if (not (eq (aref enemy-array r c) 0))
      (print "enemy!")
      ))
      ))))|#
      )
    (let ((x (if (< (cadr (assoc :x (entity-position player))) (/ (car max-characters) 2))
		 (+ (menu-x map-menu) (* (cadr (assoc :x (entity-position player))) x-offset))  ;;;;(+ (menu-x map-menu) (* (cadr (assoc :x (entity-position player))) x-offset) (round (/ x-offset 4))) 
		 (if (> (cadr (assoc :x (entity-position player))) (- (cadr (array-dimensions main-map)) (/ (car max-characters) 2)))
		     (- (+ (menu-x map-menu) (menu-width map-menu))
			(* (- (cadr (array-dimensions main-map))
			      (cadr (assoc :x (entity-position player))))
			   x-offset)
			(round (/ x-offset 4)))
		     (- (+ (menu-x map-menu) (round (/ (menu-width map-menu) 2))) (round (/ x-offset 8)))
		     )))
	  (y (if (< (cadr (assoc :y (entity-position player))) (/ (cadr max-characters) 2))
		 (+ (menu-y map-menu) (* (cadr (assoc :y (entity-position player))) y-offset))
;;;		 (+ (menu-y map-menu) (* (cadr (assoc :y (entity-position player))) y-offset) (round (/ y-offset 4)))
		 (if (> (cadr (assoc :y (entity-position player))) (- (car (array-dimensions main-map)) (/ (cadr max-characters) 2)))
		     (- (+ (menu-y map-menu) (menu-height map-menu))
			(* (- (car (array-dimensions main-map))
			      (cadr (assoc :y (entity-position player))))
			   y-offset)
			(round (/ y-offset 4)))
		     (- (+ (menu-y map-menu) (round (/ (menu-height map-menu) 2))) (round (/ y-offset 8)))
		     )))
	  (c-x (if (< (cadr (assoc :x (entity-position player))) (/ (car max-characters) 2))
		   (cursor-x cursor)
		   (if (> (cadr (assoc :x (entity-position player))) (- (cadr (array-dimensions main-map)) (/ (car max-characters) 2)))  ;;;;(> (cadr (assoc :x (entity-position player))) (- (cadr (array-dimensions main-map)) (/ (car max-characters) 2)))
		       (+ (- (cursor-x cursor) (- (cadr (array-dimensions main-map)) (/ (car max-characters) 2))) (/ (car max-characters) 2))
;;;;		       (+ (- (cursor-x cursor) (cadr (array-dimensions main-map))) (/ (car max-characters) 2))
		       (+ (- (cursor-x cursor) (cadr (assoc :x (entity-position player)))) (/ (car max-characters) 2)))))
	  (c-y (if (< (cadr (assoc :y (entity-position player))) (/ (cadr max-characters) 2));;;;(cadr (assoc :y (entity-position player))) (/ (cadr max-characters) 2))
		   (cursor-y cursor)
		   (if (> (cadr (assoc :y (entity-position player))) (- (car (array-dimensions main-map)) (/ (cadr max-characters) 2)))  ;;;;(> (cadr (assoc :x (entity-position player))) (- (cadr (array-dimensions main-map)) (/ (car max-characters) 2)))
		       (+ (- (cursor-y cursor) (- (car (array-dimensions main-map)) (/ (cadr max-characters) 2))) (/ (cadr max-characters) 2))
		       (+ (- (cursor-y cursor) (cadr (assoc :y (entity-position player)))) (/ (cadr max-characters) 2)))))
	  )
      (render-box (+ (- (+ x (round (/ x-offset 2))) (round (/ x-offset 4)) 4) (round (/ (car character-size) 4)))
		  (+ (- (+ y (round (/ y-offset 2))) (round (/ y-offset 4)) 4) (round (/ (cadr character-size) 4)))
						   ;(- (+ x (round (/ x-offset 2))) (round (/ x-offset 4)))
		  ;(- (+ y (round (/ y-offset 2))) (round (/ y-offset 4)))
		  x-offset
		  y-offset
		  :color (entity-bg-color player))
      (sdl2:render-copy renderer
			player-buffer
			:source-rect (sdl2:make-rect 0 0
						     (car character-size) (cadr character-size))
			:dest-rect (sdl2:make-rect (+ (- (+ x (round (/ x-offset 2))) (round (/ x-offset 4)) 4) (round (/ (car character-size) 4)))
						   (+ (- (+ y (round (/ y-offset 2))) (round (/ y-offset 4)) 4) (round (/ (cadr character-size) 4)))
						   x-offset
						   y-offset
						   )
			)
      (if (or (eq sub-state 'look)
	      (eq sub-state 'target))
	  (progn (if (< c-x 0)
		     (setf c-x 0))
		 (if (< c-y 0)
		     (setf c-y 0))
		 (if (> c-x (cursor-x cursor) (- (cadr (array-dimensions main-map)) (/ (car max-characters) 2)))
		     (setf c-x (1- c-x)))
		 (if (> c-y (cursor-y cursor) (- (car (array-dimensions main-map)) (/ (cadr max-characters) 2)))
		     (setf c-y (1- c-y)))
		 (render-box (+ (menu-x map-menu) 8 (* c-x x-offset))
			     (- (+ (menu-y map-menu) 10 (* c-y y-offset)) (round (/ y-offset 2)))
			     x-offset
			     y-offset
			     :color (if (eq sub-state 'look)
					(list 255 255 255 255)
					+yellow-zinc+))))
      )))
