(defstruct menu
  x
  y
  width
  height)
(defmacro define-menu (name screen-menus x y w h border-color fill-color)
  `(progn (push (quote ,name) ,screen-menus)
	  (defvar ,name (make-menu :x ,x :y ,y :width (- ,w 8) :height (- ,h 8)))
	  (defun ,name ()
	    (let ((rect1 (sdl2:make-rect ,x ,y ,w ,h))
		  (rect2 (sdl2:make-rect (+ ,x 4) (+ ,y 4) (- ,w 8) (- ,h 8))))
	      (sdl2:set-render-draw-color renderer (car ,border-color) (cadr ,border-color) (caddr ,border-color) (cadddr ,border-color))
	      (sdl2:render-fill-rect renderer rect1)
	      (sdl2:set-render-draw-color renderer (car ,fill-color) (cadr ,fill-color) (caddr ,fill-color) (cadddr ,fill-color))
	      (sdl2:render-fill-rect renderer rect2)
	      (sdl2:free-rect rect1)
	      (sdl2:free-rect rect2))
	    )))
(defmacro define-screen (name menus)
  `(defun ,name ()
     (loop for menu in ,menus
	do (funcall menu))))
;;;;Usage: (define-menu stats-menu room-menus (- *sw* 128) 0 128 (- *sh* 128) '(127 127 127 127) +yellow-zircon+)
;;;;(define-screen room-screen room-menus)
;;;;(case state (battle (case sub-state (top (top-battle-screen)))))
;;;;

#|		 
(defun display-screen (menus)
  (loop for menu in menus
     do (funcall menu)
       )
  )
|#
#|
(defmacro display-menu (cell x y)
  `(sdl:draw-surface-at-* panes ,x ,y :cell ,cell))
(defmacro display-string (string x y)
  `(sdl:draw-string-at-* ,string ,x ,y))

(defun pause-menu ()
  )

(defun inventory-menu ()
  (display-string "WEAPONS" (- (/ screen-width 4) 64) 6)
  (display-string "ARMOR" (- (* (/ screen-width 4) 2) 64) 6)
  (display-string "ITEMS" (- (* (/ screen-width 4) 3) 64) 6)
  (case selection
    (0 (loop for str in (inventory-weapons (player-inventory player))
	  do (loop for x below 2
		do (loop for y below (length (inventory-weapons (player-inventory player)))
		      do (draw-string (item-name str) (+ (* x 256) 12) (* y 36))
			))))
    (1  (loop for str in (inventory-armor (player-inventory player))
	   do (loop for x below 2
		 do (loop for y below (length (inventory-armor (player-inventory player)))
		       do (draw-string (item-name str) (+ (* x 256) 12) (* y 36))
			 ))))
    (2 (loop for str in (inventory-items (player-inventory player))
	  do (loop for x below 2
		do (loop for y below (length (inventory-items (player-inventory player)))
		      do (draw-string (item-name str) (+ (* x 256) 12) (* y 36))
			))))
    ))
  
  #|  (cond ((eq selection 0)
  (loop for str in (inventory-weapons (player-inventory player))
  do (loop for x below 2
  do (loop for y below (length (inventory-weapons (player-inventory player)))
  do (draw-string (item-name str) (+ (* x 256) 12) (* y 36))
  ))))
  ((eq selection 1)
  (loop for str in (inventory-armor (player-inventory player))
  do (loop for x below 2
  do (loop for y below (length (inventory-armor (player-inventory player)))
  do (draw-string (item-name str) (+ (* x 256) 12) (* y 36))
  ))))
  
  ((eq selection 2)
  (loop for str in (inventory-items (player-inventory player))
  do (loop for x below 2
  do (loop for y below (length (inventory-items (player-inventory player)))
  do (draw-string (item-name str) (+ (* x 256) 12) (* y 36))
  ))))
  )
|#

(defun equip-menu (current-member)
  )
|#
