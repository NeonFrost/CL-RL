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

;;Now for some 'helper' functions

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
	  (sdl2:free-rect ,src)
	  (sdl2:free-rect ,dest)
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
    (sdl2:free-rect src-rect)
    (sdl2:free-rect dest-rect)
    ))

(defmacro reset-text-buffer (buffer)
  `(if ,buffer
       (progn (sdl2:destroy-texture ,buffer)
	      (setf ,buffer nil)))
  )

(defun render-buffer (buffer menu &key color)
  (sdl2:set-texture-color-mod buffer (car color) (cadr color) (caddr color))
  (let ((src (sdl2:make-rect 0
			     0
			     (sdl2:texture-width buffer)
			     (sdl2:texture-height buffer)
			     ))
	(dest (sdl2:make-rect (+ (menu-x menu) 8)
			      (+ (menu-y menu) 8)
			      (- (menu-width menu) 8)
			      (- (menu-height menu) 8)
			      )))
    (sdl2:render-copy renderer
		      buffer
		      :source-rect src				    
		      :dest-rect dest
		      )
    (sdl2:free-rect src)
    (sdl2:free-rect dest)))

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
     (sdl2:free-rect rect)
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
     )
  )

(defmacro draw-battle-menu (x y w h color)
  `(progn (draw-box ,x ,y ,w ,h ,color)
	  (draw-rectangle ,x ,y ,w ,h *white*))
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
