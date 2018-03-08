#|
starting sequence
(ql:quickload :sdl2) (ql:quickload :sdl2-image) (ql:quickload :sdl2-mixer) (load "Main.lisp") (main)
|#
(proclaim '(optimize (speed 3) (debug 0)))

(defvar screen-surface nil)
(defvar *screen-width* 1680)
(defvar *screen-height* 1050)
(defvar state 'title)
(defvar sub-state 'top)
(defvar renderer nil)
(defvar accumulator 0)
(defvar selection 0)
(defvar *font* nil)
(defvar *font-color* '(255 255 255 0))
(defvar item-array nil)
(defvar enemy-array nil)
(defvar creatures nil)
(defvar max-characters '(40 40))
(defvar buffers nil)

(defun init-engine ()
  (load "engine.cl")
  )

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "S'daia"
			      :w *screen-width*
			      :h *screen-height*
			      :flags '(:shown))
      (sdl2:with-renderer (default-renderer window :flags '(:renderer-accelerated))
	(sdl2-mixer:init :ogg)
	(sdl2-mixer:open-audio 44100 :s16sys 2 1024)
	(setf renderer (sdl2:get-renderer window))
	(init-engine)
	(sdl2:with-event-loop (:method :poll)
	  (:keydown (:keysym keysym)
		    (keydown-check (sdl2:scancode keysym))
		    )
	  (:keyup (:keysym keysym)
		  (keyup-check (sdl2:scancode keysym))
		  )
	  (:idle ()
		 (sdl2:set-render-draw-color renderer 0 0 0 255)
		 (sdl2:render-clear renderer)
		 (game-loop)
		 (sdl2:render-present renderer)
		 (sdl2:delay 16)
		 (gc :full t)
		 )
	  (:quit ()
		 (quit-audio)
		 (kill-textures)
		 t)
	  )))))

(defun game-loop ()
  (test-music)
  (case state
    (title (title-loop))
    (level (level-loop))
    (equip (equip-loop))
    (paused (paused-loop))
    (inventory (inventory-loop))
    (credits (credits-loop))
    (game-over (game-over-loop))
    ))

(defun kill-textures () ;;start pushing buffer names to a variable and loop through that variable on close
  (loop for buffer in buffers
     do (if buffer
	    (sdl2:destroy-texture buffer)))
  (if +font-sheet+
      (progn (sdl2:free-surface +font-sheet+)
	     (setf +font-sheet+ nil)))
  )

(defun create-exec ()
  (sb-ext:save-lisp-and-die "main" :toplevel #'main :executable t)
  )
