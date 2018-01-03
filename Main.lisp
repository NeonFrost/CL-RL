#|
starting sequence
(ql:quickload :sdl2) (ql:quickload :sdl2-mixer) (ql:quickload :sdl2-image) (load "Main.lisp") (main)
|#
;;;;TYPE C-c C-c to send an //slime interrupt// to sbcl, so that it will evaluate whatever code you wrote
(defvar screen-surface nil)
(defvar *screen-width* 1680)
(defvar *screen-height* 1050)
(defvar state 'level)
(defvar sub-state 'top)
(defvar renderer nil)
(defvar accumulator 0)
(defvar selection 0)
(defvar *font* nil)
(defvar *font-color* '(255 255 255 0))
(defvar item-array nil)
(defvar enemy-array nil)
(defvar max-characters '(40 40))

(defmacro restartable (&body body)
  "Stolen from 3bb.cc"
  `(restart-case
    (progn ,@body)
    (continue () :report "Countinue" )))

(defun init-engine ()
  (load "engine.cl")
;;;;  (load "engine.lisp")
  )

(defun main ()
  (sdl2:with-init (:everything) ;audio, etc.
    (sdl2:with-window (window :title "Title"
			      :w *screen-width*
			      :h *screen-height*
			      :flags '(:shown))
      (sdl2:with-renderer (default-renderer window)
;;;;	(sdl2-image:init '(:png))
;;;;	(sdl2-ttf:init)
	(sdl2-mixer:init :ogg)
	(sdl2-mixer:open-audio 44100 :s16sys 2 1024)
	(setf renderer (sdl2:get-renderer window))
;;;;	(setf *font* (sdl2-ttf:open-font "Test.ttf" 16))
	(init-engine) ;start sdl-mixer somewhere
	(start-main-menu-music (track-path level-track))
	(sdl2:with-event-loop (:method :poll)
	  (:keydown (:keysym keysym)
		    (keydown-check (sdl2:scancode keysym))
		    )
	  (:keyup (:keysym keysym)
		  (keyup-check (sdl2:scancode keysym))
		  )
	  (:idle ()
		 #+(and sbcl (not sb-thread)) (restartable (sb-sys:serve-all-events 0))
		 (restartable (la-loop))
		 )
	  (:quit ()
		 ;;;;(close-font)
		 (quit-audio)
		 (kill-textures)
		 t)
	  )))))

(defun la-loop ()
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)
  (game-loop)
  (sdl2:render-present renderer)
  (sdl2:delay 30)
  )

(defun game-loop ()
  (case state ;state management
    (title (title-loop))
    (level (level-loop))
    (paused (paused-loop))
    (inventory (inventory-loop))
    (credits (credits-loop))
    (game-over (game-over-loop))
    )
  )

(defun kill-textures ()
  (if player-buffer
      (sdl2:destroy-texture player-buffer))
  (if room-buffer
      (sdl2:destroy-texture room-buffer))
  (if enemy-buffer
      (sdl2:destroy-texture enemy-buffer))
  (if item-buffer
      (sdl2:destroy-texture item-buffer))
  (if status-buffer
      (sdl2:destroy-texture status-buffer))
  (if cursor-buffer
      (sdl2:destroy-texture cursor-buffer))
  (if information-buffer
      (sdl2:destroy-texture information-buffer))
  (if message-buffer
      (sdl2:destroy-texture message-buffer))
  (if +font-sheet+
      (progn (sdl2:free-surface +font-sheet+)
	     (setf +font-sheet+ nil)))
  )
