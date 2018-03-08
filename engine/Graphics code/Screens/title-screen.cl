(defvar title-menus nil)
(define-menu title-menu title-menus 0 0 *screen-width* *screen-height* '(127 127 127 127) +black+)
(define-screen title-screen title-menus)

(defvar title-screen-buffer nil)
(defvar title-name-buffer nil)
(push title-screen-buffer buffers)
(push title-name-buffer buffers)

(defun render-title-screen ()
  (title-screen)
  (if (not title-name-buffer)
      (setf title-name-buffer (create-text-buffer "S'daia"
						  0 0
						  :width (* (car character-size) (length "S'daia"))
						  :height (cadr character-size)
						  :to-texture t
						  :string-case 'text))
      (tex-blit title-name-buffer
		:src (sdl2:make-rect 0 0
				     (sdl2:texture-width title-screen-buffer)
				     (sdl2:texture-height title-screen-buffer))
		:dest (sdl2:make-rect (- (round (/ *screen-width* 2)) (* (car character-size) (length "S'daia")))
				      0
				      (sdl2:texture-width title-screen-buffer)				      
				      (* (cadr character-size) 2))))
  (if (not title-screen-buffer)
      (setf title-screen-buffer (create-text-buffer (start-string "Start New Game"
								  "  Load Game   "
								  "   Options    "
								  "  Exit Game   ")
						    0 0 :width (* (car character-size) (length "Start New Game")) :height (* (cadr character-size) 4) :to-texture t :string-case 'text))
      (tex-blit title-screen-buffer
		:src (sdl2:make-rect 0 0
				     (sdl2:texture-width title-screen-buffer)
				     (sdl2:texture-height title-screen-buffer))
		:dest (sdl2:make-rect (- (round (/ *screen-width* 2)) (* 7 (car character-size)))
				      (- (round (/ *screen-height* 2)) (* 2 (cadr character-size)))
				      (* (length "Start New Game") (car character-size))
				      (* (cadr character-size) 4)))
      ))
