(defvar book-buffer nil)
(defvar book-menus nil)

(define-menu book-information-menu book-menus
  0 0
  *screen-width* (round (/ *screen-height* 2))
  +pastel-grey+ +black+)
(define-menu book-stats-menu book-menus
  0 (round (/ *screen-height* 2))
  *screen-width* (round (/ *screen-height* 2))
  +pastel-grey+ +black+)

(define-screen book-screen book-menus)

;;;;goes to item-information substate

(defun render-book (book)
  (book-screen)
  (if (not book-buffer)
      (setf book-buffer (create-text-buffer (start-string (book-name book) (book-contents book) (write-to-string (book-weight book)))
					    0 0
					    :width (- *screen-width* 16)
					    :height (- *screen-height* 16)
					    :to-texture t
					    :string-case 'text))
      (progn (tex-blit book-buffer
		       :src (sdl2:make-rect 0 0 (texture-width book-buffer) (cadr character-size))
		       :dest (sdl2:make-rect (menu-x book-information-menu)
					     (menu-y book-information-menu)
					     (menu-width book-information-menu)
					     (cadr character-size))
		       :color +cobalt+)
	     (tex-blit book-buffer
		       :src (sdl2:make-rect 0 (cadr character-size) (texture-width book-buffer) (- (texture-height book-buffer) (cadr character-size)))
		       :dest (sdl2:make-rect (menu-x book-information-menu)
					     (menu-y book-information-menu)
					     (menu-width book-information-menu)
					     (- (menu-height book-information-menu) (cadr character-size)))
		       :color +pastel-grey+))
      ))
  
