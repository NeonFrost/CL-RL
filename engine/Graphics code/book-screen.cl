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
  (setf book-buffer (progn  (book-screen)
			    (render-string (book-name book) 8 8 :*font-color* +cobalt+)
			    (render-string (book-contents book) 8 24)
			    (render-string (book-weight book) 8 (+ 24 (round (/ *screen-height* 2))) :*font-color* +pastel-grey+)
			    ))
  )
