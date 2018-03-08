#|
Displays: Item-name, Item-description, item-actions (wear, drop, fire, quaff)
|#

(defvar item-menus nil)

(define-menu information-menu item-menus
  0 0
  *screen-width* *screen-height*
  +pastel-grey+ +black+)

(define-screen item-screen item-menus)

#|(defun display-item-information (item)
  (let ((name (item-name item))
	(description (item-description item))
	(actions (item-actions item))
	(x (menu-x item-information))
	(y (menu-y item-information))
	)
    (render-string name (+ x 8) (+ y 8) )))|#
