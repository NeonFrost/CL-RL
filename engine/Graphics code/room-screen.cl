#|
Map takes up 6/8 of the width of the screen
             7/8 of the height of the screen
Player Stats is on the right topmost
Visible enemies is under PS
The Level the player is in and the room the player is in is displayed under the enemies list

On the left most part of the screen (the first 1/8)
Possible Actions (a, attack, f, fire projectile, z, cast a spell, ? for more actions)
Weapon
Armor

What is currently under the player
(creature)
(item)
(tile-type, i.e. water, soil, grass, temple floor, etc.)

-------------------------------------------------------

Last 1/8 of the height of the screen
1/8, unsure
6/8, messages
1/8, Level - Floor
|#
(defvar room-menus nil)
(define-menu player-menu room-menus
  0 0
  (round (/ *screen-width* 8)) (round (* (/ *screen-height* 8) 7))
  +pastel-grey+ +black+)
(define-menu map-menu room-menus
  (round (/ *screen-width* 8)) 0
  (round (* (/ *screen-width* 8) 6)) (round (* (/ *screen-height* 8) 7))
  +pastel-grey+ +black+)
(define-menu status-menu room-menus
  (round (* (/ *screen-width* 8) 7)) 0
  (round (/ *screen-width* 8)) (round (* (/ *screen-height* 8) 7))
  +pastel-grey+ +black+)
(define-menu cursor-menu room-menus ;;;;what is currently at cursor-x/y/z (default player-x/y/z)
  0 (round (* (/ *screen-height* 8) 7))
  (round (/ *screen-width* 8)) (round (/ *screen-height* 8))
  +pastel-grey+ +black+)
(define-menu message-menu room-menus
  (round (/ *screen-width* 8)) (round (* (/ *screen-height* 8) 7))
  (round (* (/ *screen-width* 8) 6)) (round (/ *screen-height* 8))
  +pastel-grey+ +black+)
(define-menu level-floor-menu room-menus
  (round (* (/ *screen-width* 8) 7)) (round (* (/ *screen-height* 8) 7))
  (round (/ *screen-width* 8)) (round (/ *screen-height* 8))
  +pastel-grey+ +black+)
(define-screen room-screen room-menus)

