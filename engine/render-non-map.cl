(defvar start-screen-buffer nil)
(push start-screen-buffer buffers)

#|
(defun render-start-screen ()
  (start-screen)
  (if start-screen-buffer
      (tex-blit start-screen-buffer
		:src (sdl2:make-rect 0 0
				     (sdl2:texture-width start-screen-buffer)
				     (sdl2:texture-height start-screen-buffer))
		:dest (sdl2:make-rect (menu-x start-screen-menu)
				      (menu-y start-screen-menu)
				      (menu-width start-screen-menu)
				      (menu-height start-screen-menu)))
      (progn (setf start-screen-buffer (create-text-buffer (start-string "The Galactic Council has entrusted you with obtaining the tome of Karakas, a book about the first Vir incursion."
									 "The information in this book will help in the fight against the Vir, hopefully pushing them back permanently."
									 "The planet you are going to, \"S'daia,\" was ravaged by a Vir weapon, causing the planet to displace itself on occasion."
									 "It is said that you never truly die on the planet, simply 'resetting' you to when you first stepped on the planet, but"
									 "retaining few, if any, memories past that point. Our information about this phenomenon is limited and it is believed that"
									 "the Vir have created technology that allows for this phenomenon on a more localized scale."
									 "Proceed with Caution,there have been reports of the worst kinds of Vir treading the temple grounds, with the"
									 "local fauna of the Ice Caverns proving to be...deadly."
									 "Good luck, you'll need it.")
							   0 0 :width (* (car character-size) 120) :height (* (cadr character-size) 60) :to-texture t :string-case 'text))
	     (render-start-screen))))
  
|#
(defun render-start-screen ()
  (start-screen)
  (if start-screen-buffer
      (tex-blit start-screen-buffer
		:src (sdl2:make-rect 0 0
				     (sdl2:texture-width start-screen-buffer)
				     (sdl2:texture-height start-screen-buffer))
		:dest (sdl2:make-rect (menu-x start-screen-menu)
				      (menu-y start-screen-menu)
				      (menu-width start-screen-menu)
				      (menu-height start-screen-menu)))
      (progn (setf start-screen-buffer (create-text-buffer "The Galactic Council has entrusted you with obtaining the tome of Karakas, a book about the first Vir incursion. The information in this book will help in the fight against the Vir, hopefully pushing them back permanently. The planet you are going to, \"S'daia,\" was ravaged by a Vir weapon, causing the planet to displace itself on occasion. It is said that you never truly die on the planet, simply 'resetting' you to when you first stepped on the planet, but retaining few, if any, memories past that point. Our information about this phenomenon is limited and it is believed that the Vir have created technology that allows for this phenomenon on a more localized scale. Proceed with Caution,there have been reports of the worst kinds of Vir treading the temple grounds, with the local fauna of the Ice Caverns proving to be...deadly. Good luck, you'll need it." 0 0 :width (* (car character-size) 105) :height (* (cadr character-size) 60) :to-texture t :string-case 'text))
	     (render-start-screen))))
