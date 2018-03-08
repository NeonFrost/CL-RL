(defvar +track-volume+ 0)
(defstruct track
  path
  stream
  loop-point)

(defmacro define-track (track path &key (loop-point 0))
  `(defvar ,track (make-track :path ,path :loop-point ,loop-point)))

#|
int music_internal_position(double position)
{
    if (music_playing->interface->Seek) {
        return music_playing->interface->Seek(music_playing->context, position);
    }
    return -1;
}
|#

(defun loop-track (track) ;;;;called when the song is 'finished'
  "An helper-function for sdl2-mixer's SetMusicPosition in common lisp. Requires ogg format. Uses the \":sdl2-ffi.functions\" package."
  (sdl2-ffi.functions:mix-set-music-position (coerce (track-loop-point track) 'double-float))
  )
  
(defvar main-menu-track (make-track :path "engine/audio/Title Screen.ogg")) ;;;;:path is relevant to where the program is started
(defvar level-track (make-track :path "engine/audio/Temple.ogg"))

(defun start-level-music (music) ;music is a path
  (setf (track-path level-track) music)
  (let ((music (sdl2-mixer:load-music music)))
    (setf (track-stream level-track) music)
    (sdl2-mixer:play-music (track-stream level-track) -1)
    )
  (setf volume-state 'increasing)
  (sdl2-mixer:volume-music +track-volume+)
  )


(defun start-main-menu-music (music)
  (let ((music (sdl2-mixer:load-music music)))
    (setf (track-stream main-menu-track) music)
    (sdl2-mixer:play-music (track-stream main-menu-track) -1)
    )
  )

(defun stop-music ()
  (sdl2-mixer:halt-music)
  )

(defun resume-level-music ()
  (volume-music 0)
  (setf +track-volume+ 0)
  (sdl2-mixer:play-music (track-stream level-track) -1)
  (setf volume-state 'increasing)
  )

(defun quit-audio ()
  (sdl2-mixer:halt-music)
  (if (track-stream main-menu-track)
      (progn (sdl2-mixer:free-music (track-stream main-menu-track))
	     (setf main-menu-track nil)))
  (if (track-stream level-track)
      (progn (sdl2-mixer:free-music (track-stream level-track))
	     (setf level-track nil)))
  (sdl2-mixer:close-audio)
  (sdl2-mixer:quit)
  )

(defun change-level-track (music)
  "Stops 'level-track' and changes it to another track. Used upon exit/entrance of a level/area."
  (sdl2-mixer:halt-music)
  (sdl2-mixer:free-music (track-stream level-track))
  (start-level-music music)
  )

(defun test-music ()
  (case volume-state
    (increasing (if (< +track-volume+ 125)
		    (progn (incf +track-volume+ 5)
			   (sdl2-mixer:volume-music +track-volume+))
		    (setf volume-state nil)))
      
    (decreasing (if (> +track-volume+ 0)
		    (progn (decf +track-volume+ 5)
			   (sdl2-mixer:volume-music +track-volume+))
		    (setf volume-state nil)))
    (track-changing (if (> +track-volume+ 0)
			(progn (decf +track-volume+ 5)
			       (sdl2-mixer:volume-music +track-volume+))
			(progn (setf volume-state nil)
			       (change-level-track (level-music (player-level user))))))
    )
  )
