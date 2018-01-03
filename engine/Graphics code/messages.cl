(defvar messages "")

(defun push-message (message)
  (setf messages (start-string messages message))
  )

(defun reset-messages ()
  (setf messages "")
  )

(defun create-message ()
  "Create a message that is to be put at the bottom of the screen"  ;;;;"Renders a message at the bottom of the screen"
  (if (not message-buffer)
      (setf message-buffer (create-text-buffer messages 0 0 :width (menu-width message-menu) :height (menu-height message-menu) :to-texture t :string-case 'text))
      (progn (reset-text-buffer message-buffer)
	     (setf message-buffer (create-text-buffer messages 0 0 :width (menu-width message-menu) :height (menu-height message-menu) :to-texture t :string-case 'text)))
      )
  )
