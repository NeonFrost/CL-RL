(defstruct (book (:include item
			   (name "Book")
			   (type 'Book)
			   (information "")
			   (symbol "|")
			   (weight 1)))

  read
  (xp 20)
  )

(defun read-book (book) ;;;;from the player's inventory
  (if (not (book-read book))
      (progn (setf (book-read book) t)
	     (incf (entity-xp player) (* (entity-level player) (book-xp book))))
      )
  )

