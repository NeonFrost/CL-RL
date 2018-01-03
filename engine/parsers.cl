(defmacro delimiter (limiter str &key (modifier 0))
  `(if (eq (type-of ,limiter) 'standard-char)
       (subseq ,str (+ (1+ (position ,limiter ,str)) ,modifier) (length ,str))
       (let ((dl (aref ,limiter 0)))
	 (subseq ,str (+ (1+ (position dl ,str)) ,modifier) (length ,str))
	 )))

(defmacro delimit-to (lower-limit upper-limit str &key (modifier 0))
  `(if (and (eq (type-of ,lower-limit) 'standard-char)
	    (eq (type-of ,upper-limit) 'standard-char))
       (subseq ,str (+ (1+ (position ,lower-limit ,str)) ,modifier) (position ,upper-limit ,str))
       (let ((dl (if (eq (type-of ,lower-limit) 'standard-char)
		     ,lower-limit
		     (aref ,lower-limit 0)))
	     (ul (if (eq (type-of ,upper-limit) 'standard-char)
		     ,upper-limit
		     (aref ,upper-limit 0))))
	 (subseq ,str (+ (1+ (position dl ,str)) ,modifier) (position ul ,str))
	 )))

(defmacro define-parser (type read-modes)
  `(defun ,type (file)
     (let ((vals nil))
       (with-open-file (stream file :direction :input)
	 (loop for n below (length ,read-modes)
	    do (setf vals (append vals (list (funcall (nth n ,read-modes) stream))))
	      ))
       vals)))

(define-parser book '(read-line
		      read
		      read))

(defmacro define-book (book-name file)
  `(let* ((vals (book ,file))
	  (name (car vals))
	  (contents (cadr vals))
	  (xp (caddr vals)))
     (defvar ,book-name (make-book :name name
				   :contents contents
				   :xp xp))))

;;;;usage: (define-book Gfens-death "G'fens death.book")

(define-parser creature '(read-line
			  read
			  read-line))
(defun collect-spells (spells tmp-list)
  (if tmp-list
      (if (find #\; spells)
	  (if (< (position #\space spells) (position #\; spells))
	      (setf tmp-list (append tmp-list (list (delimit-to " " ";" spells))))
	      (setf tmp-list (append tmp-list (list (delimit-to (aref spells 0) ";" spells :modifier -1))))
	      )
	  (setf tmp-list (append tmp-list (list (delimiter " " spells))))
	  )
      (if (find #\; spells)
	  (if (< (position #\space spells) (position #\; spells))
	      (setf tmp-list (list (delimit-to " " ";" spells)))
	      (setf tmp-list (list (delimit-to (aref spells 0) ";" spells :modifier -1)))
	      )
	  (setf tmp-list (list (delimiter " " spells)))
	  ))
  (if (find #\; spells)
      (collect-spells (delimiter ";" spells) tmp-list)
      tmp-list)
  )
      
(defun collect-stats (stats tmp-list)
  (if tmp-list
      (if (find #\% (delimit-to " " ";" stats))
	  (setf tmp-list (append tmp-list (list (parse-integer (delimit-to " " "%" stats)))))
	  (if (find #\" (delimit-to " " ";" stats))
	      (setf tmp-list (append tmp-list (list (delimit-to " " ";" stats))))
	      (if (find-if #'alpha-char-p (delimit-to " " ";" stats))
		  (setf tmp-list (append tmp-list (list (delimit-to " " ";" stats))))
		  (setf tmp-list (append tmp-list (list (parse-integer (delimit-to " " ";" stats))))))))
      (if (find-if #'alpha-char-p (delimit-to " " ";" stats))
	  (setf tmp-list (list (delimit-to " " ";" stats)))
	  (setf tmp-list (list (parse-integer (delimit-to " " ";" stats)))))
      )
  (if (find #\; stats)
      (collect-stats (delimiter " " (delimiter ";" stats)) tmp-list)
      tmp-list)
  )

(defmacro define-creature (creature-name file)
  `(let* ((vals (creature ,file))
	  (name (car vals))
	  (description (cadr vals))
	  (stats (caddr vals)) ;;;;create a helper function like the lisp file parser does with structs
	  (tmp-list (collect-stats stats '()))
	  (attack (nth 0 tmp-list))
	  (defense (nth 1 tmp-list))
	  (range-attack (nth 2 tmp-list))
	  (magic-attack (nth 3 tmp-list))
	  (magic-defense (nth 4 tmp-list))
	  (agility (nth 5 tmp-list))
	  (dodge (nth 6 tmp-list))
	  (hp (nth 7 tmp-list))
	  (mp (nth 8 tmp-list))
	  (xp (nth 9 tmp-list))
	  (level (nth 10 tmp-list))
	  (element (nth 11 tmp-list))
	  (sym (nth 12 tmp-list))
	  (los (nth 13 tmp-list))
	  )
     (defstruct (,creature-name (:include creature
					  (name name)
					  (description description)
					  (attack attack)
					  (defense defense)
					  (range-attack range-attack)
					  (magic-attack magic-attack)
					  (magic-defense magic-defense)
					  (speed agility)
					  (dodge dodge)
					  (hp hp)
					  (max-hp hp)
					  (mp mp)
					  (max-mp mp)
					  (xp xp)
					  (level level)
					  (elemental element)
					  (symbol sym)
					  (line-of-sight los))))))

(define-parser character-class '(read-line
				 read-line
				 read-line
				 read-line
				 read-line))

(defmacro define-character-class (class-name file)
  `(let* ((vals (character-class ,file))
	  (character-class (nth 0 vals))
	  (spell-list (collect-spells (nth 1 vals) '()))
	  (attributes (collect-stats (nth 2 vals) '()))
	  (resistant (nth 3 vals))
	  (weak (nth 4 vals))
	  )
     (defstruct (,class-name (:include entity (symbol "@")
				       (elemental character-class)))
       (spells spell-list)
       (attributes attributes)
       (resistance resistant)
       (weakness weak))
     ))
#|
".class" files like so

NAME
ABILITIES
ATTRIBUTES (strength, agility, etc.)
RESISTANT
WEAKNESSES
example:
Welder
Weld; Torch; Arson
A: +2; D: +5; M: -50%; S: -1
Fire
Water
|#	  
