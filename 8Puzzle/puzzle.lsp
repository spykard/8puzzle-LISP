(defvar *state*) ; List
(defvar *tempState*) ; Array			 

(defvar *endstate* '((1 2 3)
					 (8 0 4)
					 (7 6 5)))
					 
(defvar *roundcount*)					 
					 

;; Start Game
(defun start-the-game ()
	(display-message-1)
	(insert-data)
	(loop
		(input-move)
		(if (equal *state* *endstate*) (return)))
	(display-message-3)		
	(return-from start-the-game "GameEnded")
)
	
(defun display-message-1 ()
	(format t "~%Welcome to the Puzzle Game!~%You will be Required to insert the Starting Puzzle and you can move a tile Up/Down/Right/Left in order to reach:")
	(pretty-print *endstate*)	; Pretty Print Final State
	(format t "~%#########")
)	

(defun display-message-2 ()
	(format t "~%--- Problem: Invalid Move!")
)

(defun display-message-3 ()
	(format t "~%Game Finished Successfully!")
)		
	
(defun insert-data ()
	(let ((type nil))
	
		(loop	; Loop until Valid Input
			(format t "~%~%Starting Puzzle Input. From keyboard (k) or from file (f)? : ")	
			(setf type (read))
			(if (or (eql type 'k) (eql type 'f)) (return)))
		
		(if (eql type 'k) (keyboard-input))		; KEYBOARD
		(if (eql type 'f) (file-input))			; or FILE
	)
)		
	
;; Keyboard Input	
(defun keyboard-input ()
	(let ((row1 nil)
		  (row2 nil)
		  (row3 nil))

		(loop	; Loop until Valid Input
			(format t "~%Input 1st Row in Parentheses, 0 for Empty - (Tile1 Tile2 Tile3) : ")
			(setf row1 (read))
			(format t "~%Input 2nd Row in Parentheses, 0 for Empty - (Tile4 Tile5 Tile6) : ")
			(setf row2 (read))	
			(format t "~%Input 3rd Row in Parentheses, 0 for Empty - (Tile7 Tile8 Tile9) : ")
			(setf row3 (read))	
			
			(if (eql (validate-input row1 row2 row3) 1) (return)))
		
		(initialize row1 row2 row3)
	)
)
	
;; File Input	
(defun file-input ()
	(let ((row1 nil)
		  (row2 nil)
		  (row3 nil)	
		  (filename "C:/puzzleT.txt"))
		  
		(with-open-file (infile filename :direction :input)
			; read-from-string converts a string to list
			(setf row1 (read-from-string (read-line infile nil 'eof)))	; eof-value -> 'eof
			(setf row2 (read-from-string (read-line infile nil 'eof)))
			(setf row3 (read-from-string (read-line infile nil 'eof))))

		(if (eql (validate-input row1 row2 row3) 1) (initialize row1 row2 row3)
			; else, Input from Keyboard
			(keyboard-input))
   )
)

;; Make sure the Input is correct, returns 1 if it is	
(defun validate-input (row1 row2 row3)
	(let ((uniquenumbers '("placeholder" 0 1 2 3 4 5 6 7 8)))
		  
		(if (and (listp row1) (listp row2) (listp row3))	; are Lists
			(if (and (eql (list-length row1) 3) (eql (list-length row2) 3) (eql (list-length row3) 3))	; have Size 3
				(when t 
					;; Make sure all elements are Unique, no Duplicates	
					(dolist (element1 row1) (delete element1 uniquenumbers)) 
					(dolist (element2 row2) (delete element2 uniquenumbers)) 
					(dolist (element3 row3) (delete element3 uniquenumbers))		
					(if (eql (list-length uniquenumbers) 1)	(return-from validate-input 1)
						(format t "~%--- Problem: You should use Non-Duplicate 0 to 8!"))
				)
				(format t "~%--- Problem: You inputed too few or too many!"))
			(format t "~%--- Problem: You forgot Parentheses!"))	
	)
)	

;; Initializing State, input is done
(defun initialize (row1 row2 row3)
	(setf *roundcount* 0) 	
	(setf *state* nil)  ; Wipe Previous Runs
	(push row3 *state*) 
	(push row2 *state*) 
	(push row1 *state*) 		
	(format t "~%~%STARTING STATE")
	(pretty-print *state*)		
	(file-output *state*)
)

;; Output to .txt every Round
(defun file-output (puzzleboard)
	(let ((filename "C:/puzzleT.txt"))
		(with-open-file (str filename
						:direction :output
						:if-exists :append
						:if-does-not-exist :create)
			;; Pretty Print board to .txt
			(format str "~%")
			(dolist (row puzzleboard)
				(format str "~a~%" row))
			(format str "Round ~a by Player~%" *roundcount*))
				
		(format t "~%Output Sent to ~a~%" filename)
	)
)		
			
;; Pretty Print board to Terminal		
(defun pretty-print (puzzleboard)
	(format t "~%.-------.")
	(dolist (row puzzleboard)	; ((1 2 3) ...)
		(format t "~%| ")
		(dolist (tile row)		; (1 2 3)
			(if (eql tile 0) (format t "  ")
				(format t "~a " tile)))
		(format t "|"))			
	(format t "~%.-------.")
)

(defun input-move ()
	(let ((number nil)
		  (direction nil))
	
		(loop	; Loop until Valid Input
			(format t "~%~%Round ~a. Choose Number to Move : " *roundcount*)	
			(setf number (read))
			(if (and (> number 0) (< number 9)) (return)))
			
		(loop	; Loop until Valid Input
			(format t "~%------ Choose Direction (u/d/r/l) : ")	
			(setf direction (read))
			(if (or (eql direction 'u) (eql direction 'd) (eql direction 'l) (eql direction 'r)) (return)))			
				
		(move-Tile (first (find-Tile number)) (car (rest (find-Tile number))) direction)
		(setf *state* (puzArray-to-list *tempState*))  ; Apply
		(incf *roundcount*)
		(pretty-print *state*)
		(file-output *state*)		
	)
)	

;; Find (i,j) of Selected
(defun find-Tile (number)
	(loop for i from 0 to 2 do
		(loop for j from 0 to 2 do
			(if (equal (aref (puzList-to-array *state*) i j) number) 
				(return-from find-Tile (list i j)))))
)

(defun move-Tile (i j direction)
	(setf *tempState* (puzList-to-array *state*))
	(cond ((eql direction 'u) (if (not (eql (checkUp i j) 1)) (display-message-2)))  ; UP
		((eql direction 'd) (if (not (eql (checkDown i j) 1)) (display-message-2)))  ; DOWN
		((eql direction 'r) (if (not (eql (checkRight i j) 1)) (display-message-2))) ; RIGHT
		((eql direction 'l) (if (not (eql (checkLeft i j) 1)) (display-message-2)))) ; LEFT	
)

(defun checkUp (i j)
    (if (> i 0)	
		;; Check for Gap
		(when (equal (aref *tempState* (- i 1) j) 0)  ; UP
			;; Swap		
			(setf (aref *tempState* (- i 1) j) (aref *tempState* i j))
			(setf (aref *tempState* i j) 0)
			(return-from checkUp 1)))  ; Success
)

(defun checkDown (i j)
    (if (< i 2)
		;; Check for Gap
		(when (equal (aref *tempState* (+ i 1) j) 0)  ; UP
			;; Swap
			(setf (aref *tempState* (+ i 1) j) (aref *tempState* i j))
			(setf (aref *tempState* i j) 0)
			(return-from checkDown 1)))  ; Success
)

(defun checkRight (i j)
    (if (< j 2)
		;; Check for Gap
		(when (equal (aref *tempState* i (+ j 1)) 0)  ; UP
			;; Swap
			(setf (aref *tempState* i (+ j 1)) (aref *tempState* i j))
			(setf (aref *tempState* i j) 0)
			(return-from checkRight 1)))  ; Success
)

(defun checkLeft (i j)
    (if (> j 0)
		;; Check for Gap
		(when (equal (aref *tempState* i (- j 1)) 0)  ; UP
			;; Swap
			(setf (aref *tempState* i (- j 1)) (aref *tempState* i j))
			(setf (aref *tempState* i j) 0)
			(return-from checkLeft 1)))  ; Success
)

;; List -> Array, used to Move
(defun puzList-to-array (lista)
	(make-array (list (length lista)
				(length (first lista))) :initial-contents lista)
)

;; Array -> List, used to Move				
(defun puzArray-to-list (arrayP)
	(loop for i below (array-dimension arrayP 0)
		collect (loop for j below (array-dimension arrayP 1)
				collect (aref arrayP i j)))
)