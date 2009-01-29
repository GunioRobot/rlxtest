(eval-when (:execute :load-toplevel :compile-toplevel) 
  (require :rlx))

(defpackage :rlxtest (:use rlx :common-lisp))
(in-package :rlxtest)


;;; Utils

(defmacro awhen (condition &body body)
  "Anaphoric when. Acts like a while, but defines the variable
   'it' in it's lexical scope, containg the value of condition."
  `(let ((it ,condition))
     (when it
       ,@body)))
    
;;; A stupid test world running Conway's Game of Life

(define-prototype test-world (:parent rlx:=world=)
  (width :initform 100)
  (height :initform 100))

;;; utility, delete the cell and drop the other one on the same location

(define-method replace-with cell (new-cell)
  (clon:with-field-values (row column) self
      [delete-from-world self]
      [drop-cell *active-world* new-cell row column]))

;;; default response to is-star/is-space
(define-method is-star cell ()
  nil)

;;; a 'dead' cell

(define-prototype space (:parent rlx:=cell=)
  (categories :initform '(:actor))
  (tile :initform "starfield"))

(define-method is-star space ()
  nil)
(define-method is-space space ()
  t)

	   
;; an 'alive' cell

(define-prototype star (:parent rlx:=cell=)
  (categories :initform '(:actor))
  (tile :initform "star"))

(define-method is-star star ()
  t)
(define-method is-space star ()
  nil)

;; game of life rules, inefficient (constantly delete/drop cells), but meh

(define-method run space ()
  (let ((neighbour-count 
	 (loop for direction in (list :north :east :west :south)
	    count (awhen [find self :direction direction]
			 [is-star it]))))
    (when (= neighbour-count 2) 
      [replace-with self (clone =star=)])))

(define-method run star ()
  (let ((neighbour-count 
	 (loop for direction in (list :north :east :west :south)
	    count (awhen [find self :direction direction]
			 [is-star it]))))
    (cond ((< neighbour-count 2) [replace-with self (clone =space=)])
	  ((> neighbour-count 3) [replace-with self (clone =space=)]))))

;; drop some random stars in a world full of space

(define-method generate test-world (&optional parameters)
  (declare (ignore parameters))
  (clon:with-field-values (height width) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =space=) i j]))
    (dotimes (i 4000)
      [drop-cell self (clone =star=) (random height) (random width)])
    ))

(define-prototype testgame-prompt (:parent rlx:=prompt=)
  (default-keybindings :initform 
      '(("W" nil "move :north .")
	("Q" nil "quit .")
	("SPACE" nil "skip-turn .")
	)))

(define-prototype dummy-player (:parent rlx:=cell=)
  (tile :initform "player")
  (categories :initform '(:actor :player))
  (movement-cost :initform (make-stat :base 7)))

(define-method skip-turn dummy-player ()
  (format t "Skipping a turn")
  [expend-action-points self <action-points>])

(define-method quit dummy-player ()
    (prog1 nil (rlx:quit :shutdown)))

(define-method forward dummy-player (key msg)
  (format t "player didn't understand message: ~A, ~A~%" key msg))

(defun testgame ()
  (setf rlx:*screen-height* 600)
  (setf rlx:*screen-width* 800)
  (let ((player-prompt (clone =testgame-prompt=))
	(world (clone =test-world=))
	(player (clone =dummy-player=))
	(viewport nil))
    ;;world
    (setf *active-world* world)
    [create-default-grid world]
    [generate world]

    ;;player
    [set-player world player]
    [drop-cell world player 10 10]

    ;;prompt
  
    [install-default-keybindings player-prompt]
    [resize player-prompt :height 30 :width 400]
    [move player-prompt :x 0 :y 0]
    [set-receiver player-prompt world]
    [hide player-prompt]
    ;;viewport
    (setf viewport (clone =viewport=))
    [set-world viewport world]
    [resize viewport :height 320 :width 400]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 10 :y 90 :height 20 :width 25]
    [adjust viewport]

    ;; go !
    [start world]


    (install-widgets (list viewport player-prompt))))

(testgame)