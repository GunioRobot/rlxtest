(defpackage :rlxtest (:use rlx :common-lisp))
(in-package :rlxtest)

;;; A stupid test world running Conway's Game of Life

(define-prototype test-world (:parent rlx:=world=)
  (width :initform 100)
  (height :initform 100))

;;; utility, delete the cell and drop the other one on the same location

(define-method replace-with cell (new-cell)
  (clon:with-field-values (row column) self
      [delete-from-world self]
      [drop-cell *active-world* new-cell row column]))


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
	    count [is-star [find-cell self :direction direction]])))
    (when (= neighbour-count 2) 
      [replace-with self (clone =star=)])))

(define-method run star ()
  (let ((neighbour-count 
	 (loop for direction in (list :north :east :west :south)
	    count [is-star [find-cell self :direction direction]])))
    (cond ((< neighbour-count 2) [replace-with self (clone =space=)])
	  ((> neighbour-count 3) [replace-with self (clone =space=)]))))

;; drop some random stars in a world full of space

(define-method generate test-world (&optional parameters)
  (declare (ignore parameters))
  (clon:with-field-values (height width) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =space=) i j]))
    (dotimes (i 140)
      [drop-cell self (clone =star=) (random height) (random width)])
    ))

(define-prototype testgame-prompt (:parent rlx:=prompt=)
  (default-keybindings :initform '(("W" nil "move :north .")
				   ("Q" nil "quit .")
				   )))


(define-prototype dummy-player (:parent rlx:=cell=)
  (tile :initform "player")
  (categories :initform '(:actor :player))
  (movement-cost :initform (make-stat :base 7)))

(define-method quit dummy-player ()
    (prog1 nil (rlx:quit :shutdown)))

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
    [start world]

    ;;player
    [set-player world player]
    [drop-cell world player 10 10]

    ;;prompt
    [resize player-prompt :height 30 :width 400]
    [move player-prompt :x 0 :y 770]
    [set-receiver player-prompt player]
    [set-mode player-prompt :forward]
    
    ;;viewport
    (setf viewport (clone =viewport=))
    [set-world viewport world]
    [resize viewport :height 320 :width 400]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 10 :y 90 :height 20 :width 25]
    [adjust viewport]

    (install-widgets (list viewport player-prompt))))