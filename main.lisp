
(in-package #:com.thejach.aoc22)

(defclass food ()
  ((calories :accessor .calories :initarg :calories :initform 0)))

(defclass elf ()
  ((id :accessor .id :initarg :id)
   (foods :accessor .foods :initarg :foods :initform (list)
          :documentation "List of food objects carried by the elf")))

(defmethod print-object ((self elf) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "id ~a calories ~a" (.id self) (count-calories self))))

(defmethod count-calories ((self elf))
  (reduce #'+ (.foods self) :key #'.calories))

(defun day1 ()
  (let* ((elves-data (cl-ppcre:split "\\n\\n" *day1-input*))
         (elves (loop for elf-data in elves-data
                      for id from 1
                      collect
                      (make-instance 'elf
                                     :id id
                                     :foods (mapcar (lambda (calories)
                                                      (make-instance 'food :calories (parse-integer calories)))
                                                    (cl-ppcre:split "\\n" elf-data))))))
    ;; part 1
    (format t "Part 1: ~a~%"
            (loop for elf in elves
                  maximizing (count-calories elf)))

    ;; part 2
    (setf elves (sort elves #'> :key #'count-calories))
    (format t "Part 2: ~a~%"
            (reduce #'+ (subseq elves 0 3) :key #'count-calories))))

;;;; day 2

; a b c => rock paper scissors
; x y z => rock paper scissors
; 1 point for self being rock, 2 for paper, 3 scissors
; 0 point for loss, 3 point for draw, 6 for win
(defun score-round (opponent self)
  "Part 1 version"
  (cond
    ((and (eql opponent 'a) (eql self 'x)) (+ 1 3))
    ((and (eql opponent 'a) (eql self 'y)) (+ 2 6))
    ((and (eql opponent 'a) (eql self 'z)) (+ 3 0))
    ((and (eql opponent 'b) (eql self 'x)) (+ 1 0))
    ((and (eql opponent 'b) (eql self 'y)) (+ 2 3))
    ((and (eql opponent 'b) (eql self 'z)) (+ 3 6))
    ((and (eql opponent 'c) (eql self 'x)) (+ 1 6))
    ((and (eql opponent 'c) (eql self 'y)) (+ 2 0))
    ((and (eql opponent 'c) (eql self 'z)) (+ 3 3))
    ))

(defun day2 ()
  (score-round 'a 'y)
  (score-round 'b 'x)
  (score-round 'c 'z)

  (loop for (opponent self) on *day2-input* by #'cddr
        summing (score-round opponent self)))

; now x says need to lose, y says need to draw, z says need to win
(defun score-round2 (opponent self)
  "Part 2 version"
  (cond
    ((and (eql opponent 'a) (eql self 'x)) (+ 3 0)) ; choose paper to lose
    ((and (eql opponent 'a) (eql self 'y)) (+ 1 3))
    ((and (eql opponent 'a) (eql self 'z)) (+ 2 6))
    ((and (eql opponent 'b) (eql self 'x)) (+ 1 0))
    ((and (eql opponent 'b) (eql self 'y)) (+ 2 3))
    ((and (eql opponent 'b) (eql self 'z)) (+ 3 6))
    ((and (eql opponent 'c) (eql self 'x)) (+ 2 0))
    ((and (eql opponent 'c) (eql self 'y)) (+ 3 3))
    ((and (eql opponent 'c) (eql self 'z)) (+ 1 6))
    ))

