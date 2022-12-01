
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

(day1)
