
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

;;;; day 3

(defvar *sample-sacks* '("vJrwpWtwJgWrhcsFMMfFFhFp"
                         "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                         "PmmdzqPrVvPwwTWBwg"
                         "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                         "ttgJtRGJQctTZtZT"
                         "CrZsJsPPZsGzwwsLwLmpwMDw"))

(defun common-items (sack)
  (let ((comp1 (coerce (subseq sack 0 (/ (length sack) 2)) 'list))
        (comp2 (coerce (subseq sack (/ (length sack) 2)) 'list)))
    (remove-duplicates (intersection comp1 comp2))))

(defun prioritize-letter (char)
  (cond
    ((<= (char-code #\a) (char-code char) (char-code #\z))
     (1+ (- (char-code char) (char-code #\a))))
    ((<= (char-code #\A) (char-code char) (char-code #\Z))
     (+ 27 (- (char-code char) (char-code #\A))))))


(defun day3 ()
  (reduce #'+ (mapcar (lambda (sack) (mapcar #'prioritize-letter (common-items sack))) *sample-sacks*) :key #'first)
  (format t "Part 1: ~a~%"
          (reduce #'+ (mapcar (lambda (sack) (mapcar #'prioritize-letter (common-items sack))) *day3-input*) :key #'first))

  (format t "Part 2: ~a~%"
          (loop for (elf1 elf2 elf3) on *day3-input* by #'cdddr
                summing (reduce #'+ (mapcar #'prioritize-letter
                                            (remove-duplicates (intersection (coerce elf3 'list)
                                                                             (intersection (coerce elf1 'list) (coerce elf2 'list)))))))))


;;;; day 4

(defvar *day4-sample* "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")


(defun extract-pair-ranges (input)
  (let ((pairs (cl-ppcre:split "\\n" input)))
    (loop for pair in pairs
          collect
          (let* ((pair-halves (cl-ppcre:split "," pair))
                 (ranged-pair-halves (mapcar (lambda (range)
                                               (mapcar #'parse-integer (cl-ppcre:split "-" range)))
                                             pair-halves)))
            ranged-pair-halves))))

(defun fully-overlapping-pairs (ranged-pair-halves &aux (total 0))
  (loop for pair in ranged-pair-halves
        do
        (let ((p1-lower (first (first pair)))
              (p1-upper (second (first pair)))
              (p2-lower (first (second pair)))
              (p2-upper (second (second pair))))
          (when (or (<= p1-lower p2-lower p2-upper p1-upper)
                    (<= p2-lower p1-lower p1-upper p2-upper))
            (incf total))))
  total)

(defun overlapping-pairs (ranged-pair-halves &aux (total 0))
  (loop for pair in ranged-pair-halves
        do
        (let ((p1-lower (first (first pair)))
              (p1-upper (second (first pair)))
              (p2-lower (first (second pair)))
              (p2-upper (second (second pair))))
          (when (or (<= p1-lower p2-lower p1-upper)
                    (<= p1-lower p2-upper p1-upper)
                    (<= p2-lower p1-lower p2-upper)
                    (<= p2-lower p1-upper p2-upper))
            (incf total))))
  total)

(defun day4 ()
  (format t "Part 1: ~a~%"
          (fully-overlapping-pairs (extract-pair-ranges *day4-input*)))
  (format t "Part 1: ~a~%"
          (overlapping-pairs (extract-pair-ranges *day4-input*))))


;;;; day 5

(defvar *day5-sample* "
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defun initialize-stacks (stacks stack-contents)
  (loop for stack-row in (reverse stack-contents)
        do
        (dotimes (i (length stack-row))
          (unless (equal (elt stack-row i) "")
            (push (elt stack-row i) (aref stacks i))))))

(defun day5 ()
  (let+ (((stacks-data moves-data) (cl-ppcre:split "\\n\\n" *day5-input*))
         (stack-lines (cl-ppcre:split "\\n" stacks-data))
         (stack-count (parse-integer (car (last (cl-ppcre:split "( )+" (car (last stack-lines)))))))
         (stacks (make-array stack-count :initial-element (list)))
         (content-matcher "((\\[[A-Z]\\]|   ) ?)")
         (stack-contents (remove-if (lambda (line)
                                      (or (null line)
                                          (every (lambda (s) (equal s "")) line)))
                                    (loop for line in stack-lines
                                          collect
                                          (mapcar (lambda (s)
                                                    (string-trim '(#\space) (cl-ppcre:regex-replace-all "(\\[|\\])" s "")))
                                                  (cl-ppcre:all-matches-as-strings content-matcher line))))))
    (initialize-stacks stacks stack-contents)

    (let ((move-matcher "move ([0-9]+) from ([0-9]+) to ([0-9]+)"))
      (loop for move in (cl-ppcre:split "\\n" moves-data)
            do
            (cl-ppcre:register-groups-bind (amount src dest) (move-matcher move)
              (dotimes (i (parse-integer (string amount)))
                (push (pop (aref stacks (1- (parse-integer (string src)))))
                      (aref stacks (1- (parse-integer (string dest)))))))))

    (format t "Part 1: ~a~%"
            (apply #'uiop:strcat (loop for stack across stacks
                                       collect (first stack))))

    (initialize-stacks stacks stack-contents)

    (let ((move-matcher "move ([0-9]+) from ([0-9]+) to ([0-9]+)"))
      (loop for move in (cl-ppcre:split "\\n" moves-data)
            for crates = (list)
            do
            (cl-ppcre:register-groups-bind (amount src dest) (move-matcher move)
              (dotimes (i (parse-integer (string amount)))
                (push (pop (aref stacks (1- (parse-integer (string src)))))
                      crates))
              (loop for c in crates do (push c (aref stacks (1- (parse-integer (string dest)))))))))

    (format t "Part 2: ~a~%"
            (apply #'uiop:strcat (loop for stack across stacks
                                       collect (first stack))))))

;;;; day 6

(defun find-marker (sample &key (distinct-chars 4))
  (loop for i below (length sample)
        do
        (when (= distinct-chars (length (remove-duplicates (subseq sample i (+ i distinct-chars)))))
          (return (+ i distinct-chars)))))

(defun day6 ()
  (format t "Part 1: ~a~%"
          (find-marker *day6-input* :distinct-chars 4))
  (format t "Part 2: ~a~%"
          (find-marker *day6-input* :distinct-chars 14)))


;;;; day 7

(defvar *day7-sample* "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defclass folder ()
  ((name :accessor .name :initarg :name)
   (parent :accessor .parent :initarg :parent)
   (contents :accessor .contents :initform (list) :documentation "List of folders or files")))

(defmethod get-subdir ((self folder) dir-name)
  (find dir-name (.contents self) :key #'.name :test #'equal))

(defclass file ()
  ((name :accessor .name :initarg :name)
   (size :accessor .size :initarg :size)))

(defun construct-filesystem (input)
  (let* ((filesystem (make-instance 'folder :name "/"))
         (commands (mapcar (lambda (cmd) (cl-ppcre:split "\\n" cmd))
                           (cdr (cl-ppcre:split "\\$ " input))))
         (pwd nil))
    (loop for command in commands do
          (cond
            ((str:starts-with? "cd" (first command))
             (let ((dir-name (second (cl-ppcre:split " " (first command)))))
               (cond
                 ((equal "/" dir-name)
                  (setf pwd filesystem))
                 ((equal ".." dir-name)
                  (setf pwd (.parent pwd)))
                 (:otherwise
                   (setf pwd (get-subdir pwd dir-name))))))
            ((equal (first command) "ls")
             (loop for content in (cdr command) do
                   (let+ (((dir-or-size name) (cl-ppcre:split " " content)))
                     (push (if (equal "dir" dir-or-size)
                               (make-instance 'folder :name name :parent pwd)
                               (make-instance 'file :name name :size (parse-integer dir-or-size)))
                           (.contents pwd)))))))
    filesystem))

(defmethod total-size ((folder folder))
  (reduce #'+ (mapcar #'total-size (.contents folder))))

(defmethod total-size ((file file))
  (.size file))

(defun all-subdirs (filesystem)
  (alexandria:flatten
    (loop for content in (.contents filesystem)
        if (eql (type-of content) 'folder)
        collect (cons content (all-subdirs content)))))

(defun total-size-of-max (filesystem max)
  (reduce #'+ (remove-if (lambda (size) (> size max))
                         (mapcar #'total-size (all-subdirs filesystem)))))

(defun smallest-when-deleted-gives-needed-space (filesystem max-available needed-space)
  (let* ((total (total-size filesystem))
         (unused (- max-available total))
         (free-up-min (- needed-space unused)))
    (find-if (lambda (size) (>= size free-up-min))
             (sort (mapcar #'total-size (all-subdirs filesystem)) #'<))))

(defun day7 ()
  (format t "Part 1: ~a~%"
          (total-size-of-max (construct-filesystem *day7-input*) 100000))

  (format t "Part 2: ~a~%"
          (smallest-when-deleted-gives-needed-space (construct-filesystem *day7-input*) 70000000 30000000)))

