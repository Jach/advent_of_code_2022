
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


;;;; day 8

(defvar *day8-sample* "30373
25512
65332
33549
35390")

(defun construct-treegrid (input)
  (let* ((rows (cl-ppcre:split "\\n" input))
         (col-count (length (first rows)))
         (grid (make-array (list (length rows) col-count))))
    (loop for i below (length rows)
          for row in rows do
          (loop for j below col-count do
                (setf (aref grid i j) (parse-integer (string (elt row j))))))
    grid))

(defun visible? (grid i j)
  "True if point at row i and col j is visible to the outside"
  (let ((height (aref grid i j)))
    (some #'identity
          (loop for neighbors in
                (list
                  ; everything to the left
                  (loop for x from 0 below j collect (aref grid i x))
                  ; everything to the right
                  (loop for x from (1+ j) below (array-dimension grid 1) collect (aref grid i x))
                  ; everything above
                  (loop for y from 0 below i collect (aref grid y j))
                  ; everything below
                  (loop for y from (1+ i) below (array-dimension grid 0) collect (aref grid y j)))
                collect (every (lambda (el) (< el height)) neighbors)))))

(defun scenic-score (grid i j)
  (let ((height (aref grid i j)))
    (* (viewing-distance height (reverse (loop for x from 0 below j collect (aref grid i x)))) ; left
       (viewing-distance height (loop for x from (1+ j) below (array-dimension grid 1) collect (aref grid i x))) ; right
       (viewing-distance height (reverse (loop for y from 0 below i collect (aref grid y j)))) ; above
       (viewing-distance height (loop for y from (1+ i) below (array-dimension grid 0) collect (aref grid y j)))))) ; below

(defun viewing-distance (height trees)
  (let ((sum 0))
    (loop for tree in trees do
          (incf sum)
          (when (>= tree height)
            (loop-finish)))
    sum))

(defun day8 ()
  (let ((grid (construct-treegrid *day8-input*)))
    (format t "Part 1: ~a~%"
            (loop for i below (array-dimension grid 0) summing
                  (loop for j below (array-dimension grid 1) counting
                        (visible? grid i j))))

    (format t "Part 2: ~a~%"
            (loop for i below (array-dimension grid 0) maximizing
                  (loop for j below (array-dimension grid 1) maximizing
                        (scenic-score grid i j))))))


;;;; day 9

(defvar *day9-sample* '(
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2))

(defun day9-part1 ()
  (let ((head (complex 0 0))
        (tail (complex 0 0))
        (tail-positions (list)))
    (loop for (direction amount) on *day9-input* by #'cddr
          do
          (dotimes (i amount)
            (case direction
              (r (incf head 1))
              (l (decf head 1))
              (u (incf head #C(0 1)))
              (d (decf head #C(0 1))))
            (when (not (adjacent? head tail))
              (push tail tail-positions)
              (incf tail (approach-head tail head)))))
    (1+ (length (remove-duplicates tail-positions)))))

(defun adjacent? (head tail)
  (<= (abs (- head tail))
      (abs #C(1 1))))

(defun approach-head (tail head)
  "Move tail one unit closer to head"
  (let ((dist (- head tail)))
    (complex (if (plusp (realpart dist))
                 (min (realpart dist) 1)
                 (max (realpart dist) -1))
             (if (plusp (imagpart dist))
                 (min (imagpart dist) 1)
                 (max (imagpart dist) -1)))))

(defvar *day9-sample2* '(
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20))

(defun day9-part2 ()
  (let ((head (complex 0 0))
        (tails (coerce (loop for i from 1 to 9 collect (complex 0 0)) 'vector))
        (last-tail-positions (list)))
    (loop for (direction amount) on *day9-input* by #'cddr
          do
          (dotimes (i amount)
            (case direction
              (r (incf head 1))
              (l (decf head 1))
              (u (incf head #C(0 1)))
              (d (decf head #C(0 1))))
            (let ((ref-head head))
              (loop for tail-idx from 0 below (length tails)
                    do
                    (when (not (adjacent? ref-head (aref tails tail-idx)))
                      (when (= tail-idx (1- (length tails)))
                        (push (aref tails tail-idx) last-tail-positions))
                      (incf (aref tails tail-idx) (approach-head (aref tails tail-idx) ref-head)))
                    (setf ref-head (aref tails tail-idx))))))
    (1+ (length (remove-duplicates last-tail-positions)))))

;;;; day 10

(defparameter *day10-sample* "noop
addx 3
addx -5")

(defclass simple-cpu ()
  ((current-cycle :accessor .current-cycle :initform 0)
   (reg-X :accessor .reg-X :initform 1)))

(defmethod instruction-cycles ((instruction (eql 'noop)))
  1)
(defmethod instruction-cycles ((instruction (eql 'addx)))
  2)
(defmethod execute-after ((cpu simple-cpu) (instruction (eql 'addx)) &rest args)
  (incf (.reg-X cpu) (first args)))

(defun iter-cycles (cpu cycles)
  "Iterates cpu by cycles, returning a non-zero signal strength
   if we pass over an interesting cycle number"
  (let ((strength 0))
    (dotimes (i cycles)
      (incf (.current-cycle cpu))
      (when (= 20 (mod (.current-cycle cpu) 40)) ; triggers
        (setf strength (* (.current-cycle cpu) (.reg-X cpu)))))
    strength))

(defun day10-part1 ()
  (let ((cpu (make-instance 'simple-cpu))
        (strength-sum 0))
    (loop for instruction in (cl-ppcre:split "\\n" *day10-input*)
          do
          (cond
            ((str:starts-with? "noop" instruction)
             (incf strength-sum (iter-cycles cpu (instruction-cycles 'noop))))

            ((str:starts-with? "addx" instruction)
             (incf strength-sum (iter-cycles cpu (instruction-cycles 'addx)))
             (execute-after cpu 'addx (parse-integer (second (str:split " " instruction)))))))
    strength-sum))

(defun print-crt (crt)
  (loop for r below (array-dimension crt 0) do
        (loop for c below (array-dimension crt 1) do
              (format t "~a" (aref crt r c)))
        (format t "~%")))

(defun iter-cycles2 (cpu cycles crt)
  "Iterates cpu by cycles, also drawing to crt each cycle."
  (let ((current-sprite-left (1- (.reg-x cpu))))
    (dotimes (i cycles)
      (setf (row-major-aref crt (mod (.current-cycle cpu) (array-total-size crt)))
            (if (<= current-sprite-left (mod (.current-cycle cpu) (array-dimension crt 1)) (+ 2 current-sprite-left))
                #\#
                #\.))
      (incf (.current-cycle cpu)))))

(defun day10-part2 ()
  (let ((cpu (make-instance 'simple-cpu))
        (crt (make-array '(6 40) :initial-element #\.)))
    (loop for instruction in (cl-ppcre:split "\\n" *day10-input*)
          do
          (cond
            ((str:starts-with? "noop" instruction)
             (iter-cycles2 cpu (instruction-cycles 'noop) crt))

            ((str:starts-with? "addx" instruction)
             (iter-cycles2 cpu (instruction-cycles 'addx) crt)
             (execute-after cpu 'addx (parse-integer (second (str:split " " instruction)))))))
    (print-crt crt)))

;;;; day 11

(defvar *day11-sample* "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defclass monkey ()
  ((id :accessor .id :initarg :id)
   (items :accessor .items :initarg :items)
   (operation :accessor .operation :initarg :operation)
   (test :accessor .test :initarg :test)
   (inspections :accessor .inspections :initform 0)))

(defmethod print-object ((self monkey) stream)
  (print-unreadable-object (self stream :type t :identity t)
    ;(format stream "id ~a items ~a operation ~a test ~a" (.id self) (.items self) (.operation self) (.test self))))
    (format stream "id ~a items ~a inspections ~a" (.id self) (.items self) (.inspections self))))

(defun make-monkeys (input)
  (loop for monkey in (cl-ppcre:split "\\n\\n" input) collect
      (let+ (((id starting op test if-test if-not-test) (cl-ppcre:split "\\n" monkey))
             (starting (second (cl-ppcre:split ":" starting)))
             (starting (mapcar #'parse-integer (cl-ppcre:split "," starting)))
             (items (make-array 10 :fill-pointer 0)))
        (loop for el in starting do (vector-push el items))
        (make-instance 'monkey
                       :id id
                       :items items
                       :operation (let ((operation (read-from-string (uiop:strcat "(" (second (cl-ppcre:split "= " op)) ")"))))
                                    (rotatef (elt operation 0) (elt operation 1))
                                    (eval `(lambda (old) ,operation)))
                       :test (lambda (n)
                               (if (zerop (mod n (parse-integer (second (cl-ppcre:split "by " test)))))
                                   (parse-integer (second (cl-ppcre:split "monkey " if-test)))
                                   (parse-integer (second (cl-ppcre:split "monkey " if-not-test)))))))))
(vector-push 3 (make-array 5 :fill-pointer 0))

(defun day11 ()
  ; part 1
  (let ((monkeys (make-monkeys *day11-sample*)))
    (dotimes (round 20)
      (loop for monkey in monkeys do
            (loop for item across (.items monkey) do
                  ; inspect
                  (setf item (funcall (.operation monkey) item))
                  (incf (.inspections monkey))
                  ; relief
                  (setf item (floor (/ item 3)))
                  ; test and throw
                  (let ((next-monkey (funcall (.test monkey) item)))
                    (vector-push item (.items (elt monkeys next-monkey)))))
            ; monkey can't throw to itself, so after processing, array can be 'emptied'
            (setf (fill-pointer (.items monkey)) 0)))
    (apply #'* (subseq (sort (mapcar #'.inspections monkeys)  #'>) 0 2)))
  )


#|
Coming back to part 2 later, probably some trick with relativley prime numbers or the graph flow reduction?
Better data structures won't really help though I did refactor the starting items to a max-length fixed array...
  ; part 2
  (let ((monkeys (make-monkeys *day11-sample*)))
    (dotimes (round 10000)
      (loop for monkey in monkeys do
            (loop for item across (.items monkey) do
                  ; inspect
                  (setf item (funcall (.operation monkey) item))
                  (incf (.inspections monkey))
                  ; relief
                  (setf item (floor (/ item 17)))
                  ; test and throw
                  (let ((next-monkey (funcall (.test monkey) item)))
                    (vector-push item (.items (elt monkeys next-monkey)))))
            ; monkey can't throw to itself, so after processing, array can be 'emptied'
            (setf (fill-pointer (.items monkey)) 0)))
    monkeys)
|#

;;;; day 12

(defparameter *day12-sample* "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defun make-heightmap (input)
  (let* ((rows (cl-ppcre:split "\\n" input))
         (n-cols (length (first rows)))
         (map (make-array (list (length rows) n-cols))))
    (loop for r below (length rows) do
          (loop for c below n-cols do
                (setf (aref map r c) (elt (elt rows r) c))))
    map))

(defun elevation (char)
  (case char
    (#\S (char-code #\a))
    (#\E (char-code #\z))
    (t (char-code char))))

(defun valid-neighbors (heightmap location)
  "Up/down/left/right, within bounds, and destination must be at most one step higher.
   Assume cost of each step is 1."
  (let+ (((rows cols) (array-dimensions heightmap))
         ((r c) location)
         (dirs (list (list r (1- c))
                     (list r (1+ c))
                     (list (1- r) c)
                     (list (1+ r) c))))
    (mapcar (lambda (neighbor)
              (list neighbor 1))
            (remove-if-not (lambda (el)
                             (let+ (((el-r el-c) el))
                               (and
                                 (and (<= 0 el-r (1- rows))
                                      (<= 0 el-c (1- cols)))
                                 (>= (1+ (elevation (aref heightmap r c)))
                                     (elevation (aref heightmap el-r el-c))))))
                           dirs))))

(defun day12 ()
  ;; part 1:
  (let* ((heightmap (make-heightmap *day12-input*))
         (pathfinder (make-instance 'lgame.pathfinding::A*
                                    :size (array-dimensions heightmap)
                                    ;:start-pos '(0 0)
                                    :start-pos '(20 0)
                                    ;:end-pos '(2 5)
                                    :end-pos '(20 58)
                                    :neighbor-fn (lambda (location)
                                                   (valid-neighbors heightmap location)))))
    (lgame.pathfinding:compute-path pathfinder :single-step? t :new-request? t)
    (1- (length (lgame.pathfinding:.waypoint-list pathfinder))))

  ;; part 2:
  (let* ((heightmap (make-heightmap *day12-input*))
         (lowest-elevation-starting-positions
           (loop for r below (array-dimension heightmap 0) append
                 (loop for c below (array-dimension heightmap 1)
                       if (= (elevation #\a) (elevation (aref heightmap r c)))
                       collect (list r c)))))
    (loop for starting in lowest-elevation-starting-positions minimize
          (let ((pathfinder (make-instance 'lgame.pathfinding:A*
                                           :size (array-dimensions heightmap)
                                           :start-pos starting
                                           :end-pos '(20 58)
                                           :neighbor-fn (lambda (location)
                                                          (valid-neighbors heightmap location)))))
            (lgame.pathfinding:compute-path pathfinder :single-step? t :new-request? t)
            (let ((path (lgame.pathfinding:.waypoint-list pathfinder)))
              (if (plusp (length path)) ; only consider where there is a path
                  (1- (length path))
                  most-positive-fixnum))))))

