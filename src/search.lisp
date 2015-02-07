(in-package :cl-user)
(defpackage gomoku.search
  (:use :cl
        :cl-ppcre)
  (:export :nextstep
           :restart_board))
(in-package :gomoku.search)

(defparameter *board* (make-array '(15 15) :initial-element 'O))

(defparameter *limitation* 1)

(defparameter *illegal-action* (cons -1 -1))

(declaim (optimize speed))

(defun nextstep (json)
  "This function is the main interface to the game. The web server will call this function
   to play game."
  (let ((p_x (parse-integer (cadr json)))
        (p_y (parse-integer (cadddr json)))
        (b *board*))
    (place b 'B p_x p_y)
    (if (win? 'B b)
        (make-result -1 -1 2)
        (let* ((action (alpha-beta-search b)))
          (place b 'w (car action) (cdr action))
          (if (win? 'W b)
              (make-result (car action) (cdr action) 1)
              (make-result (car action) (cdr action) 0))))))

(defun make-result (x y state)
  `(:|x| ,y :|y| ,x :|state| ,state))

(defun restart_board ()
  "This function is another interface to the game. The web server will call it to restart
  the game."
  (clear-board *board*))

(defun clear-board (board)
  (dotimes (i 15 board)
    (dotimes (j 15)
      (setf (aref board i j) 'O))))

(defun place (board color x y)
  (setf (aref board x y) color))

(defun backplace (board x y)
  (setf (aref board x y) 'O))

(defun alpha-beta-search (board)
  "The top level function of alpha-beta-search that is a max-value function, but this
  function returns a action rather than a evaluation value."
  (let ((max_v most-negative-fixnum)
        (ss (successors board))
        (alpha most-negative-fixnum)
        (beta most-positive-fixnum)
        (depth 0)
        (action (cons 0 0)))
    (dolist (s ss)
      (let ((value (min-value
                    (backup-place board 'w s)
                    alpha
                    beta
                    (+ depth 1))))
        (if (> value max_v)
            (setf action s))
        (setf max_v (max max_v value))
       
       
        (setf alpha (max alpha max_v))))
    action))

(defun max-value (board alpha beta depth)
  "The main function of alpha-beta-search for white stone."
  (if (cut-off board depth)
      (complete-eval board)
      (let ((v most-negative-fixnum)
            (ss (successors board)))
        (dolist (s ss)
          (let ((value (min-value
                        (backup-place board 'w s)
                        alpha
                        beta
                        (+ depth 1))))
            
            (setf v (if (> value v) value v))
            (if (>= v beta)
                (return-from max-value v))
            (setf alpha (if (> alpha v) alpha v))))
        v)))

(defun min-value (board alpha beta depth)
  "The main function of alpha-beta-search for black stone."
  (if (cut-off board depth)
      (complete-eval board)
      (let ((v most-positive-fixnum)
            (ss (successors board)))
        (dolist (s ss)
          (let ((value  (max-value
                           (backup-place board 'b s)
                           alpha
                           beta
                           (+ depth 1))))
            (setf v (if (< value v)
                        value
                        v))
            (if (<= v alpha)
                (return-from min-value v))
            (setf beta (if (< beta v) beta v))))
        v)))

(defun cut-off (board depth)
  (or (> depth *limitation*)
      (win? 'w board)
      (win? 'b board)))

(defun backup-place (board color action)
  (let ((new-board (copy-array board)))
    (place new-board color (car action) (cdr action))
    new-board))

(defun successors (board)
  (let* ((non-blanks (all-nonblank board))
         (result '()))
    (dolist (item non-blanks result)
      (let ((arround (arround-postions item)))
        (dolist (a arround)
          (if (and (blank? a board) (not (member a result :test #'equal)))
              (push a result)))))))

(defun arround-postions (position)
  (let* ((x (car position))
         (y (cdr position))
         (temp-positions (list (cons (+ x 1)  (+ y 1))
                               (cons (- x 1)  (- y 1))
                               (cons (+ x 1)  (- y 1))
                               (cons (- x 1)  (+ y 1))
                               (cons x  (- y 1))
                               (cons x  (+ y 1))
                               (cons (+ x 1)  y)
                               (cons (- x 1)  y))))
    (remove nil
            (mapcar #'(lambda (p)
                        (if (and (>= (car p) 0)
                                 (>= (cdr p) 0)
                                 (<= (car p) 14)
                                 (<= (cdr p) 14))
                            p
                            nil))
                    temp-positions))))

(defun win? (color board)
  "Goal test function."
  (let* ((patten `(,color ,color ,color ,color ,color))
         (s-p (apply #'concatenate
                     (cons 'string
                           (mapcar #'symbol-name patten)))))
    (some #'(lambda (n) (all-matches s-p n))
          (lists-to-strings (all-situations board)))))

(defun complete-eval (board)
  "The evaluation function. See the 'powerpoint.ppt'."
  (-  (* 2 (evaluation board 'w))
      (evaluation board 'B)))

(defun evaluation (board color)
  (let ((languages (lists-to-strings (all-situations board))))
    (reduce #'+ languages :key #'(lambda (n)
                                   (eval-item n color)))))

(defun eval-item (str color)
  (let* ((p-color (if (eql color 'b) 'w 'b))
         (pattens `(((O ,color O) 10)
                    ((,p-color ,color O) 1)
                    ((O ,color ,p-color) 1)
                    ((O ,color ,color O) 100)
                    ((,p-color ,color ,color O) 10)
                    ((O ,color ,color ,p-color) 10)
                    ((O ,color ,color ,color O) 1000)
                    ((,p-color ,color ,color ,color O) 100)
                    ((O ,color ,color ,color ,p-color) 100)
                    ((,color ,color O ,color) 100)
                    ((,color O ,color ,color) 100)
                    ((O ,color ,color ,color ,color O) 10000)
                    ((,color ,color O ,color ,color) 1000)
                    ((,color O ,color ,color ,color) 1000)
                    ((,color ,color ,color O ,color) 1000)
                    ((,p-color ,color ,color ,color ,color O) 1000)
                    ((O ,color ,color ,color ,color ,p-color) 1000)
                    ((,color ,color ,color ,color ,color) 10000000)))
         (string-pattens (mapcar #'(lambda (l)
                                     (cons (apply #'concatenate
                                                  (cons 'string
                                                        (mapcar #'symbol-name (car l))))
                                           (cadr l)))
                                 pattens)))
    (reduce #'+ string-pattens :key #'(lambda (str-p) (match-item str-p str)))))

(defun match-item (str-p str)
  (let* ((p (car str-p))
         (val (cdr str-p))
         (l (all-matches p str)))
    (if (null l)
        0
        (* val (/ (length l) 2)))))

(defun lists-to-strings (situations)
  (mapcar #'(lambda (l)
              (apply #'concatenate
                     (cons 'string
                           (mapcar #'symbol-name l))))
          situations))

(defun all-nonblank (board)
  (let ((result '()))
    (dotimes (i 15 result)
      (dotimes (j 15)
        (let ((this-spot (aref board i j)))
          (when (not (eql this-spot 'O))
            (push (cons i j) result)))))))

(defun blank? (position board)
  (eql 'O (aref board (car position) (cdr position))))

(defun all-situations (board)
  (let ((rows '())
        (columns '())
        (diagonals1 '())
        (diagonals2 '())
        (diagonals3 '())
        (diagonals4 '()))
    (dotimes (i 15)
      (let ((temp '()))
        (dotimes (j 15)
          (push (aref board i j) temp))
        (push (nreverse temp) rows)))
    (dotimes (i 15)
      (let ((temp '()))
        (dotimes (j 15)
          (push (aref board j i) temp))
        (push (nreverse temp) columns)))
    (dotimes (i 15)
      (let ((temp '()))
        (dotimes (j 15)
          (if (<= (+ i j) 14)
              (push (aref board (+ i j) j) temp)))
        (push (nreverse temp) diagonals1)))
    (dotimes (i 15)
      (let ((temp '()))
        (dotimes (j 15)
          (if (>= (- i j) 0)
              (push (aref board (- i j) j) temp)))
        (push (nreverse temp) diagonals2)))
    (dotimes (i 15)
      (let ((temp '()))
        (dotimes (j 15)
          (if (<= (+ i j) 14)
              (push (aref board j (+ i j)) temp)))
        (push (nreverse temp) diagonals3)))
    (dotimes (i 15)
      (let ((temp '()))
        (dotimes (j 15)
          (if (<= (+ i j) 14)
              (push (aref board (- 14 j) (+ i j)) temp)))
        (push (nreverse temp) diagonals4)))
    (remove nil
            (mapcar #'(lambda (l)
                        (if (or (< (length l) 5)
                                (every #'(lambda (i) (eql i 'O)) l))
                            nil l))
                    (append (nreverse rows)
                            (nreverse columns)
                            (nreverse diagonals1)
                            (nreverse diagonals2)
                            (nreverse diagonals3)
                            (nreverse diagonals4))))))

(defun copy-array (array &key
                           (element-type (array-element-type array))
                           (fill-pointer (and (array-has-fill-pointer-p array)
                                              (fill-pointer array)))
                           (adjustable (adjustable-array-p array)))
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

