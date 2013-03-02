;; sudoku-solver.el -- Sudoku puzzle solver for emacs

;; Run these commands in the *scratch* buffer to load:
;; (add-to-list 'load-path /path/to/dir/for/this/file)
;; (require 'sudoku-solver)
;;
;; The solution for a puzzle can be obtained in any of two ways:
;; 1)
;;
;; (let ((puzzle [[4 1 0 0 0 0 0 0 0]  ; sample puzzle
;;                [0 2 0 0 0 8 6 1 0]
;;                [0 0 0 0 4 0 7 5 3]
;;                [7 0 5 0 0 2 8 0 0]
;;                [0 0 0 8 1 3 9 7 0]
;;                [8 0 0 0 7 0 3 0 0]
;;                [0 0 2 7 8 6 0 0 0]
;;                [0 0 0 0 0 5 0 0 6]
;;                [3 4 0 0 0 9 0 0 0]]))
;;   (sudoku puzzle))
;;
;; Empty spaces should be completed with 0.
;;
;; 2) Create an empty buffer and enter the puzzle numbers without brackets.
;;    (suppress empty spaces at line endings)
;; M-x sudoku-solve-buffer RET
;; i.e.
;; ------ buffer starts here --------
;; 4 1 0 0 0 0 0 0 0
;; 0 2 0 0 0 8 6 1 0
;; 0 0 0 0 4 0 7 5 3
;; 7 0 5 0 0 2 8 0 0
;; 0 0 0 8 1 3 9 7 0
;; 8 0 0 0 7 0 3 0 0
;; 0 0 2 7 8 6 0 0 0
;; 0 0 0 0 0 5 0 0 6
;; 3 4 0 0 0 9 0 0 0
;;
;; Solution
;; +-------+-------+-------+
;; ┃ 4 1 3 ┃ 6 5 7 ┃ 2 9 8 ┃
;; ┃ 5 2 7 ┃ 3 9 8 ┃ 6 1 4 ┃
;; ┃ 6 8 9 ┃ 2 4 1 ┃ 7 5 3 ┃
;; +-------+-------+-------+
;; ┃ 7 3 5 ┃ 9 6 2 ┃ 8 4 1 ┃
;; ┃ 2 6 4 ┃ 8 1 3 ┃ 9 7 5 ┃
;; ┃ 8 9 1 ┃ 5 7 4 ┃ 3 6 2 ┃
;; +-------+-------+-------+
;; ┃ 1 5 2 ┃ 7 8 6 ┃ 4 3 9 ┃
;; ┃ 9 7 8 ┃ 4 3 5 ┃ 1 2 6 ┃
;; ┃ 3 4 6 ┃ 1 2 9 ┃ 5 8 7 ┃
;; +-------+-------+-------+

(require 'cl)                           ; for position

(provide 'sudoku-solver)


(defun getval (rown coln grid)
  (aref (row rown grid) coln))

(defun setval (newval rown coln grid)
  (let ((r (row rown grid)))
    (aset r coln newval)
    (aset grid rown r)))

(defun empty (rown coln grid)
  (= (getval rown coln grid) 0))

(defun row (rown grid)
  (aref grid rown))

(defun col (coln grid)
  (mapcar (lambda (r) (aref r coln)) grid))

(defun cuad (n grid)
  (flet ((join (vec)
               (reduce (lambda (x y) (vconcat x y)) vec)))
    (let ((colfrom (* 3 (mod n 3)))
          (rowfrom (cond ((< n 3) 0)
                         ((< n 6) 3)
                         (t 6))))
      (join (mapcar (lambda (r) (subseq r colfrom (+ 3 colfrom)))
                    (subseq grid rowfrom (+ 3 rowfrom)))))))

(defun valid (row-col-cuad)
  "Returns t if ROW-COL-CUAD has all numbers 1 to 9, nil otherwise."
  (and (reduce (lambda (x y) (and x y))
               (mapcar (lambda (k) (position k row-col-cuad))
                       (number-sequence 1 9)))
       t))                              ; hack to return t instead of a number

(defun check (grid)
  "Returns the list ((valid rows) (valid cols) (valid cuads))"
  (mapcar (lambda (fun) (mapcar (lambda (i) (valid (funcall fun i grid)))
                           (number-sequence 0 8)))
          '(row col cuad)))

(defun solved (grid)
  (reduce (lambda (x y) (and x y))
          (reduce (lambda (x y) (append x y))
                  (check grid))))

(defun whatcuad (rown coln)
  (let ((vcuad (cond ((< coln 3) 0)
                     ((< coln 6) 1)
                     (t 2)))
        (hcuad (cond ((< rown 3) 0)
                     ((< rown 6) 1)
                     (t 2))))
    (+ (* 3 hcuad) vcuad)))

(defun missing (row-col-cuad)
  (let (ret)
    (dolist (i (number-sequence 1 9) ret)
      (if (not (position i row-col-cuad))
          (setq ret (append ret (list i)))))))

(defun candidates (rown coln grid)
  (sort (delete-dups (reduce (lambda (x y) (intersection x y))
                             (list (missing (row rown grid))
                                   (missing (col coln grid))
                                   (missing (cuad (whatcuad rown coln) grid)))))
        (lambda (x y) (< x y))))

(defun next (rown coln)
  (if (= coln 8)
      (if (= rown 8)
          nil
        (list (1+ rown) 0))
    (list rown (1+ coln))))

(defun try-next (rown coln grid)
  (let ((nxt (next rown coln)))
    (if (not (empty rown coln grid))
        ;; fixed value, continue with next
        (if nxt
            (try-next (car nxt) (cadr nxt) grid)
          grid)
      (let ((ret)
            (newgrid (copy-grid grid))
            (cand (candidates rown coln grid)))
        (dolist (n cand ret)
          (unless ret
            (setval n rown coln newgrid)
            (if (not nxt)
                (if (solved newgrid)
                    (setq ret newgrid))
              (setq ret (try-next (car nxt) (cadr nxt) newgrid)))))))))

(defun sudoku (grid)
  (let ((tmp max-lisp-eval-depth))
    ;; avoid recursion stack limit
    (setq max-lisp-eval-depth 6000)
    (unwind-protect
        (try-next 0 0 grid)
      ;; restore previous value
      (setq max-lisp-eval-depth tmp))))

(defun copy-grid (grid)
  ;; TODO: there must be another way of copying sequences without references!?
  (let ((newgrid (make-vector 9 0)))
    (dotimes (rown 9 newgrid)
      (aset newgrid rown (make-vector 9 0))
      (dotimes (coln 9)
        (aset (aref newgrid rown) coln (getval rown coln grid))))
    newgrid))

(defun input->grid ()
  "Reads the input from buffer and loads the matrix data structure."
  (list->vector
   (mapcar (lambda (lst) (list->vector lst))
           (mapcar (lambda (lst) (mapcar (lambda (num) (string-to-number num))
                                    lst))
                   (mapcar (lambda (line) (split-string line " "))
                           (split-string (buffer-substring-no-properties 1 162)
                                         "\n" t))))))

(defun list->vector (lst)
  "Returns a vector with the elements of list LST."
  (reduce (lambda (x y) (vconcat x y))
          (mapcar (lambda (x) (vector x))
                  lst)))

(defun show-solution (grid start-grid &optional show-grid)
  "Appends the solution to current buffer.
START-GRID is passed in order to show fixed numbers in different color."
  (font-lock-mode -1)                   ; disable font-lock-mode
  (goto-char (point-max))
  (insert "\nSolution\n")
  (let ((horiz-border "+-------+-------+-------+\n"))
    (dotimes (r 9)
      (if (and show-grid
               (= (mod r 3) 0))
          (insert horiz-border))
      (dotimes (c 9)
        (if (and show-grid
                 (= (mod c 3) 0))
            (insert "┃ "))
        (insert (int-to-string (getval r c grid)) " ")
        (if (not (empty r c start-grid))
            (put-text-property (- (point) 2) (point) 'face 'font-lock-keyword-face)))
      (if show-grid
          (insert "┃ "))
      (insert "\n"))
    (if show-grid
        (insert horiz-border))))

(defun sudoku-solve-buffer ()
  "Reads buffer contents and solves the sudoku puzzle.
Empty spaces should be completed with 0.

For example, if we have a buffer with:
0 0 6 4 0 2 0 0 5
0 7 8 0 0 0 0 0 0
0 9 0 8 3 0 4 0 0
5 0 4 7 6 3 9 0 1
7 6 9 2 8 1 0 0 0
1 0 3 0 0 4 0 0 6
0 0 0 3 1 5 0 2 0
9 0 1 6 0 0 0 3 0
6 0 0 9 0 7 0 5 0

Use M-x sudoku-solve-buffer to show the solution.
"
     (interactive)
     (let ((grid (input->grid)))
       (show-solution (sudoku grid) grid t)))
