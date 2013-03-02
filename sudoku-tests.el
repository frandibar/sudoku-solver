;; sudoku-tests.el -- Test suite for sudoku-solver

;; Run the tests with M-x ert RET t RET

(require 'sudoku-solver)

(ert-deftest test-col ()
  (let ((grid [[3 1 6 4 7 2 8 9 5]
               [4 7 8 1 5 9 3 6 2]
               [2 9 5 8 3 6 4 1 7]
               [5 2 4 7 6 3 9 8 1]
               [7 6 9 2 8 1 5 4 3]
               [1 8 3 5 9 4 2 7 6]
               [8 4 7 3 1 5 6 2 9]
               [9 5 1 6 2 8 7 3 4]
               [6 3 2 9 4 7 1 5 8]]))
    (should (equal (col 0 grid) '(3 4 2 5 7 1 8 9 6)))
    (should (equal (col 1 grid) '(1 7 9 2 6 8 4 5 3)))
    (should (equal (col 8 grid) '(5 2 7 1 3 6 9 4 8)))
    ))

(ert-deftest test-setval ()
  (let ((grid [[3 1 6 4 7 2 8 9 5]
               [4 7 8 1 5 9 3 6 2]
               [2 9 5 8 3 6 4 1 7]
               [5 2 4 7 6 3 9 8 1]
               [7 6 9 2 8 1 5 4 3]
               [1 8 3 5 9 4 2 7 6]
               [8 4 7 3 1 5 6 2 9]
               [9 5 1 6 2 8 7 3 4]
               [6 3 2 9 4 7 1 5 8]]))
    (setval 'A 0 0 grid)
    (setval 'B 8 8 grid)
    (setval 'C 0 8 grid)
    (setval 'D 8 0 grid)
    (should (equal grid
                   [[A 1 6 4 7 2 8 9 C]
                    [4 7 8 1 5 9 3 6 2]
                    [2 9 5 8 3 6 4 1 7]
                    [5 2 4 7 6 3 9 8 1]
                    [7 6 9 2 8 1 5 4 3]
                    [1 8 3 5 9 4 2 7 6]
                    [8 4 7 3 1 5 6 2 9]
                    [9 5 1 6 2 8 7 3 4]
                    [D 3 2 9 4 7 1 5 B]]))
    ))

(ert-deftest test-cuad ()
  (let ((grid [[3 1 6 4 7 2 8 9 5]
               [4 7 8 1 5 9 3 6 2]
               [2 9 5 8 3 6 4 1 7]
               [5 2 4 7 6 3 9 8 1]
               [7 6 9 2 8 1 5 4 3]
               [1 8 3 5 9 4 2 7 6]
               [8 4 7 3 1 5 6 2 9]
               [9 5 1 6 2 8 7 3 4]
               [6 3 2 9 4 7 1 5 8]]))
    (should (equal (cuad 0 grid)
                   [3 1 6 4 7 8 2 9 5]))))

(ert-deftest test-valid ()
  (should (valid [1 2 3 4 5 6 7 8 9]))
  (should (not (valid [0 1 2 3 4 5 6 7 8]))))

(ert-deftest test-check ()
  (let ((grid [[3 1 6 4 7 2 8 9 5]
               [4 7 8 0 0 0 0 0 0]
               [2 9 5 0 0 0 0 0 0]
               [5 0 0 0 0 0 0 0 0]
               [7 0 0 0 0 0 0 0 0]
               [1 0 0 0 0 0 0 0 0]
               [8 0 0 0 0 0 0 0 0]
               [9 0 0 0 0 0 0 0 0]
               [6 0 0 0 0 0 0 0 0]]))
    (should (equal (check grid)
                   '((t nil nil nil nil nil nil nil nil)
                     (t nil nil nil nil nil nil nil nil)
                     (t nil nil nil nil nil nil nil nil))))))

(ert-deftest test-solved ()
  (let ((grid-ok [[3 1 6 4 7 2 8 9 5]
                  [4 7 8 1 5 9 3 6 2]
                  [2 9 5 8 3 6 4 1 7]
                  [5 2 4 7 6 3 9 8 1]
                  [7 6 9 2 8 1 5 4 3]
                  [1 8 3 5 9 4 2 7 6]
                  [8 4 7 3 1 5 6 2 9]
                  [9 5 1 6 2 8 7 3 4]
                  [6 3 2 9 4 7 1 5 8]])
        (bad [[3 1 6 4 7 2 8 9 5]
              [4 7 8 0 0 0 0 0 0]
              [2 9 5 0 0 0 0 0 0]
              [5 0 0 0 0 0 0 0 0]
              [7 0 0 0 0 0 0 0 0]
              [1 0 0 0 0 0 0 0 0]
              [8 0 0 0 0 0 0 0 0]
              [9 0 0 0 0 0 0 0 0]
              [6 0 0 0 0 0 0 0 0]]))
    (should (solved grid-ok))
    (should (not (solved bad)))))

(ert-deftest test-whatcuad ()
  (should (= (whatcuad 0 0) 0))
  (should (= (whatcuad 0 2) 0))
  (should (= (whatcuad 0 3) 1))
  (should (= (whatcuad 0 5) 1))
  (should (= (whatcuad 0 6) 2))
  (should (= (whatcuad 0 8) 2))
  (should (= (whatcuad 1 0) 0))
  (should (= (whatcuad 2 0) 0))
  (should (= (whatcuad 3 0) 3))
  (should (= (whatcuad 5 0) 3))
  (should (= (whatcuad 6 0) 6))
  (should (= (whatcuad 8 0) 6))
  (should (= (whatcuad 8 8) 8)))

(ert-deftest test-missing ()
  (should (equal (missing [1 2 3 4 5 6 7 8 9]) '()))
  (should (equal (missing [0 0 0 0 1 2 3 4 5]) '(6 7 8 9))))


(ert-deftest test-candidates ()
  (let ((grid [[3 1 6 4 7 2 8 9 5]
               [4 7 8 0 0 0 0 0 0]
               [2 9 5 0 0 0 0 0 0]
               [5 0 0 0 0 0 0 0 0]
               [7 0 0 0 0 0 0 0 0]
               [1 0 0 0 0 0 0 0 0]
               [8 0 0 0 0 0 0 0 0]
               [9 0 0 0 0 0 0 0 0]
               [6 0 0 0 0 0 0 0 0]]))
    (should (equal (candidates 0 0 grid) '()))
    (should (equal (candidates 1 3 grid) '(1 3 5 6 9)))
    (should (equal (candidates 8 8 grid) '(1 2 3 4 7 8 9)))))

(ert-deftest test-next ()
  (should (equal (next 0 0) '(0 1)))
  (should (equal (next 0 1) '(0 2)))
  (should (equal (next 0 8) '(1 0)))
  (should (equal (next 8 8) nil)))

(ert-deftest test-copy-grid ()
  ;; FIXME: sometimes fails!
  (let* ((grid [[3 1 6 4 7 2 8 9 5]
                [4 7 8 1 5 9 3 6 2]
                [2 9 5 8 3 6 4 1 7]
                [5 2 4 7 6 3 9 8 1]
                [7 6 9 2 8 1 5 4 3]
                [1 8 3 5 9 4 2 7 6]
                [8 4 7 3 1 5 6 2 9]
                [9 5 1 6 2 8 7 3 4]
                [6 3 2 9 4 7 1 5 8]])
         (newgrid (copy-grid grid)))
    (should (equal newgrid grid))
    (setval 0 0 0 grid)
    (should (= (getval 0 0 grid) 0))
    (should (= (getval 0 0 newgrid) 3))
    (should (not (equal newgrid grid)))))

(ert-deftest test-list->vector ()
  (should (equal (list->vector '(1 2 3)) [1 2 3])))

(ert-deftest test-sudoku ()
  (let ((level1 [[0 0 6 4 0 2 0 0 5]
                 [0 7 8 0 0 0 0 0 0]
                 [0 9 0 8 3 0 4 0 0]
                 [5 0 4 7 6 3 9 0 1]
                 [7 6 9 2 8 1 0 0 0]
                 [1 0 3 0 0 4 0 0 6]
                 [0 0 0 3 1 5 0 2 0]
                 [9 0 1 6 0 0 0 3 0]
                 [6 0 0 9 0 7 0 5 0]])

        (level2 [[4 1 0 0 0 0 0 0 0]
                 [0 2 0 0 0 8 6 1 0]
                 [0 0 0 0 4 0 7 5 3]
                 [7 0 5 0 0 2 8 0 0]
                 [0 0 0 8 1 3 9 7 0]
                 [8 0 0 0 7 0 3 0 0]
                 [0 0 2 7 8 6 0 0 0]
                 [0 0 0 0 0 5 0 0 6]
                 [3 4 0 0 0 9 0 0 0]])

        (level3 [[1 2 0 0 9 0 5 0 3]
                 [0 0 0 0 1 3 0 2 0]
                 [0 3 9 0 2 5 0 6 0]
                 [2 0 0 0 0 0 0 9 0]
                 [4 0 8 0 3 0 6 0 0]
                 [0 0 0 1 6 8 0 0 0]
                 [9 5 0 0 0 0 0 0 0]
                 [0 0 0 0 0 2 3 0 7]
                 [0 0 0 7 0 0 0 0 4]])

        (level4 [[0 1 0 0 0 0 0 0 0]
                 [0 7 0 0 0 0 4 0 6]
                 [0 0 0 9 3 4 0 0 0]
                 [0 0 2 6 0 8 0 0 0]
                 [9 0 5 0 0 0 0 0 2]
                 [0 0 0 0 0 0 0 6 9]
                 [0 0 0 0 0 0 6 2 7]
                 [7 0 0 2 1 0 0 3 0]
                 [2 0 0 3 5 0 0 0 4]]))

    (should (equal (sudoku level1)
                   [[3 1 6 4 7 2 8 9 5]
                    [4 7 8 1 5 9 3 6 2]
                    [2 9 5 8 3 6 4 1 7]
                    [5 2 4 7 6 3 9 8 1]
                    [7 6 9 2 8 1 5 4 3]
                    [1 8 3 5 9 4 2 7 6]
                    [8 4 7 3 1 5 6 2 9]
                    [9 5 1 6 2 8 7 3 4]
                    [6 3 2 9 4 7 1 5 8]]))

    (should (equal (sudoku level2)
                   [[4 1 3 6 5 7 2 9 8]
                    [5 2 7 3 9 8 6 1 4]
                    [6 8 9 2 4 1 7 5 3]
                    [7 3 5 9 6 2 8 4 1]
                    [2 6 4 8 1 3 9 7 5]
                    [8 9 1 5 7 4 3 6 2]
                    [1 5 2 7 8 6 4 3 9]
                    [9 7 8 4 3 5 1 2 6]
                    [3 4 6 1 2 9 5 8 7]]))

    ;; (should (equal (sudoku level3)
    ;;                [[1 2 6 8 9 7 5 4 3]
    ;;                 [5 7 4 6 1 3 8 2 9]
    ;;                 [8 3 9 4 2 5 7 6 1]
    ;;                 [2 6 3 5 7 4 1 9 8]
    ;;                 [4 1 8 2 3 9 6 7 5]
    ;;                 [7 9 5 1 6 8 4 3 2]
    ;;                 [9 5 7 3 4 1 2 8 6]
    ;;                 [6 4 1 9 8 2 3 5 7]
    ;;                 [3 8 2 7 5 6 9 1 4]]))

    ;; (should (equal (sudoku level4)
    ;;                [[4 1 9 7 6 5 2 8 3]
    ;;                 [5 7 3 8 2 1 4 9 6]
    ;;                 [6 2 8 9 3 4 7 5 1]
    ;;                 [1 4 2 6 9 8 3 7 5]
    ;;                 [9 6 5 1 7 3 8 4 2]
    ;;                 [8 3 7 5 4 2 1 6 9]
    ;;                 [3 5 1 4 8 9 6 2 7]
    ;;                 [7 9 4 2 1 6 5 3 8]
    ;;                 [2 8 6 3 5 7 9 1 4]]))
    ))
