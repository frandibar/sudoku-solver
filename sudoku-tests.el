;; sudoku-tests.el -- Test suite for sudoku-solver

;; Run the tests with M-x ert RET t RET

(require 'sudoku-solver)

(ert-deftest test-col ()
  (let ((grid ["316472895"
               "478159362"
               "295836417"
               "524763981"
               "769281543"
               "183594276"
               "847315629"
               "951628734"
               "632947158"]))
    (should (equal (col 0 grid) (string-to-list "342571896")))
    (should (equal (col 1 grid) (string-to-list "179268453")))
    (should (equal (col 8 grid) (string-to-list "527136948")))
    ))

(ert-deftest test-setval ()
  (let ((grid ["316472895"
               "478159362"
               "295836417"
               "524763981"
               "769281543"
               "183594276"
               "847315629"
               "951628734"
               "632947158"]))
    (setval ?A 0 0 grid)
    (setval ?B 8 8 grid)
    (setval ?C 0 8 grid)
    (setval ?D 8 0 grid)
    (should (equal grid
                   ["A1647289C"
                    "478159362"
                    "295836417"
                    "524763981"
                    "769281543"
                    "183594276"
                    "847315629"
                    "951628734"
                    "D3294715B"]))
    ))

(ert-deftest test-cuad ()
  (let ((grid ["316472895"
               "478159362"
               "295836417"
               "524763981"
               "769281543"
               "183594276"
               "847315629"
               "951628734"
               "632947158"]))
    (should (equal (cuad 0 grid) "316478295"))
    (should (equal (cuad 8 grid) "629734158"))
    ))

(ert-deftest test-valid ()
  (should (valid "123456789"))
  (should (not (valid "012345678"))))

(ert-deftest test-check ()
  (let ((grid ["316472895"
               "478000000"
               "295000000"
               "500000000"
               "700000000"
               "100000000"
               "800000000"
               "900000000"
               "600000000"]))
    (should (equal (check grid)
                   '((t nil nil nil nil nil nil nil nil)
                     (t nil nil nil nil nil nil nil nil)
                     (t nil nil nil nil nil nil nil nil))))))

(ert-deftest test-solved ()
  (let ((grid-ok ["316472895"
                  "478159362"
                  "295836417"
                  "524763981"
                  "769281543"
                  "183594276"
                  "847315629"
                  "951628734"
                  "632947158"])
        (bad ["316472895"
              "478000000"
              "295000000"
              "500000000"
              "700000000"
              "100000000"
              "800000000"
              "900000000"
              "600000000"]))
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
  (should (equal (missing "123456789") '()))
  (should (equal (missing "000012345") (string-to-list "6789"))))

(ert-deftest test-candidates ()
  (let ((grid ["316472895"
               "478000000"
               "295000000"
               "500000000"
               "700000000"
               "100000000"
               "800000000"
               "900000000"
               "600000000"]))
    (should (equal (candidates 0 0 grid) '()))
    (should (equal (candidates 1 3 grid) (string-to-list "13569")))
    (should (equal (candidates 8 8 grid) (string-to-list "1234789")))
    ))

(ert-deftest test-next ()
  (should (equal (next 0 0) '(0 1)))
  (should (equal (next 0 1) '(0 2)))
  (should (equal (next 0 8) '(1 0)))
  (should (equal (next 8 8) nil)))

(ert-deftest test-list->vector ()
  (should (equal (list->vector '(1 2 3)) [1 2 3])))

(ert-deftest test-sudoku ()
  (let ((level1 ["006402005"
                 "078000000"
                 "090830400"
                 "504763901"
                 "769281000"
                 "103004006"
                 "000315020"
                 "901600030"
                 "600907050"])

        (level2 ["410000000"
                 "020008610"
                 "000040753"
                 "705002800"
                 "000813970"
                 "800070300"
                 "002786000"
                 "000005006"
                 "340009000"])

        (level3 ["120090503"
                 "000013020"
                 "039025060"
                 "200000090"
                 "408030600"
                 "000168000"
                 "950000000"
                 "000002307"
                 "000700004"])

        (level4 ["010000000"
                 "070000406"
                 "000934000"
                 "002608000"
                 "905000002"
                 "000000069"
                 "000000627"
                 "700210030"
                 "200350004"]))

    (should (equal (sudoku level1)
                   ["316472895"
                    "478159362"
                    "295836417"
                    "524763981"
                    "769281543"
                    "183594276"
                    "847315629"
                    "951628734"
                    "632947158"]))

    (should (equal (sudoku level2)
                   ["413657298"
                    "527398614"
                    "689241753"
                    "735962841"
                    "264813975"
                    "891574362"
                    "152786439"
                    "978435126"
                    "346129587"]))

    (should (equal (sudoku level3)
                   ["126897543"
                    "574613829"
                    "839425761"
                    "263574198"
                    "418239675"
                    "795168432"
                    "957341286"
                    "641982357"
                    "382756914"]))

    (should (equal (sudoku level4)
                   ["419765283"
                    "573821496"
                    "628934751"
                    "142698375"
                    "965173842"
                    "837542169"
                    "351489627"
                    "794216538"
                    "286357914"]))
    ))
