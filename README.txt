Sudoku puzzle solver for Emacs

How to use:

1) launch Emacs
2) load lisp files by evaluating these commands in the scratch buffer
   (add-to-list 'load-path /path/to/dir/for/sudoku-solver.el/file)
   (require 'sudoku-solver)
3) enter puzzle into empty buffer. i.e
   ------ buffer starts here --------
   410000000
   020008610
   000040753
   705002800
   000813970
   800070300
   002786000
   000005006
   340009000
4) M-x sudoku-solve-buffer RET
   After that the solution is appended to the buffer, i.e.

   Solution
   +-------+-------+-------+
   ┃ 4 1 3 ┃ 6 5 7 ┃ 2 9 8 ┃
   ┃ 5 2 7 ┃ 3 9 8 ┃ 6 1 4 ┃
   ┃ 6 8 9 ┃ 2 4 1 ┃ 7 5 3 ┃
   +-------+-------+-------+
   ┃ 7 3 5 ┃ 9 6 2 ┃ 8 4 1 ┃
   ┃ 2 6 4 ┃ 8 1 3 ┃ 9 7 5 ┃
   ┃ 8 9 1 ┃ 5 7 4 ┃ 3 6 2 ┃
   +-------+-------+-------+
   ┃ 1 5 2 ┃ 7 8 6 ┃ 4 3 9 ┃
   ┃ 9 7 8 ┃ 4 3 5 ┃ 1 2 6 ┃
   ┃ 3 4 6 ┃ 1 2 9 ┃ 5 8 7 ┃
   +-------+-------+-------+

Check out the test suites for a better understanding of the code.
