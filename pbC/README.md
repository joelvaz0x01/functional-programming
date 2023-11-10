
# [Problem C - The Futoshiki Game](./pbC.pdf)

## Problem description
- The challenge of this exercise is the complete algorithmic resolution of a game called *Futoshiki*.
- The game is presented on a square board of n by n where certain pairs of cells indicate an order relationship.
- It is intended to fill each row and each column with the 4 digits 1,2,3 and 4 so that the order relationships indicated on the board are respected (for example the digit in the upper left corner must be greater than the first digit of the next line).
- We will consider in this exercise boards of size between `4` and `9`.

## Input
- Knowing that the cells of the `n` × `n` size board can be referenced by their coordinates from `(0, 0)` to `(n-1, n-1)`, the input of the problem is given as follows:
- A line with the integer `n`.
- A line with the integer `p` of order restrictions that the board contains. For reference, the board in the example has 4 order restrictions.
- The remaining `p` lines contain the position of each restriction given by two pairs `(a, b)` and `(c, d)`.

For example, the line
```
2 1 3 1
```
indicates that the content of the second cell of the third line (ie at position `(2, 1)`) must be greater than the content of the second cell of the fourth line (i.e. at position `(3, 1)`).


## Output
- There are two possible outputs. Either the proposed game has a solution or it has no solution.
- If the game has no solution, then the output consists of a single line with the word `IMPOSSIBLE`.
- If the game has a solution (it may not be unique), the smallest solution is presented in lexicographic order. That is, the solution that respects the "*dictionary order*" when read from left to right of the first line (top) to the last line.

## Limits:
$4 \leq n \leq 9$

## Input example
```
4
4
0 0 1 0
0 1 0 0
2 1 3 1
3 1 3 2
```


## Output example
```
3 4 2 1
2 1 3 4
1 3 4 2
4 2 1 3
```

## Compile and run with input example
```bash
cd pbC/
dune exec pbC < input
```
