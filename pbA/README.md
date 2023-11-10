
# [Problem A - See to believe](./pbA.pdf)


## Problem description
- The sequence of integers $S_n$ called Schröder models the following combinatorial phenomenon: In a grid of size n × n, how many ways do I have to walk from the lower left corner to the upper right corner without ever crossing the diagonal `(0, 0) - (n, n)` and using exclusively the paths up (↑), to the right (→) and in diagonal mount to the right (↗).

To calculate $S_n$ for a given integer n we can use two equivalent recursive definitions:
$$S_0 = 1$$

$$S1 = 2$$

$$S_n = 3S_n − 1 + \sum_{k=1}^{n-1} S_k S_{n−k−1}$$

**or** 

$$S_0 = 1$$

$$S1 = 2$$

$$S_n = {6n - 3 \over n + 1} S_{n − 1} - {n - 2 \over n + 1} S_{n-2}$$

**For the $S_n$ in both definitions, $n > 1$.**


- It seems difficult to believe that they calculate the same sequence!
- They don't even seem to have the same complexity. By complexity, we mean the number of calls to S. So, calling S0 or S1 in the two definitions has a cost of 1. However, calling S3 with the first definition does not seem to have the same cost as with the second definition!
- The surprise does not end here!
- It is also said that the sequence is diabolical, that its complexity is monumental and that it calculates values ​​quickly enormous.
- To us, it is a half-truth! We affirm that the sequence can be calculated in linear time even if, in fact, the calculated values ​​tend to the enormous.
- In this exercise, we propose to validate or invalidate these statements experimentally.

### Warning
1. If we wait for a direct implementation, faithful to the definitions of the sequence for the first part of the exercise, we expect an optimized implementation for the second part of the exercise.
2. The second part of the exercise needs a careful implementation of the sequence. First, it is necessary a clearly efficient implementation, as referred to in the previous point. Second, because it considers larger input values, the sequence will very quickly return values ​​that clearly exceed the size of the integers. For this, it is advisable to use arbitrary precision arithmetic. Such functionality can be found in the ***zarith* library**.

## Input
- One line with two integers `a` and `b` separated by a space.

## Output
- A first line with two integers `u` and `v` separated by a space. The integer `u` is the result of $S_a$ by the first definition of the *Schröder* sequence. The integer `v` is the number of times the recursive function that faithfully implements the first definition of $S$ is called.
- A second line with two integers `x` and `y` separated by a space. The integer `x` is the result of $S_a$ by the second definition of the Schröder sequence. The integer `y` is the number of times the recursive function that faithfully implements the second definition of $S$ is called.
- A third line with an integer `z`. This integer is $S_b$.


## Limits
$0 \leq a \leq 20$ and $0 \leq b \leq 10000$

## Input example
```
6 100
```

## Output example
```
1806 70
1806 25
28747611153504860266534250007458881388313583561117443629896620307440340890
```

## Explanation
- For the first integer of the input, 6, the value of $S_6$ was calculated with the first definition of the sequence. The result was 1806, and it was necessary to execute 70 calls to the function.
- In the second line, the same results are presented, but using the second definition of the sequence. Which naturally gives the same value for the term $S_6$, 1806. But the number of calls to the function is much more interesting, here 25.
- The third line presents the value of $S_{100}$.

## Compile and run with input example
```bash
cd pbA/
dune exec pbA < input
```
