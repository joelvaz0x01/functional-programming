
# [Problem B - Common mutation](./pbB.pdf)

## Problem description
- Imagine the following hypothetical scenario: the Earth was exposed to an unknown radiation coming from space that subtly altered the genes of the world population. Although harmless in appearance, this mutation deserves study by scientists.
- They launched a study in the different communities to which they have access. It seems that mutations are formed as mutations exist around the individual, family, neighbors and community. It is necessary to study these relationships! It is relevant to try to highlight related mutations, as they appear in different communities and how they interconnect with each other.
- It was possible to encode each existing mutation for an integer. A preliminary study determined that the genealogy of mutations in each community is determined by the order in which mutations appear, and that the dependency relationships in mutations adapt as new mutations appear in the community. It turns out that this hierarchy follows the balance policy AVL of a binary search tree induced by the natural order on integers!
- So, you are given a sequence of integers that need to be organized into a forest of integer trees. It is recalled that, even in the area of data structures, a forest is a set of trees.
- Each tree in the forest represents a community and is a balanced binary search tree according to the AVL policy.
- Given two integers `a` and `b`, we intend in this problem to determine the first *common mutation*, if any, for each of the trees in the forest.

## Input
- On the first line, we find an integer `N` that indicates how many trees the forest has.
- The rest of the input is organized in `N` groups of lines (one for each tree) plus one more line.
- The input for a given tree is organized as follows:
    - A line with the integer `n`, the total number of nodes contained in the tree.
    - Followed by `n` lines, with an integer in each line that represents a value to join in the tree.
    - If there are repeated values, they will not be considered in the tree, by definition of the *binary search trees*.
- After defining the `N` trees, we have a line with two integers `a` and `b` separated by a space.

## Output
- How many *common mutations* were found, presented in the same order as the trees in the forest, where each line identifies the **first** *common mutation* found.
- If there is no *common mutation* for any tree in the forest, a single line with the word `NO` is presented.

## Limits
- The values that make up the trees are positive or null integers of OCaml int type.
- The value `N` has a limit: `0 < N ≤ 5000`. Each tree has a maximum number of elements `10000`.

## Input example
```
3
5
3
2
5
6
1
7
8
2
4
10
34
21
5
5
1
3
6
4
7
1 6
```
This input example introduces `3` trees of respective size `5`, `7` and `5`. In these trees/communities, it is necessary to determine which are the mutations closest to mutations `1` and `6`.

## Output example
```
3
3
```

## Compile and run with input example
```bash
cd pbB/
dune exec pbB < input
```
