// https://www.hackerrank.com/challenges/fp-array-of-n-elements/problem

def f(num: Int): List[Int] = (0 until num).toList

// There is a bug in HackerRank. readInt doesn't exist anymore in the predefined Solution.
// So I add it manually.
def readInt = scala.io.StdIn.readInt()