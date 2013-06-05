package algos

import scala.annotation.tailrec


object MaxSubArray {
  
  /**
   * Given an array of values, find the contiguous sub-array 
   * that has the largest sum.
   */
  
  /* Functional way; Tail recursive */
  def getMaximumSum(numbers: Array[Int]): Int = {
    
    @tailrec
    def maxSum(maxSoFar: Int, currSum: Int, remaining: Array[Int]): Int = {
      if (remaining.isEmpty)
        return maxSoFar
      
      val newMaxSoFar = Math.max(maxSoFar, currSum + remaining.head)
      val newCurrSum = if (currSum + remaining.head  < 0) { 0 } 
      				   else { currSum + remaining.head }
      
      maxSum(newMaxSoFar, newCurrSum, remaining.tail)
    }
    
    maxSum(scala.Int.MinValue, 0, numbers)
    
  }
  
  /* Traditional iterative way */
  def getMaximumSumIterative(numbers: Array[Int]): Int = {
    var currSum = 0
    var maxSoFar = scala.Int.MinValue
    for (n <- numbers) {
      currSum += n
      maxSoFar = Math.max(maxSoFar, currSum)
      if (currSum < 0)
        currSum = 0
    }
    
    maxSoFar
  }

  def main(args: Array[String]): Unit = {
    val test_arrays: Map[Array[Int], Int] = Map(
        Array(2, -23, 42, 5)  -> 47,
        Array(-1, -2, -3, -5) -> -1,
        Array(2, -4, 42, 5)   -> 47,
        Array(2, -1, 3)       -> 4
    )
    
    for ((arr, expectedAnswer) <- test_arrays)
      assert(getMaximumSum(arr) == expectedAnswer)
      
    for ((arr, expectedAnswer) <- test_arrays)
      assert(getMaximumSumIterative(arr) == expectedAnswer)
      
     println ("All tests passed")

  }

}
