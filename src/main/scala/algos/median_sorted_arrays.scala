package algos
import scala.math.{max, min}

object MedianFinder {
  
  /**
   * median_of_sorted_arrays
   * @param a  Array of Int
   * @param b  Array of Int
   * @return   The median of a and b combined
   * Both arrays a and b should be of the same length
   */
  def median_of_sorted_arrays(a: Array[Int], b: Array[Int]): Double = {

    /* Finds median from two sorted slices */
    @scala.annotation.tailrec
    def median_slices(a_start: Int, b_start: Int, n: Int): Double = n match {
      
      case 1 => ( a(a_start) + b(b_start) ) / 2.0
      
      // Base case for n=2; when both the median numbers are in one array
      // e.g. a=[2, 3];  b = [1, 4] =>  We can't split array "a" further.
      case 2 => ( max(a( a_start ), b( b_start )) + 
                  min(a(a_start+1), b(b_start+1))  ) / 2.0
      case _ =>
        val a_mid = a_start + (n-1)/2
        val b_mid = b_start + (n-1)/2
        
        if (a(a_mid) < b(b_mid))          
          median_slices(a_mid, b_start, 1+n/2) // search a's left and b's right
        else if (a(a_mid) > b(b_mid))
          median_slices(a_start, b_mid, 1+n/2) // search a's right and b's left
        else {
          val incr = (n + 1) % 2
          (a(a_mid) + min(a(a_mid+incr), b(b_mid+incr))) / 2.0
        }
    }
    
    median_slices(0, 0, a.length)
  }
}

object MedianSortedArraysTester {
  def main(args: Array[String]): Unit = {
    
    /* Test cases */
    val test_map = Map(


        Pair(Array(0),
             Array(1)) -> 0.5,
             
        Pair(Array(0, 1),
             Array(1, 2)) -> 1.0,
             
        Pair(Array(2, 3),  // This is why we need a special base case for n = 2
             Array(1, 4)) -> 2.5,

        Pair(Array(0, 2, 4),
             Array(1, 3, 5)) -> 2.5,

        Pair(Array(1, 2, 3),
             Array(4, 5, 6)) -> 3.5,
        
        Pair(Array(4, 5, 6),
             Array(1, 2, 3)) -> 3.5,
             
        Pair(Array(1, 4, 5, 7),
             Array(2, 2, 3, 4)) -> 3.5,
        
        Pair(Array(1, 5, 7, 9, 12),
             Array(1, 13, 14, 15, 16)) -> 10.5,

        Pair(Array(1, 12, 13, 16, 17),
             Array(1, 1, 2, 14, 15)) -> 12.5,
             
        Pair(Array(1, 2, 12, 19, 21, 23),
             Array(20, 22, 24, 26, 28, 30)) -> 21.5,

        Pair(Array(1, 1, 2, 2),
             Array(1, 1, 2, 2)) -> 1.5,
             
        Pair(Array(1, 1, 3, 5),
             Array(1, 1, 2, 4)) -> 1.5,
             
        Pair(Array(1, 1, 1, 2, 4),
             Array(1, 1, 1, 3, 5)) -> 1.0,
          
        Pair(Array(1, 2, 3, 3),
             Array(1, 1, 4, 4)) -> 2.5
    )

    

    for ((Pair(a, b), ans) <- test_map)
      assert(MedianFinder.median_of_sorted_arrays(a , b) == ans)
      
    println("All tests passed")
                                                 
  }
}
