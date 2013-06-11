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
    def median_slices(slice_a: Pair[Int, Int], slice_b: Pair[Int, Int]): Double = {
      
      val Pair(a_left, a_right) = slice_a
      val Pair(b_left, b_right) = slice_b
      val n = (1 + a_right - a_left)
      
      n match {  
        case 1 => (  a(a_left) + b(b_left)  ) / 2.0
        case 2 => (  max(a(a_left), b(b_left)) +
                     min(a(a_right), b(b_right))    ) / 2.0
        case _ =>
          val a_mid = (a_left + a_right) / 2
          val b_mid = (b_left + b_right) / 2
          val incr  = (n+1) % 2  // to accommodate even sized slices
          
          if (a(a_mid) < b(b_mid))
            median_slices(Pair(a_mid+incr, a_right), Pair(b_left, b_mid))
          else if (a(a_mid) > b(b_mid))
            median_slices(Pair(a_left, a_mid), Pair(b_mid+incr, b_right))
          else
            a(a_mid)
      }
    }
  
    median_slices(Pair(0, a.length-1), Pair(0, a.length-1))
  }
}

object MedianSortedArraysTester {
  def main(args: Array[String]): Unit = {
    
    val test_map = Map(

        Pair(Array(0),
             Array(1)) -> 0.5,
             
        Pair(Array(0, 1),
             Array(1, 2)) -> 1.0,
             
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
             Array(20, 22, 24, 26, 28, 30)) -> 21.5
    )

    

    for ((Pair(a, b), ans) <- test_map)
      assert(MedianFinder.median_of_sorted_arrays(a, b) == ans)
      
    println("All tests passed")

                                                 
  }
}
