package algos

/**
 * Functional approach for finding permutations of a list
 * Functional <==> Fun!
 */
object FunctionalPermutations {
  /* 
   * The Seq type would have been more appropriate instead of the List type
   * But adding to the head of a List is O(1) unlike adding to a Seq 
   */
  
  /**
   * @param c   Item that needs to be inserted at all possible positions
   * @param xs  List of items
   * @return    Insert item c at every possible location in xs.
   */
  private def insert[T](c: T, xs: List[T]): List[List[T]] = xs.isEmpty match {
    case true  => List(List(c))
    case false => (c :: xs) :: insert(c, xs.tail).map(xs.head :: _)
  }
  
  /**
   * @param xs   List of items
   * @return     List of permutations
   */
  def permutations[T](xs: List[T]): List[List[T]] = xs.isEmpty match {
    case true => List(List())
    case false => permutations(xs.tail) flatMap (p => insert(xs.head, p))
  }

}

/**
 * Permutations Printer
 */
object PermutationsPrinter {
  
  
  private def swap[T](arr: Array[T], i: Int, j: Int): Unit = {
    val tmp = arr(j)
    arr(j)  = arr(i)
    arr(i)  = tmp
  }
   
  
  /** 
   * Uses iterative approach to printing
   * Needs mutable array
   */
  def print_permutations[T](arr: Array[T]): Unit = {
    
    val length = arr.length
    
    def permute[T](start: Int): Unit = {
      
      if (start == length)
        println(arr.toList)
      
      for (i <- Range(start, length)) {
        swap(arr, start, i)
        permute(start + 1)
        swap(arr, start, i)
      }
    } 
    
    permute(0)
      
  }
}

object Permutations {
  
  def main(args: Array[String]): Unit = {
    println (FunctionalPermutations.permutations(List()))
    println (FunctionalPermutations.permutations(List("single_item")))
    println (FunctionalPermutations.permutations(List("x", "y")))
    println (FunctionalPermutations.permutations(List(1, 2, 3)))
    
    PermutationsPrinter.print_permutations(Array(1, 2, 3))
    PermutationsPrinter.print_permutations("abcd".toArray)
  }

}
