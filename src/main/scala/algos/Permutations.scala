package algos

object Permutations {
  
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
  

  def main(args: Array[String]): Unit = {
    println (permutations(List()))
    println (permutations(List("single_item")))
    println (permutations(List("x", "y")))
    println (permutations(List(1, 2, 3)))
  }

}
