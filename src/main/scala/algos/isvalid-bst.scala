package algos

/**
 * Generic Binary Search Tree validator 
 */
object BSTValidator {

  /**
   * is_valid_bst: Check whether a given binary tree is a Binary Search Tree
   * @param node   A BinaryTree node
   * @return       true if the tree is a binary search tree
   *               false otherwise
   */
  def is_valid_bst[T <% Ordered[T]](node: BinaryTree[T]): Boolean = {
    
    /** inner func. to check bounds for current node, and recursively check children nodes   */
    def is_valid_range(n: Option[BinaryTree[T]], min: Option[T], max: Option[T]): Boolean = n match {
      case None => true
      case Some(treeNode) =>
        val lower_bound_ok: Boolean = min match {
          case None        => true  // No lower bound
          case Some(value) => treeNode.root >= value
        }
        
        val upper_bound_ok: Boolean = max match {
          case None        => true // No upper bound
          case Some(value) => treeNode.root <= value
        }
          
        ( (lower_bound_ok && upper_bound_ok) && // check bounds for this node
          is_valid_range(treeNode.left, min, Some(treeNode.root)) &&  // check left sub-tree
          is_valid_range(treeNode.right, Some(treeNode.root), max)  ) // check right sub-tree
          
    }
    
    // No bounds in initial range i.e., like (-infinity, +infinity)
    is_valid_range(Some(node), None, None)
  }
}

object BinarySearchTreeTester {
  def main(args: Array[String]): Unit = {
    test1
    test2
    println("All tests passed")
  }
  
  def test1: Unit = {
    val n40 = BinaryTree(40)
    val n24 = BinaryTree(24)
    val n45 = BinaryTree(45)
    val n23 = BinaryTree(23)
    val n99 = BinaryTree(99)
    val n22 = BinaryTree(22)

    n40.left = Some(n24)
    n40.right = Some(n45)
    
    n24.left = Some(n23)
    n24.right = Some(n99)
    
    n45.left = Some(n22)
    
    assert(BSTValidator.is_valid_bst(n40) == false)
  }
  
  def test2: Unit = {
    val a = BinaryTree("a")
    val b = BinaryTree("b")
    val c = BinaryTree("c")
    val d = BinaryTree("d")
    val e = BinaryTree("e")
    
    c.left = Some(a)
    c.right = Some(e)
    e.left = Some(d)
    a.right = Some(b)
    
    assert(BSTValidator.is_valid_bst(c) == true)
  }
}
