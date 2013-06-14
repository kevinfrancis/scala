package algos



case class BinaryTree[T](var root: T) {
  var left: Option[BinaryTree[T]] = None
  var right: Option[BinaryTree[T]] = None
}

object BinaryTree {
  
  object TraversalOrders extends Enumeration {
    type TraversalOrder = Value
    val PREORDER, INORDER, POSTORDER = Value
  }
  
  import TraversalOrders._
  
  def tree_print[T](order: TraversalOrder)(node_option: Option[BinaryTree[T]]): Unit = {
    
    def root_print = node_option match {
      case None => // do nothing
      case Some(n) => print(n.root + " ")
    }
    
    def left_print = node_option match {
      case None => // do nothing
      case Some(n) => tree_print(order)(n.left)
    }
    
    def right_print = node_option match {
      case None => // do nothing
      case Some(n) => tree_print(order)(n.right)
    }
    
    order match {
      case PREORDER  => root_print; left_print; right_print;
      case INORDER   => left_print; root_print; right_print;
      case POSTORDER => left_print; right_print; root_print;
    }
  }
  
  def tree_println[T](order: TraversalOrder)(node_option: Option[BinaryTree[T]]) = {
    tree_print(order)(node_option)
    println()
  }

}