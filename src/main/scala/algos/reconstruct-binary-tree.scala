package algos

import scala.collection.immutable.Range
import BinaryTree.TraversalOrders.{PREORDER, INORDER}

object BinaryTreeReconstructor {
  
  /**
   * Given inorder and preorder traversals, construct the binary tree
   */
  def reconstruct[T](inorder: Array[T], preorder: Array[T]): Option[BinaryTree[T]] = {
    
    def search(k: T, start: Int, len: Int): Int = {
      for (i <- Range(start, start + len))
        if (k == inorder(i))
          return i
          
      throw new Exception("impossible case reached! Input could be incorrect")
    }
    
    /** inner function that works on sections of array rather than the whole array */
    def construct( inorder_start: Int, preorder_start: Int,
                   len: Int ): Option[BinaryTree[T]] = len match {
      case 0 => None
      case _ =>
        val root_val = preorder(preorder_start)
        val inorder_pos_of_root = search(root_val, inorder_start, len)
        val node = BinaryTree(root_val)
          
        val left_slice_len = inorder_pos_of_root - inorder_start
        node.left = construct(inorder_start, preorder_start+1, left_slice_len)
          
        val right_slice_len = len - left_slice_len - 1
        node.right = construct(inorder_pos_of_root + 1,
                               preorder_start + 1 + left_slice_len,
                               right_slice_len)
        // return
        Some(node)
    }
    
    construct(0, 0, inorder.length)
  }
}

object BinaryTreeReconstructionTester {
  def main(args: Array[String]): Unit = {
    
    val inorder  = Array(5, 12, 23, 24, 25, 28, 44, 45, 46, 47, 67, 68)
    val preorder = Array(45, 23, 12, 5, 44, 28, 24, 25, 67, 46, 47, 68)
    
    val tree = BinaryTreeReconstructor.reconstruct(inorder, preorder)
    
    println("After reconstruction: ")
    
    print("inorder : ")
    BinaryTree.tree_println(INORDER)(tree)
    
    print("preorder : ")
    BinaryTree.tree_println(PREORDER)(tree)
  }
    
}