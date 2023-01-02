package binarytree

object FindPAth extends App {

  sealed trait BinaryTree {
    def value: Int
    def left: BinaryTree
    def right: BinaryTree
    def empty: Boolean

    /*
    None -> No path exist
    Empty List -> same node

     */
    def findPath(dest: Int): Option[List[Char]] = {
      if (empty) None
      else {
        if (value == dest) Some(List.empty[Char])
        else {

          val routeLeft = if (!left.empty) {
            left.findPath(dest) match {
              case Some(l) => Some('L'::l)
              case None => None
            }
          } else None

          val routeRight = if (!right.empty) {
            right.findPath(dest) match {
              case Some(r) => Some('R' :: r)
              case None => None
            }
          } else None


          (routeLeft, routeRight) match {
            case (None, None) => None
            case (_, None) => routeLeft
            case (None, _) => routeRight
            case (Some(l1), Some(l2)) if l1.length < l2.length => routeLeft
            case _ => routeRight
          }
        }
      }
    }
  }

  case object EmptyNode extends BinaryTree {
    def value: Int = ???
    def left: BinaryTree = ???
    def right: BinaryTree = ???
    def empty: Boolean = true
  }

  case class BinaryTreeNode(value: Int, left: BinaryTree = EmptyNode, right: BinaryTree = EmptyNode) extends BinaryTree {
    def empty: Boolean = false
  }

  def findPathForNodes(root: BinaryTree, startValue: Int, endValue: Int): String ={
    val res1 = input3.findPath(startValue)
    val res2 = input3.findPath(endValue)
    (res1, res2) match {
      case (None, None) => ""
      case (Some(l), None) => l.mkString("")
      case (None, Some(l)) => l.mkString("")
      case (Some(l1), Some(l2)) =>
        val commonPrefix = l1.zip(l2).takeWhile{case(c1,c2) => c1 == c2}.map(_._1).mkString("")
        val rootToSource = l1.mkString("")
        val rootToDest = l2.mkString("")
        rootToSource.stripPrefix(commonPrefix).map(_ => 'U') + rootToDest.stripPrefix(commonPrefix)
    }
  }

  //   val input1 = BinaryTreeNode(1, BinaryTreeNode(2, BinaryTreeNode(4), BinaryTreeNode(5)), BinaryTreeNode(3))
  //  println(findLeaves(input1))

  //  val input2 = BinaryTreeNode(1, BinaryTreeNode(2, BinaryTreeNode(4), BinaryTreeNode(5, BinaryTreeNode(6))), BinaryTreeNode(3))
  //  println(findLeaves(input2))

//
  val input3 = BinaryTreeNode(1, BinaryTreeNode(2, BinaryTreeNode(4), BinaryTreeNode(5, BinaryTreeNode(6))), BinaryTreeNode(3, BinaryTreeNode(32)))
  println(findPathForNodes(input3,4,5))
  println(findPathForNodes(input3,4,3))




}
