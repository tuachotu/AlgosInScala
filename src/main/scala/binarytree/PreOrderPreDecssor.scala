package binarytree

object PreOrderPreDecessor extends App{
  sealed trait BinaryTree {
    def value: Int
    def left : BinaryTree
    def right : BinaryTree
    def empty : Boolean
    def rightMost: Option[Int] = {
      if (empty) None
      else if (right.empty) Some(value)
      else right.rightMost
    }

    def preOrder: List[Int] ={
      if (empty) List.empty[Int] else value::(left.preOrder++right.preOrder)
    }

    def preOrderPreDecessor(v: Int, parent: BinaryTree = EmptyNode): Int = {
      if (empty) -1
      else if (value == v) parent.value
      else if (parent.empty) this.preOrderPreDecessor(v, this)
      else if (parent.left.empty) parent.value
      else parent.left.rightMost.get
      }

  }

  case object EmptyNode extends BinaryTree {
    def value: Int = ???
    def left: BinaryTree = ???
    def right: BinaryTree = ???
    def empty: Boolean = true
  }

  case class BinaryTreeNode (value: Int, left: BinaryTree = EmptyNode, right: BinaryTree = EmptyNode) extends BinaryTree {
    def empty: Boolean = false
  }

  val input = BinaryTreeNode(20,
                BinaryTreeNode(10,
                  BinaryTreeNode(4),
                  BinaryTreeNode(18,
                    BinaryTreeNode(14,
                      BinaryTreeNode (13),
                      BinaryTreeNode(15)
                    ),
                    BinaryTreeNode(19)
                  )
                ),
                BinaryTreeNode(26,
                  BinaryTreeNode(24),
                  BinaryTreeNode(17)
                )
              )

  println(input.preOrder.mkString(","))
  println(input.preOrderPreDecessor(4))
  println(input.preOrderPreDecessor(19))

}
