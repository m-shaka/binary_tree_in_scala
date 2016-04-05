package tree

sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[+A](left: Tree[A], value: A, right: Tree[A]) extends Tree[A]

object Tree {

  def contains(tree: Tree[Int], n: Int): Boolean =
    tree match {
      case Empty => false
      case Node(l, v, r) =>
        if (v == n) true
        else if (v < n) contains(r, n)
        else contains(l, n)
    }

  def insert(tree: Tree[Int], n: Int): Tree[Int] =
    tree match {
      case Empty => Node(Empty, n, Empty)
      case Node(l, v, r) =>
        if (v == n) tree
        else if (v < n) Node(l, v, insert(r, n))
        else Node(insert(l, n), v, r)
    }

  def delete(tree: Tree[Int], n: Int): Tree[Int] =
    tree match {
      case Empty => Empty
      case Node(Empty, v, r) =>
        if (v == n) r
        else if (v < n) delete(r, n)
        else Empty
      case Node(l, v, Empty) =>
        if (v == n) l
        else if (v < n) Empty
        else delete(l, n)
      case Node(l, v, r) =>
        // leftの最大値を持ってくるぞ
        if (v == n) Node(delete(l, max(l)), max(l), r)
        else if (v < n) Node(l, v, delete(r, n))
        else Node(delete(l, n), v, r)
    }

  def max[B >: Int](tree: Tree[Int]): B =
    tree match {
      case Empty => error("the tree has no content.")
      case Node(_, v, Empty) => v
      case Node(_, _, r) => max(r)
    }

}
