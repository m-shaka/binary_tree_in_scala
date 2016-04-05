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
        else if (v < n) Node(Empty, v, delete(r, n))
        else Empty
      case Node(l, v, Empty) =>
        if (v == n) l
        else if (v < n) Empty
        else Node(delete(l, n), v, Empty)
      case Node(l, v, r) if depth(l) > depth(r) =>
        // leftの最大値を持ってくるぞ
        if (v == n) Node(delete(l, max(l)), max(l), r)
        else if (v < n) Node(l, v, delete(r, n))
        else Node(delete(l, n), v, r)
      case Node(l, v, r) if depth(l) <= depth(r) =>
        // rightの最小値を持ってくるぞ
        if (v == n) Node(l, min(r), delete(r, min(r)))
        else if (v < n) Node(l, v, delete(r, n))
        else Node(delete(l, n), v, r)

    }

  // 木の中身を昇順リストにして返す
  def list(tree: Tree[Int]): List[Int] =
    tree match {
      case Empty => Nil
      case Node(l, v, r) => list(l) ::: v :: list(r)
    }

  def max(tree: Tree[Int]): Int =
    tree match {
      case Empty => error("the tree has no content.")
      case Node(_, v, Empty) => v
      case Node(_, _, r) => max(r)
    }

  def min(tree: Tree[Int]): Int =
    tree match {
      case Empty => error("the tree has no content.")
      case Node(Empty, v, _) => v
      case Node(l, _, _) => min(l)
    }

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Empty => 0
      case Node(l, _, r) => 1 + (depth(l) max depth(r))
    }
}
