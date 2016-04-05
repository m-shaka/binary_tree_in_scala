package tree

abstract class Tree[A <% Ordered[A]] {
  def contains(n: A): Boolean
  def insert(n: A): Tree[A]
}
case class Empty[A <% Ordered[A]]() extends Tree[A] {
  def contains(n: A): Boolean = false
  def insert(n: A): Tree[A] = Node(Empty[A], n, Empty[A])
}

case class Node[A <% Ordered[A]](left: Tree[A], value: A, right: Tree[A]) extends Tree[A] {
  def contains(n: A): Boolean =
    if (value == n) true
    else if (value < n) right.contains(n)
    else left.contains(n)

  def insert(n: A): Tree[A] =
    if (value == n) this
    else if (value < n) Node(left, value, right.insert(n))
    else Node(left.insert(n), value, right)

}

// object Tree {
//   def contains(tree: Tree[Int], n: Int): Boolean =
//     tree match {
//       case Empty => false
//       case Node(l, v, r) =>
//         if (n > v) contains(r, n)
//         else if (n < v) contains(l, n)
//         else true
//     }
// }
