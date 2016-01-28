package interview

/*
 Implement the following api for a FIFO (first-in, first-out) Queue with the indicated complexities

 This should be an *immutable* queue
 */
trait Queue[T] {
  // O(1)
  def isEmpty: Boolean

  // O(1)
  def insert(t: T): Queue[T]

  // O(1)
  def head: Option[T]

  // O(1) amortised
  def tail: Queue[T]
}

object Queue {
  def empty[T]: Queue[T] = ???
}
