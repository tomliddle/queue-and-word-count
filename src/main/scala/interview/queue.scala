package interview


/*
 Implement the following api for a FIFO (first-in, first-out) Queue with the indicated complexities

 This should be an *immutable* queue
 */
trait Queue[T] {
	protected val in = List[T]()
	protected val out = List[T]()

	// O(1)
	def isEmpty: Boolean = in.isEmpty && out.isEmpty

	// O(1)
	def insert(t: T): Queue[T] = getQueue(t :: in, out)

	// O(1)
	def head: Option[T]= {
		out.headOption match {
			case a @ Some(head) => a
			case None => in.lastOption
		}
	}

	// O(1) amortised
	def tail: Queue[T] = {
		if (out.nonEmpty) getQueue(in, out.tail)
		else if (in.nonEmpty) getQueue(Nil, in.reverse.tail)
		else throw new NoSuchElementException("tail on empty queue")
	}

	private def getQueue(inList: List[T], outList: List[T]): Queue[T] = {
		new Queue[T]{
			override val in = inList
			override val out = outList
		}
	}
}

object Queue {
	def empty[T]: Queue[T] = new Queue[T] {}
}
