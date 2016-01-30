import java.util.NoSuchElementException

import interview.Queue
import org.scalatest.BeforeAndAfterEach
import org.scalatest.Matchers
import org.scalatest.WordSpec


class queueTest extends WordSpec with Matchers with BeforeAndAfterEach {

	val emptyQueue = Queue.empty[Int]

	"queue" when {

		"calling is empty" should {

			"be true on an empty queue" in {
				emptyQueue.isEmpty should equal(true)
			}

			"be false on a non empty queue" in {
				val queue = emptyQueue.insert(1)

				queue.head should equal(Some(1))
				queue.isEmpty should equal(false)
			}
		}

		"calling insert" should {

			"insert one element on an empty queue" in {
				val queue = emptyQueue.insert(55)

				queue.isEmpty should equal(false)
				queue.head should equal(Some(55))
				queue.tail.isEmpty should equal(true)
			}

			"insert one element on a non empty queue" in {
				val queue = emptyQueue.insert(55).insert(400)

				queue.isEmpty should equal(false)
				queue.tail.tail.isEmpty should equal(true)
			}

		}

		"calling head" should {

			"return None on an empty queue" in {
				emptyQueue.head should equal(None)
			}

			"return the head on a queue with one element" in {
				val queue = emptyQueue.insert(55)

				queue.head should equal(Some(55))
			}

			"return the head on a queue with five elements" in {
				val queue = emptyQueue.insert(77).insert(100).insert(3).insert(1).insert(88)

				queue.head should equal(Some(77))
			}
		}

		"calling tail" should {

			"return an empty queue on a queue with one element" in {
				val queue = emptyQueue.insert(77)

				queue.tail.isEmpty should equal(true)
			}

			"return an queue with one element on a queue with two elements" in {
				val queue = emptyQueue.insert(77).insert(44)

				queue.tail.isEmpty should equal(false)
				queue.head should equal(Some(77))
			}

			"repeatedly should return elements in a FIFO order" in {
				val queue = emptyQueue.insert(11).insert(22).insert(33).insert(44)

				queue.head should equal(Some(11))
				queue.tail.head should equal(Some(22))
				queue.tail.tail.head should equal(Some(33))
				queue.tail.tail.tail.head should equal(Some(44))
				queue.tail.tail.tail.tail.isEmpty should equal(true)
			}

			"throw an exception on an empty list" in {
				try {
					emptyQueue.tail
					fail()
				}
				catch {
					case _: NoSuchElementException => // Expected behaviour
				}

			}
		}
	}
}