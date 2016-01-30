import interview.CountCharacters
import org.scalatest.BeforeAndAfterEach
import org.scalatest.Matchers
import org.scalatest.WordSpec


class counttest extends WordSpec with Matchers with BeforeAndAfterEach {

	"count" when {

		"calling toWords" should {

			"9 should equal nine" in {
				CountCharacters.toWords(9) should equal("nine")
			}

			"0 should equal zero" in {
				CountCharacters.toWords(0) should equal("zero")
			}

			"10 should equal ten" in {
				CountCharacters.toWords(10) should equal("ten")
			}

			"11 should equal eleven" in {
				CountCharacters.toWords(11) should equal("eleven")
			}

			"155 should equal one hundred fifty five" in {
				CountCharacters.toWords(155) should equal("one hundred fifty five")
			}

			"7777 should equal seven thousand seven hundred seventy seven" in {
				CountCharacters.toWords(7777) should equal("seven thousand seven hundred seventy seven")
			}

			"67673 should equal sixty seven thousand six hundred seventy three" in {
				CountCharacters.toWords(67673) should equal("sixty seven thousand six hundred seventy three")
			}

			"100000 should equal one hundred thousand" in {
				CountCharacters.toWords(100000) should equal("one hundred thousand")
			}

			"111111 should equal one hundred eleven thousand one hundred eleven" in {
				CountCharacters.toWords(111111) should equal("one hundred eleven thousand one hundred eleven")
			}

			"100050 should equal one hundred thousand and fifty" in {
				CountCharacters.toWords(100050) should equal("one hundred thousand fifty")
			}

			"1700000 should equal one million seven hundred thousand" in {
				CountCharacters.toWords(1700000) should equal("one million seven hundred thousand")
			}

			"10000000 should equal one hundred million" in {
				CountCharacters.toWords(100000000) should equal("one hundred million")
			}

			"100000000 should equal one billion" in {
				CountCharacters.toWords(1000000000) should equal("one billion")
			}

			"100100000 should equal one billion one million" in {
				CountCharacters.toWords(1001000000) should equal("one billion one million")
			}
		}
	}

	"calling countCharsInWordsOptimised" should {

		"9 should equal nine" in {
			CountCharacters.countCharsInWordsOptimised(9) should equal(4)
		}

		"0 should equal zero" in {
			CountCharacters.countCharsInWordsOptimised(0) should equal(4)
		}

		"10 should equal ten" in {
			CountCharacters.countCharsInWordsOptimised(10) should equal(3)
		}

		"11 should equal eleven" in {
			CountCharacters.countCharsInWordsOptimised(11) should equal(6)
		}

		"155 should equal one hundred fifty five" in {
			CountCharacters.countCharsInWordsOptimised(155) should equal(19)
		}

		"7777 should equal seven thousand seven hundred seventy seven" in {
			CountCharacters.countCharsInWordsOptimised(7777) should equal(37)
		}

		"67673 should equal sixty seven thousand six hundred seventy three" in {
			CountCharacters.countCharsInWordsOptimised(67673) should equal(40)
		}

		"100000 should equal one hundred thousand" in {
			CountCharacters.countCharsInWordsOptimised(100000) should equal(18)
		}

		"111111 should equal one hundred eleven thousand one hundred eleven" in {
			CountCharacters.countCharsInWordsOptimised(111111) should equal(40)
		}

		"100050 should equal one hundred thousand fifty" in {
			CountCharacters.countCharsInWordsOptimised(100050) should equal(23)
		}

		"1700000 should equal one million seven hundred thousand" in {
			CountCharacters.countCharsInWordsOptimised(1700000) should equal(30)
		}

		"10000000 should equal one hundred million" in {
			CountCharacters.countCharsInWordsOptimised(100000000) should equal(17)
		}

		"100000000 should equal one billion" in {
			CountCharacters.countCharsInWordsOptimised(1000000000) should equal(10)
		}

		"100100000 should equal one billion one million" in {
			CountCharacters.countCharsInWordsOptimised(1001000000) should equal(20)
		}

		"run faster than countCharsInWords" in {

			def time(block: => Unit): Long = {
				val t0 = System.nanoTime()
				val result = block
				val t1 = System.nanoTime()
				t1 - t0
			}

			val count = time{CountCharacters.countCharsInWords(1001000000)}
			val countOptimised = time{CountCharacters.countCharsInWords(1001000000)}

			countOptimised should be < count
		}
	}



}