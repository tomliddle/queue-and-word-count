import interview.CountCharacters
import org.scalatest.BeforeAndAfterEach
import org.scalatest.Matchers
import org.scalatest.WordSpec


class counttest extends WordSpec with Matchers with BeforeAndAfterEach{

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

			"100050 should equal one hundred thousand and fifty" in {
				CountCharacters.toWords(100050) should equal("one hundred thousand fifty")
			}

			"1700000 should equal one million seven hundred thousand" in {
				CountCharacters.toWords(1700000) should equal("one million seven hundred thousand")
			}

			"10000000 should equal one hundred million" in {
				CountCharacters.toWords(10000000) should equal("one hundred million")
			}

			"100000000 should equal one billion" in {
				CountCharacters.toWords(100000000) should equal("one billion")
			}

			"100100000 should equal one billion one million" in {
				CountCharacters.toWords(101000000) should equal("one billion one million")
			}



		}
	}
}