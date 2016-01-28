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



		}
	}
}