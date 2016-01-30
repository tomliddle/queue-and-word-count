package interview

object CountCharacters {

	// All the text in sequences. The first element (zero) is left blank as we don't print these except for the value 0
	private val digitStr = Seq("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
	private val tenStr = Seq("", "ten", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety")

	private val digitStrLen = digitStr.map(_.length)
	private val tenStrLen = tenStr.map(_.length)

	/*
		We want to produce a function that counts the number of chars in an English language spelling of a number.
		The top-level function countCharsInWords is provided for you.

		Part 1) Implement the toWords function

		Returns i as spelled in english (without commas, "and"s etc)
		Assume US notation, ie billion = 10^9
		eg.
		 toWords(9) = "nine"
		 toWords(99) = "ninety nine"
		 toWords(999) = "nine hundred ninety nine"
	*/
	def toWords(i: Int): String = {
		// We currently don't deal with negative numbers
		assert(i >= 0)

		// Split out each of the values. We are currently only looking at up to 999bn but it could be expanded
		val str = f"$i%012d"
		val len = str.length
		val tens = str.takeRight(2) // 2 chars
		val hundreds = str.charAt(len -3) // 1 char
		val thousands = str.substring(len -6, len -3) // 3 chars
		val millions = str.substring(len -9, len -6) // 3 chars
		val billions = str.substring(len - 12, len - 9) // 3 chars


		/**
			* Gets text values from zero to ninety nine
			* @param str 00 to 99
			*/
		def getDoubleDigitVals(str: String): Option[String] = {
			def precedingSpace(str: String): String = if (str.length == 0) "" else  " " + str

			// For a zero we don't return any text
			if (str.toInt == 0) None
			// For a single digit number we return the text
			else if (str(0) == '0') Some(digitStr(str(1).asDigit))
			// Special case for a double digit number in the teens, we return numbers like sixteen etc
			else if (str(0) == '1') Some(digitStr(str.toInt))
			// Otherwise it is a number like seventy seven, which we join together as text from the relevant Seq
			else Some(tenStr(str(0).asDigit) + precedingSpace(digitStr(str(1).asDigit)))
		}

		/**
			* Gets the hundred values - special case as the expected syntax doesn't allow for seventeen hundred etc.
			* @param char 0 to 9
			*/
		def getHundredVals(char: Char): Option[String] = {
			if (char == '0') None
			else Some(digitStr(char.asDigit) + " hundred")
		}

		/**
			* Gets the short scale number naming e.g. five hundred (thousand|million|billion)
			* @param str must be exactly 3 chars
			* @param qualifier thousand | million | billion
			* @return a list of options of strings. None will be returned for values which we don't want displayed
			*/
		def getShortScale(str: String, qualifier: String): List[Option[String]] = {
			val hundreds = getHundredVals(str.head)
			val dd = getDoubleDigitVals(str.tail)
			// Here we add the three chars and if not zero's add the qualifier.
			// E.g. 700,000 would be seven hundred (with the qualifier) thousand
			// But 000,000 would not need the thousand qualifier
			hundreds :: dd :: (if (hundreds.isDefined || dd.isDefined) Some(qualifier) :: Nil else Nil)
		}

		// Special case for zero. This simplifies the algorithm for using the Seq indexes so as not to print out a number of "zero"'s
		if (i == 0) "zero"
		else {
			// Add the lists together. This way we don't need any complicated rules regarding spaces.
			val list = getShortScale(billions, "billion") ::: getShortScale(millions, "million") ::: getShortScale(thousands, "thousand") ::: List(getHundredVals(hundreds), getDoubleDigitVals(tens))
			// We just flatten the list to remove empty options and add a space in between
			list.flatten.mkString(" ")
		}
	}



	// countCharsInWords(9) == 4
	// countCharsInWords(99) == 10
	// countCharsInWords(999) == 21
	def countCharsInWords(i: Int): Int = toWords(i).filter(_ != ' ').length

	/*
		Part 2) Implement the countCharsInWordsOptimised function

		This should be a more efficient implementation of countCharsInWords.
		This does not need to re-use the above and may be an entirely different algorithm.
		Try to make this implementation as efficient as you can
	*/
	def countCharsInWordsOptimised(i: Int): Int = {
		1

	}
}