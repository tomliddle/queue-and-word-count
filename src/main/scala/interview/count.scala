package interview

object CountCharacters {

	// All the text in sequences
	private val digitStr = Seq("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
	private val tenStr = Seq("", "ten", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety")
	private val other = Seq("hundred", "", "thousand", "hundred thousand", "million", "hundred million", "billion")
	private val digitStrLen = digitStr.map(_.length)
	private val tenStrLen = digitStr.map(_.length)
	private val otherLen = digitStr.map(_.length)

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
		assert(i >= 0)

		// Split out each of the values. We are currently only looking at up to 999bn but it could be expanded
		val fillRightStr = f"$i%012d"
		val len = fillRightStr.length
		val tens = fillRightStr.takeRight(2) // 2 chars
		val hundreds = fillRightStr.charAt(len -3) // 1 char
		val thousands = fillRightStr.substring(len -6, len -3) // 3 chars
		val millions = fillRightStr.substring(len -9, len -6) // 3 chars
		val billions = fillRightStr.substring(len - 12, len - 9) // 3 chars


		def getDoubleDigitVals(str: String): Option[String] = {
			def precedingSpace(str: String): String = if (str.length == 0) "" else  " " + str

			if (str.toInt == 0) None
			else if (str(0) == '0') Some(digitStr(str(1).asDigit))
			else if (str(0) == '1') Some(digitStr(str.toInt))
			else Some(tenStr(str(0).asDigit) + precedingSpace(digitStr(str(1).asDigit)))
		}

		/**
			* Gets the hundred values - special case as the syntax doesn't allow for seventeen hundred etc.
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
			val teens = getDoubleDigitVals(str.tail)
			hundreds :: teens :: (if (hundreds.isDefined || teens.isDefined) Some(qualifier) :: Nil else Nil)
		}

		// Special case for zero. This simplifies the algorithm for using the Seq indexes so as not to print out a number of "zero"'s
		if (i == 0) "zero"
		else {
			val list = getShortScale(billions, "billion") ::: getShortScale(millions, "million") ::: getShortScale(thousands, "thousand") ::: List(getHundredVals(hundreds), getDoubleDigitVals(tens))
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