package interview

object CountCharacters {

	// All the text in sequences. The first element (zero) is left blank as we don't print these except for the value 0
	private val digitStr =
		Map (	"digit" -> Seq("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"),
					"tens" -> Seq("", "ten", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"),
					"qualifier" -> Seq("hundred", "thousand", "million", "billion"))

	// Calculate each individual string length in the same format as digitStr
	private val digitSeq = digitStr.map{x => x._1 -> x._2.map(word => (word, word.length))}

	/**
		* Gets text values from zero to ninety nine
		* @param d1 0 to 9
		* @param d2 0 to 9
		*/
	private def getDoubleDigitVals(d1: Int, d2: Int): List[(String, Int)] = {
		def precedingSpace(str: String): String = if (str.length == 0) "" else  " " + str

		// For a zero we don't return any text
		if (d1 + d2 == 0) List()
		// For a single digit number we return the text
		else if (d1 == 0) List(digitSeq("digit")(d2))
		// Special case for a double digit number in the teens, we return numbers like sixteen etc
		else if (d1 == 1) List(digitSeq("digit")(d1 * 10 + d2))
		// Otherwise it is a number like seventy seven, which we join together as text from the relevant Seq
		else if (d2 == 0) List(digitSeq("tens")(d1))
		else List(digitSeq("tens")(d1), digitSeq("digit")(d2))
	}

	/**
		* Gets the hundred values - special case as the expected syntax doesn't allow for seventeen hundred etc.
		* @param d1 0 to 9
		*/
	private def getHundredVals(d1: Int): List[(String, Int)] = {
		if (d1 == 0) List()
		else List(digitSeq("digit")(d1), digitSeq("qualifier").head)
	}

	/**
		* Gets the short scale number naming e.g. five hundred (thousand|million|billion)
		* @param d1 digit 1,
		* @param d2 digit 2
		* @param d3 digit 3
		* @param qualifier thousand | million | billion
		* @return a list of options of strings. None will be returned for values which we don't want displayed
		*/
	private def getShortScale(d1: Int, d2: Int, d3: Int, qualifier: (String, Int)): List[(String, Int)] = {
		val hundreds = getHundredVals(d1)
		val dd = getDoubleDigitVals(d2, d3)
		// Here we add the three chars and if not zero's add the qualifier.
		// E.g. 700,000 would be seven hundred (with the qualifier) thousand
		// But 000,000 would not need the thousand qualifier
		hundreds ::: dd ::: (if (hundreds.nonEmpty || dd.nonEmpty) qualifier :: Nil else Nil)
	}

	private def getToWords(i: Int): List[(String, Int)] = {
		// We currently don't deal with negative numbers
		assert(i >= 0)

		// Pad the string to the left so we don't have to worry about string length.
		val str = f"$i%012d"
		def s(idxFromRight: Int): Int = str.charAt(str.length - 1 - idxFromRight).asDigit

		// Special case for zero. This simplifies the algorithm for using the Seq indexes so as not to print out a number of "zero"'s
		if (i == 0) List(digitSeq("digit").head)
		else {
			// We are currently only looking at up to 999bn but it could be expanded
			// Add the lists together. This way we don't need any complicated rules regarding spaces.
			(getShortScale(s(11), s(10), s(9), digitSeq("qualifier")(3))
					++ getShortScale(s(8), s(7), s(6), digitSeq("qualifier")(2))
					++ getShortScale(s(5), s(4), s(3), digitSeq("qualifier")(1))
					++ getHundredVals(s(2))
					++ getDoubleDigitVals(s(1), s(0)))
		}
	}

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
		getToWords(i).foldLeft(new StringBuilder) {
			(acc, curr) => acc.append(curr._1).append(" ")
		}.toString.dropRight(1)
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
		getToWords(i).foldLeft(0) {
			(acc, curr) => acc + curr._2
		}
	}
}