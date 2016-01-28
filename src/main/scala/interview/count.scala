package interview

object CountCharacters {
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

		val digit = "" :: "one" :: "two" :: "three" :: "four" :: "five" :: "six" :: "seven" :: "eight" :: "nine" :: "ten" :: "eleven" :: "twelve" :: "thirteen" :: "fourteen" :: "fifteen" :: "sixteen" :: "seventeen" :: "eighteen" :: "nineteen" :: Nil
		val tens = "" :: "ten" :: "twenty" :: "thirty" :: "fourty" :: "fifty" :: "sixty" :: "seventy" :: "eighty" :: "ninety" :: Nil
		val other = "hundred" :: "thousand" :: "hundred thousand" :: "million" :: "hundred million" :: "billion" :: Nil
		val str = i.toString

		def getFrom(idxFromRight: Int): String = {
			if (idxFromRight >= str.size) ""
			else idxFromRight match {
				case 1 => tens(str(str.length - 1 - idxFromRight).asDigit) + " " + digit(str.last.asDigit) + " "
				case 4 => tens(str(str.length - 1 - idxFromRight).asDigit) +  " "
				case 5 => digit(str(str.length - 1 - idxFromRight).asDigit)
				case _ => digit(str(str.length - 1 - idxFromRight).asDigit) + " " + other(idxFromRight - 2) + " "
			}
		}

		if (i == 0) "zero"
		else if (i < 20) digit(i)
		else {
			(1 to str.length-1).foldRight("") {
				(current, output) => output + getFrom(current)
			}.dropRight(1)
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