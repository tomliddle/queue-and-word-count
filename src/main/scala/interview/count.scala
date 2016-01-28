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
		val other = "hundred" :: "thousand" :: "thousand" :: "hundred thousand" :: "million" :: "hundred million" :: "billion" :: Nil
		val str = i.toString

		def getTens(idxFromRight: Int): String = {
			val idx = str(str.length - 1 - idxFromRight).asDigit
			tens(idx) + (if (idx > 0) " " else "")
		}

		def getDigit(idx: Int): String = {
			digit(idx) + (if (idx == 0) " " else "")
		}

		def getFrom(idxFromRight: Int): String = {
			if (idxFromRight >= str.length) ""
			else idxFromRight match {
				case 1 => getTens(idxFromRight) + getDigit(str.last.asDigit)
				case 4 => getTens(idxFromRight)
				case _ =>
					val currDigit = digit(str(str.length - 1 - idxFromRight).asDigit)
					val placeValue = if (currDigit != "" || str.length == idxFromRight+1) other(idxFromRight - 2)+ " " else ""
					currDigit + " " + placeValue
			}
		}

		if (i == 0) "zero"
		else if (i < 20) digit(i)
		else {
			(1 to str.length-1).foldRight("") {
				(placeValue, output) => output + getFrom(placeValue)
			}
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