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
		""
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