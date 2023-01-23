import Rules.Token

import scala.collection.immutable.HashMap

case class Lexer(spec: String) {
	private type ErrorMessage = String

	// token -> priority
	private val Priorities: HashMap[Token, Int] =
		Rules.priorityMap(spec)
	// dfa -> token
	private val LexerRules: HashMap[Dfa[Int], Token] =
		constructMinRules
	// regexes for tokens
	private val Regexes: Set[String] =
		Rules.specMap(spec).keySet

	private var lastIndexAccepted: Int = 0
	private var oldLastIndexAccepted = 0

	// min DFA -> token
	private def constructMinRules: HashMap[Dfa[Int], Token] = {
		val rNFA = Rules.rulesNFAs(spec)
		val rDFA = RulesDeterminizer.rulesDFA(rNFA)
		val rENC = RulesEncoder.rulesEncoded(rDFA)
		val rMIN = RulesMinimizer.rulesMin(rENC)

		rMIN
	}


	// Lexes a single word, either returns an error message
	// or a 3Tuple containing: the token, the word lexed, and the rest of the text.
	// Kinds works the same as a monadic parser.
	private def lexWord(word: String, fullWord: String): Either[ErrorMessage, (Token, String, String)] = {
		// Initially, all of the DFAs are working.
		var isWorking: HashMap[Dfa[Int], Boolean] =
			LexerRules.map(
				x => (x._1, true)
			)

		// Holds the state of each DFA.
		var currentStates: HashMap[Dfa[Int], Int] =
			LexerRules.map(
				x => (x._1, x._1.getStartState)
			)

		// Holds the last time a DFA accepted.
		// Also, if a DFA never accepted its
		// corresponding value is -1
		var whenDidMachineLastAccept: HashMap[Dfa[Int], Int] =
			LexerRules.map(
				x => (x._1, -1)
			)

		// Checks if no DFA is working.
		def areAllSunk: Boolean = !isWorking.exists(_._2)

		var isLast: Boolean = false

		var amountLexed: Int = 0

		// Returns the 3Tuple or an error message if no
		// DFA accepted.
		def bestTokenFound: Either[ErrorMessage, (Token, String, String)] = {
			var bestDFAs: List[Dfa[Int]] = List()

			var longestMatch: Int = -1

			// Get the longest match.
			for (tr <- whenDidMachineLastAccept) {
				val d: Dfa[Int] = tr._1
				val index: Int = tr._2

				if (index == longestMatch) {
					bestDFAs = d :: bestDFAs
				}
				else if (index > longestMatch) {
					longestMatch = index
					bestDFAs = List(d)
				}
			}


			// If none accepted, return the corresponding
			// error message
			if (bestDFAs.isEmpty || longestMatch == -1) {
				def findNrOfLinesAndCharsUntil(curr: Int): Int = {
					var nrLines: Int = 0
					for (i <- 0 until curr) {
						if (fullWord(i) == '\n') {
							nrLines += 1
						}
					}

					nrLines
				}

				if (isLast) {
					val lines: Int = findNrOfLinesAndCharsUntil(lastIndexAccepted)

					return Left(s"No viable alternative at character EOF, line ${lines}")
				}


				val lines: Int = findNrOfLinesAndCharsUntil(lastIndexAccepted)


				return Left(s"No viable alternative at character ${oldLastIndexAccepted + amountLexed}, line ${lines}")
			}

			var bestToken: Token = LexerRules.getOrElse(bestDFAs.head, "")
			var bestPriority: Int = Priorities.getOrElse(bestToken, -1)

			// Get the best token.
			for (d <- bestDFAs.tail) {
				val correspondingToken: Token =
					LexerRules.getOrElse(d, "")

				val correspondingPriority: Int =
					Priorities.getOrElse(correspondingToken, -1)

				if (correspondingPriority > bestPriority) {
					bestToken = correspondingToken
					bestPriority = correspondingPriority
				}
			}


			val RemainingWord: String = word.slice(longestMatch + 1, word.length)
			val LexedWord: String = word.slice(0, longestMatch + 1)

			Right((bestToken, RemainingWord, LexedWord))
		}

		// This is the part which lexes the biggest word with the biggest priority.
		for (i <- 0 until word.length) {
			amountLexed = i

			// skip new lines
			if (word(i) == '\n' && Regexes.exists(s => s.contains("\n")) || word(i) != '\n') {
				for (machine <- LexerRules.keys) {

					// only check DFAs that are still working
					if (isWorking.getOrElse(machine, false)) {
						val currState: Int = currentStates.getOrElse(machine, -1)
						val nextState: Int = machine.next(currState, word(i))

						if (machine.getSink == nextState) {
							isWorking =
								isWorking.updated(machine, false)
						}
						else if (machine.isFinal(nextState)) {
							whenDidMachineLastAccept =
								whenDidMachineLastAccept.updated(machine, i)
						}

						currentStates =
							currentStates.updated(machine, nextState)
					}
				}
			}

			if (areAllSunk) {
				return bestTokenFound
			} else lastIndexAccepted += 1

		}

		isLast = true

		bestTokenFound
	}



	/*
			This is the main function of the lexer, it splits
		the given word into a list of lexemes in the format (LEXEME, TOKEN).
	*/
  	def lex(word: String): Either[ErrorMessage, List[(String, Token)]] = {
		var restOfWord: String = word
		var lexemes: List[(String, Token)] = List()

		var lastIndexAccepted = 0

		while (restOfWord.nonEmpty) {
			val lexOneStep: Either[ErrorMessage, (Token, String, String)] =
				lexWord(restOfWord, word)

			lastIndexAccepted = oldLastIndexAccepted

			lexOneStep match {
				// propagate error and short circuit
				case Left(err) => return Left(err)
				case Right(res) => {
					val Token: Token = res._1
					val WordRemaining: String = res._2
					val WordLexed: String = res._3

					lastIndexAccepted += WordLexed.length
					// start from where the last lexed word finished
					oldLastIndexAccepted = lastIndexAccepted

					restOfWord = WordRemaining
					lexemes :+= (WordLexed, Token)
				}
			}
		}

		Right(lexemes)
	}
}

