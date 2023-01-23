import scala.collection.mutable.Stack

object Regex {
	/* -------------- CONSTANTS -------------- */

	private final val Colon: Char = '\''

	private final val LeftRange: Char = '['
	private final val RightRange: Char = ']'
	private final val BoundDelimiter: Char = '-'

	private final val Union: Char = '|'
	private final val Concat: Char = '.'
	private final val Star: Char = '*'
	private final val Plus: Char = '+'
	private final val Maybe: Char = '?'
	private final val WhiteSpace: Char = ' '

	private final val UnaryOperations: List[Char] =
		List(Star, Plus, Maybe)

	private final val BinaryOperations: List[Char] =
		List(Union, Concat)

	private final val AllWhiteSpaces: List[Char] =
		List(' ', '\n', '\t')

	private final val OpenParenthesis: Char = '('
	private final val CloseParenthesis: Char = ')'

	private final val Epsilon: Char = 'e'
	private final val EpsilonChars: List[Char] =
		"eps".toList

	// -------------- END CONSTANTS --------------- //


	/*
	* 	Takes a processed expression
	* and puts it between parenthesis.
	*/
	private def encloseInParenthesis(sPreproc: List[Either[Char, Char]]): List[Either[Char, Char]] = {
		Right('(') :: (sPreproc :+ Right(')'))
	}


	/*
	* 	Used to transform the
	* range format into an easier
	* to process form.
	*/
	private def buildRange(leftBound: Char, rightBound: Char): List[Either[Char, Char]] = {
		/*
		* 	Adds an element
		* between each element
		* of the list
		*/
		def intersperse(l: List[Either[Char, Char]], x: Either[Char, Char]): List[Either[Char, Char]] = {
			val res = for (el <- l; in <- List(x, el)) yield in

			if (res.isEmpty) {
				res
			}
			else {
				res.tail
			}
		}

		val range: List[Either[Char, Char]] =
			Range.inclusive(leftBound, rightBound)
				 .map(x => Left(x.toChar))
				 .toList

		val rangePreprocessed: List[Either[Char, Char]] =
			intersperse(range, Right(Union))

		encloseInParenthesis(rangePreprocessed)
	}




	/*
	* 	Helper function used to
	* tell the difference between
	* operands and operations or
	* control characters. Also,
	* processes syntactic sugars
	* like ranges.
	* 	I classified operands
	* using the Left constructor,
	* and operators (and eps) by
	* using the Right one.
	* */
	private def preprocess(s: List[Char]): List[Either[Char, Char]] = {
		// the result is stored here
		var preprocessedString: List[Either[Char, Char]] = List()
		// escape controller
		var isEscaping: Boolean = false
		var hasEscaped: Boolean = false


		// range controller
		var isRange: Boolean = false
		var leftRangeBound: Option[Char] = None
		var rightRangeBound: Option[Char] = None

		// concat controller
		var shouldConcat: Boolean = false

		// epsilon controllers
		var parsedEpsilon: List[Char] = List()
		var concatEpsCount: Int = 0
		var shouldConcatEpsilon: Boolean = false

		def resetVarsEps(): Unit = {
			parsedEpsilon = List()
			concatEpsCount = 0
			shouldConcatEpsilon = false
		}



		// word controller
		var isWord: Boolean = false

		def closeWordIfPossible(): Unit = {
			if (isWord) {
				preprocessedString :+= Right(CloseParenthesis)
				isWord = false
			}
		}

		for (el <- s) {
			// start escaping
			if (el == Colon && !isEscaping) {
				isEscaping = true

				resetVarsEps()
			}
			// escape
			else if (isEscaping && !hasEscaped) {
				hasEscaped = true

				closeWordIfPossible()

				if (shouldConcat) {
					preprocessedString =
						preprocessedString :+ Right(Concat) :+ Left(el)
				}
				else {
					preprocessedString :+= Left(el)
				}

				shouldConcat = true

				resetVarsEps()
			}

			// stop escaping
			else if (el == Colon && isEscaping && hasEscaped) {
				isEscaping = false
				hasEscaped = false

				resetVarsEps()
			}
			// start parsing a range
			else if (el == LeftRange) {
				isRange = true

				resetVarsEps()
			}
			// skip bound delimiter '-'
			else if (el == BoundDelimiter && isRange) {
				resetVarsEps()
			}
			// parse left bound of the range
			else if (leftRangeBound.isEmpty && isRange) {
				leftRangeBound = Some(el)

				resetVarsEps()
			}
			// parse right bound of the range
			else if (rightRangeBound.isEmpty && isRange) {
				rightRangeBound = Some(el)

				closeWordIfPossible()

				// start building the range
				(leftRangeBound, rightRangeBound) match {
					case (Some(leftBound), Some(rightBound)) => {
						val rangePreprocessed: List[Either[Char, Char]] =
							buildRange(leftBound, rightBound)

						if (shouldConcat) {
							preprocessedString =
								(preprocessedString :+ Right(Concat)) ++ rangePreprocessed
						}
						else {
							preprocessedString ++= rangePreprocessed
						}

						shouldConcat = true

						resetVarsEps()
					}
					case _ => throw new IllegalArgumentException("[ERROR]: Bad range format.")
				}

			}
			// stop parsing a range
			else if (el == RightRange && isRange) {
				isRange = false
				leftRangeBound = None
				rightRangeBound = None

				resetVarsEps()
			}
			// parse an unary operation
			else if (UnaryOperations.contains(el)) {
				preprocessedString :+= Right(el)

				shouldConcat = true

				resetVarsEps()
			}
			// parse a binary operation
			else if (BinaryOperations.contains(el)) {
				closeWordIfPossible()

				preprocessedString :+= Right(el)

				shouldConcat = false

				resetVarsEps()
			}
			// parse an opened parenthesis
			else if (el == OpenParenthesis) {
				closeWordIfPossible()

				if (shouldConcat) {
					preprocessedString =
						preprocessedString :+ Right(Concat) :+ Right(OpenParenthesis)
				}
				else {
					preprocessedString =
						preprocessedString :+ Right(OpenParenthesis)
				}

				shouldConcat = false

				resetVarsEps()
			}
			// parse a closed parenthesis
			else if (el == CloseParenthesis) {
				closeWordIfPossible()

				preprocessedString =
					preprocessedString :+ Right(CloseParenthesis)

				shouldConcat = true

				resetVarsEps()
			}
			// parse the whole epsilon
			else if (el == EpsilonChars(parsedEpsilon.size) && parsedEpsilon.size == 2) {

				preprocessedString =
					preprocessedString.take(preprocessedString.size - 2 - concatEpsCount)

				if (shouldConcatEpsilon) {
					preprocessedString =
						preprocessedString :+ Right(Concat) :+ Right(Epsilon)
				} else {
					preprocessedString :+= Right(Epsilon)
				}

				shouldConcat = true

				resetVarsEps()
			}
			// skip white space
			else if (el == WhiteSpace) {
				closeWordIfPossible()

				resetVarsEps()
			}
			// parse a character of epsilon
			else if (el == EpsilonChars(parsedEpsilon.size)) {
				parsedEpsilon :+= el

				if (shouldConcat) {
					concatEpsCount += 1

					preprocessedString =
						preprocessedString :+ Right(Concat) :+ Left(el)
				}
				else {
					preprocessedString :+= Left(el)
				}

				shouldConcat = true
			}
			// parse a normal character
			else {
				if (shouldConcat) {
					if (!isWord) {
						preprocessedString =
							preprocessedString :+ Right(OpenParenthesis) :+ Right(Concat) :+ Left(el)

						isWord = true
					}
					else {
						preprocessedString =
							preprocessedString :+ Right(Concat) :+ Left(el)
					}
				}
				else {
					if (!isWord) {
						preprocessedString =
							preprocessedString :+ Right(OpenParenthesis) :+ Left(el)

						isWord = true
					}
					else {
						preprocessedString :+= Left(el)
					}
				}

				shouldConcat = true

				resetVarsEps()
			}
		}

		closeWordIfPossible()

		encloseInParenthesis(preprocessedString)
	}


	// Converts a postnex to a prenex
	private def fromPostfixToPrefix(postnex: String): String = {
		if (postnex.trim.size == 1) return postnex.head.toString
		// here we keep the expressions parsed
		val exprStack: Stack[String] = Stack()

		// same regex as at the Nfa class
		val postnexElements: List[String] =
			"\\w+|\'[\\w\\s]*\'|-|=|[*]|[+]|[?]|[>]|[<]|[(]|[)]".r
			.findAllIn(postnex)
			.toList


		val binaryOpsPrenex: List[String] =
			List("CONCAT", "UNION")

		val unaryOpsPrenex: List[String] =
			List("STAR", "MAYBE", "PLUS")

		for (el <- postnexElements) {
			/*
			* 	If we find a binary operation,
			* we must switch the expressions. This is
			* only important for the CONCAT method.
			*/
			if (binaryOpsPrenex.contains(el)) {
				val exp1: String = exprStack.pop()
				val exp2: String = exprStack.pop()


				val newExpr: String = s"${el} ${exp2} ${exp1}"

				exprStack.push(newExpr)
			}
			/*
			* 	Construct a new simple
			* expression from an unary operation.
			* */
			else if (unaryOpsPrenex.contains(el)) {
				val exp: String = exprStack.pop()

				val newExpr: String = s"${el} ${exp}"

				exprStack.push(newExpr)
			}
			/*
			* 	Found a character,
			* just push it to the stack.
			*/
			else {
				exprStack.push(el)
			}
		}


		// build the prenex
		var prenex: String = ""

		while (exprStack.nonEmpty) {
			prenex =
				prenex ++ exprStack.pop() :+ WhiteSpace
		}

		prenex
	}


	// Returns the priority of an operation
	private def priorityOfElement(el: Char): Int = {
		if (UnaryOperations.contains(el)) 2
		else if (BinaryOperations.contains(el)) 1
		else 0
	}


	// Translates syntactic sugar
	private def translateOperation(op: Char): String = {
		op match {
			case Union => "UNION"
			case Concat => "CONCAT"
			case Star => "STAR"
			case Maybe => "MAYBE"
			case Plus => "PLUS"
			case Epsilon => "eps"
			case _ => throw new IllegalArgumentException(s"[ERROR]: Operation ${op} is unknown.")
		}
	}


	/*
	* 	Given a Regex (infix notation)
	* returns a Prenex (prefix notation).
	* 	This is a modified version of the
	* Shunting Yard Algorithm made by Dijkstra.
	* */
	def toPrenex(str: String): String = {
		var prenexRes: String = ""
		val opStack: Stack[Char] = Stack()


		val processedRegex: List[Either[Char, Char]] =
			preprocess(str.toList)


		def quoteChar(c: Char): String = s"\'$c\'"

		for (el <- processedRegex) {
			el match {
				// found a character
				case Left(ch) => {
					/*
					*	We need to put white spaces into
					* quotes because this is the way an Nfa
					* recognizes that it's an actual character,
					* not just a control symbol.
					*/
					if (AllWhiteSpaces.contains(ch)) {
						prenexRes =
							prenexRes ++ quoteChar(ch) :+ WhiteSpace
					}
					// simple character, just add it
					else {
						prenexRes =
							prenexRes :+ ch :+ WhiteSpace
					}
				}

				// found an operation / epsilon / parenthesis
				case Right(op) => {
					/*
					* 	Treating the parenthesis.
					* We need to to pop everything
					* inside them.
					* */
					if (op == OpenParenthesis) {
						opStack.push(OpenParenthesis)
					}
					else if (op == CloseParenthesis) {
						while (opStack.nonEmpty && opStack.top != OpenParenthesis) {
							val opPopped: Char = opStack.pop()
							prenexRes =
								prenexRes ++ translateOperation(opPopped) :+ WhiteSpace
						}

						opStack.pop()
					}
					// treat epsilon as a simple character
					else if (op == Epsilon) {
						prenexRes =
							prenexRes ++ translateOperation(op) :+ WhiteSpace
					}
					/*
					* 	Treat unary operations by
					* just adding them, translated to
					* the prefix "language".
					*/
					else if (UnaryOperations.contains(op)) {
						prenexRes =
							prenexRes ++ translateOperation(op) :+ WhiteSpace
					}
					else {
						while (opStack.nonEmpty
						       && priorityOfElement(opStack.top) >= priorityOfElement(op)) {

							val opPopped: Char = opStack.pop()
							prenexRes =
								prenexRes ++ translateOperation(opPopped) :+ WhiteSpace
						}

						opStack.push(op)
					}
				}
			}
		}

		while (opStack.nonEmpty) {
			// check for valid parenthesis
			if (opStack.top == OpenParenthesis)
				throw new IllegalArgumentException("[ERROR]: Invalid parenthesization of given Regex.")

			val opPopped: Char = opStack.pop()
			prenexRes =
				prenexRes ++ translateOperation(opPopped) :+ WhiteSpace
		}

		fromPostfixToPrefix(prenexRes)
	}
}
