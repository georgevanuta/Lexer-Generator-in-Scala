import scala.collection.immutable.HashMap

object Rules {
	type Token = String

	/*
			Maps a Regex to
		its corresponding Token.
	*/
	def specMap(spec: String): HashMap[String, Token] = {
		spec.split("\n")
		.foldRight(HashMap[String, String]())(
			(curr, acc) => {
				val LineSplit: Array[String] = curr.split(": ")

				var Regex: String = {
					StringContext.treatEscapes(
						LineSplit(1).reverse
						.tail
						.reverse
					)
				}

				if (Regex.head == '\"' && Regex.reverse.head == '\"') {
					Regex = Regex.tail.reverse.tail.reverse
				}

				val Token: Token = LineSplit(0)

				acc + (Regex -> Token)
			}
		)
	}


	/*
			Maps an NFA to
		its corresponding Token.
	*/
	def rulesNFAs(spec: String): HashMap[Nfa[Int], Token] = {
		val allSpecs: HashMap[String, String] = specMap(spec)

		allSpecs.foldRight(HashMap[Nfa[Int], String]())(
			(rule, acc) => {
				val Reg: String = rule._1
				val Token: Token = rule._2

				val Pren: String = Regex.toPrenex(Reg)
				val RuleNFA: Nfa[Int] = Nfa.fromPrenex(Pren)
				acc + (RuleNFA -> Token)
			}
		)
	}


	/*
			Maps a Token to its
		order (which is also its priority).
	*/
	def priorityMap(spec: String): HashMap[Token, Int] = {
		var priority: Int = 0

		spec.split("\n")
		.foldRight(HashMap[String, Int]())(
			(curr, acc) => {
				val Token: String = curr.split(": ")(0)
				priority += 1
				acc + (Token -> priority)
			}
		)
	}
}
