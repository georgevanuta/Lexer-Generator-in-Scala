import Rules.Token

import scala.collection.immutable.HashMap


object RulesMinimizer {
	def rulesMin(encRules: HashMap[Dfa[Int], Token]): HashMap[Dfa[Int], Token] = {
		encRules.map(
			x => {
				val d: Dfa[Int] = x._1
				val t: Token = x._2

				(Dfa.minDFA(d), t)
			}
		)
	}
}
