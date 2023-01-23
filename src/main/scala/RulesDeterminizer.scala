import scala.collection.immutable.HashMap

import Rules.Token

object RulesDeterminizer {

	def rulesDFA(nondetRules: HashMap[Nfa[Int], Token]): HashMap[Dfa[Set[Int]], Token] = {
		nondetRules.map(
			x => {
				val n: Nfa[Int] = x._1
				val t: Token = x._2

				(Dfa.fromNFAtoDFA(n), t)
			}
		)
	}
}
