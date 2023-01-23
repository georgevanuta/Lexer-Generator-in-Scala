import Rules.Token

import scala.collection.immutable.HashMap

object RulesEncoder {

	def rulesEncoded(detRules: HashMap[Dfa[Set[Int]], Token]): HashMap[Dfa[Int], Token] = {
		detRules.map(
			x => {
				val d: Dfa[Set[Int]] = x._1
				val t: Token = x._2

				(Dfa.encodeFromSets(d), t)
			}
		)
	}

}
