

class MinDfaTests extends munit.FunSuite {
	test("single char") {
		val reg: String = "a"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val dMin: Dfa[Int] = Dfa.minDFA(d)

		assert(dMin.accepts("a"))
		assert(!dMin.accepts(""))
		assert(!dMin.accepts("aa"))
		assert(!dMin.accepts("aaaaaaa"))

		assert(dMin.getStates.size <= d.getStates.size)
	}

	test("multiple concat") {
		val reg: String = "abcdef"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val minD: Dfa[Int] = Dfa.minDFA(d)

		assert(minD.accepts("abcdef"))
		assert(!minD.accepts(""))
		assert(!minD.accepts("abc"))
		assert(!minD.accepts("def"))

		assert(minD.getStates.size <= d.getStates.size)
	}

	test("single union") {
		val reg: String = "a|b"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val minD: Dfa[Int] = Dfa.minDFA(d)

		assert(minD.accepts("a"))
		assert(minD.accepts("b"))
		assert(!minD.accepts(""))
		assert(!minD.accepts("ab"))
		assert(!minD.accepts("ba"))
		assert(!minD.accepts("aaaaaaaaaa"))
		assert(!minD.accepts("bbbbbbbbbb"))

		assert(minD.getStates.size < d.getStates.size)
	}

	test("multiple union") {
		val reg: String = "a|b|c|d|e|f"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val minD: Dfa[Int] = Dfa.minDFA(d)

		assert(minD.accepts("a"))
		assert(minD.accepts("b"))
		assert(minD.accepts("c"))
		assert(minD.accepts("d"))
		assert(minD.accepts("e"))
		assert(minD.accepts("f"))
		assert(!minD.accepts(""))
		assert(!minD.accepts("g"))
		assert(!minD.accepts("ab"))
		assert(!minD.accepts("abcdef"))

		assert(minD.getStates.size < d.getStates.size)
	}

	test("concat and union") {
		val reg: String = "(abc)|(abd)"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val minD: Dfa[Int] = Dfa.minDFA(d)

		assert(minD.accepts("abc"))
		assert(minD.accepts("abd"))
		assert(!minD.accepts("ab"))
		assert(!minD.accepts(""))
		assert(!minD.accepts("abcabd"))
		assert(!minD.accepts("aaa"))

		assert(minD.getStates.size <= d.getStates.size)
	}

	test("range") {
		val reg: String = "[0-9]"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val minD: Dfa[Int] = Dfa.minDFA(d)

		for (i <- (0 to 9)) {
			assert(minD.accepts(i.toString))
		}

		assert(!minD.accepts("10"))
		assert(!minD.accepts(""))
		assert(!minD.accepts("1a"))

		assert(minD.getStates.size <= d.getStates.size)
	}

	test("single star") {
		val reg: String = "a*"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val minD: Dfa[Int] = Dfa.minDFA(d)

		assert(minD.accepts(""))
		assert(minD.accepts("a"))
		assert(minD.accepts("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
		assert(!minD.accepts("b"))
		assert(!minD.accepts("aba"))
		assert(!minD.accepts("baaaaaaaaaaaaaaaa"))
		assert(!minD.accepts("aaaaaaaaaabaaaaaaaaaaa"))
		assert(!minD.accepts("aaaaaaaaaaaaaaaaaaaaab"))

		assert(minD.getStates.size <= d.getStates.size)
	}

	test("complex test 1") {
		val reg: String = "(a|b)+((ca)?d+)*"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val minD: Dfa[Int] = Dfa.minDFA(d)

		assert(minD.accepts("aaaaaaaababababababababababbbbbbabab"))
		assert(minD.accepts("acadddddcaddddd"))
		assert(!minD.accepts("ababababababababababababaaabaabbabddddddddddddddddddddca"))
		assert(minD.accepts("ababababababababababababaaabaabbabddddddddddddddddddddcadddddddd"))
		assert(!minD.accepts(""))

		assert(minD.getStates.size < d.getStates.size)
	}

	test("complex test 2") {
		val reg: String = "([0-9]*|b+)c?d(da)(\' \'|[A-Z]|\'a\')?"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val dMin: Dfa[Int] = Dfa.minDFA(d)

		assert(dMin.getStates.size < d.getStates.size)

		assert(dMin.accepts("bdda "))
		assert(dMin.accepts("28121274849cdda"))
		assert(dMin.accepts("dda"))
		assert(dMin.accepts("bbbbbbcddaa"))
		assert(dMin.accepts("bddaT"))
		assert(dMin.accepts("07cdda "))
		assert(!dMin.accepts("07bcdda "))
	}

}
