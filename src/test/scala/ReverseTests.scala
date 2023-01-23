class ReverseTests extends munit.FunSuite {
	test("rev concat") {
		val reg: String = "abc"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val r: Nfa[Int] = Dfa.reverseDFA(d)

		assert(r.accepts("cba"))
		assert(!r.accepts("abc"))
		assert(!r.accepts(""))
		assert(!r.accepts("cb"))
		assert(!r.accepts("cab"))
		assert(!r.accepts("ccc"))
		assert(!r.accepts("cbacba"))
	}

	test("Medium 1") {
		val reg: String = "a*b*|(ac*)"
		val prenex: String = Regex.toPrenex(reg)

		val d: Dfa[Int] = Dfa.fromPrenex(prenex)
		val r: Nfa[Int] = Dfa.reverseDFA(d)

		assert(r.accepts(""))
		assert(r.accepts("aaaaaaaaaaaaaaa"))
		assert(r.accepts("bbbbbbbbbbbbbbb"))
		assert(r.accepts("bbbbbbbbbbbbaaaaaaaaaaaaaa"))
		assert(!r.accepts("aaaaaaaaaaaaaabbbbbbb"))
		assert(r.accepts("ccccccccca"))
		assert(!r.accepts("ac"))
		assert(!r.accepts("cccccccaaaaaaaaa"))

	}
}
