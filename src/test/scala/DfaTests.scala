class DfaTests extends munit.FunSuite {

	test("Test eps (1p)") {
		assert(Dfa.fromPrenex("eps").accepts(""))
		assert(!Dfa.fromPrenex("eps").accepts(" "))
	}

	test("Test void (1p)") {
		val s = "void"
		assert(!Dfa.fromPrenex(s).accepts(""))
		assert(!Dfa.fromPrenex(s).accepts("a"))
		assert(!Dfa.fromPrenex(s).accepts("void"))
	}

	test("Test space (1p)") {
		val s = "' '"
		assert(Dfa.fromPrenex(s).accepts(" "))
		assert(!Dfa.fromPrenex(s).accepts(""))
	}

	test("Test character (1p)") {
		val s = "a"
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(!Dfa.fromPrenex(s).accepts("b"))
	}

	test("Test weird characters (1p)") {
		"$@*	({\"'\r\n".foreach(
			x => {
				assert(Dfa.fromPrenex(x + "").accepts(x + ""))
			}
		)
	}

	test("Test concat (1p)") {
		val s = "CONCAT a b"
		assert(Dfa.fromPrenex(s).accepts("ab"))
		assert(!Dfa.fromPrenex(s).accepts("a"))
	}

	test("Test star (1p)") {
		val s = "STAR a"
		assert(Dfa.fromPrenex(s).accepts(""))
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("aa"))
		assert(Dfa.fromPrenex(s).accepts("aaaaaaaaaaaaaaaaa"))
	}

	test("Test union (1p)") {
		val s = "UNION a b"
		assert(!Dfa.fromPrenex(s).accepts(""))
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("b"))
		assert(!Dfa.fromPrenex(s).accepts("ab"))
	}

	test("Test complex 1 (10p)") {
		val s = "STAR UNION a b"
		assert(Dfa.fromPrenex(s).accepts("aaababaaabaaaaa"))
		assert(Dfa.fromPrenex(s).accepts("aaaaaaaaaa"))
		assert(Dfa.fromPrenex(s).accepts("bbbbbbbbbbb"))
		assert(!Dfa.fromPrenex(s).accepts("baaabbbabaacabbbaaabbb"))
	}

	test("Test complex 2 (10p)") {
		val s = "STAR CONCAT a b"
		assert(Dfa.fromPrenex(s).accepts("ababababab"))
		assert(!Dfa.fromPrenex(s).accepts("abababababa"))
		assert(!Dfa.fromPrenex(s).accepts("abababaabab"))
	}

	test("Test complex 3 (10p)") {
		val s = "CONCAT UNION b STAR a STAR c"
		assert(Dfa.fromPrenex(s).accepts("aaaaaaaaaccccc"))
		assert(Dfa.fromPrenex(s).accepts("bccccccccc"))
		assert(!Dfa.fromPrenex(s).accepts("bbbbccccccccc"))

	}

	test("Test complex 4 (10p)") {
		val s = "CONCAT a STAR a"
		assert(Dfa.fromPrenex(s).accepts("aaa"))
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(!Dfa.fromPrenex(s).accepts(""))
	}
	test("Test complex 5 (10p)") {
		val s = "CONCAT STAR a STAR b"
		assert(Dfa.fromPrenex(s).accepts(""))
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("b"))
		assert(Dfa.fromPrenex(s).accepts("ab"))
		assert(Dfa.fromPrenex(s).accepts("aaaaaaa"))
		assert(Dfa.fromPrenex(s).accepts("bbbbb"))
		assert(Dfa.fromPrenex(s).accepts("aaabbbbb"))
		assert(!Dfa.fromPrenex(s).accepts("aaabbbbbab"))
	}

	test("Test complex 6 (10p)") {
		val s = "UNION STAR a STAR b"
		assert(Dfa.fromPrenex(s).accepts(""))
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("b"))
		assert(!Dfa.fromPrenex(s).accepts("ab"))
		assert(Dfa.fromPrenex(s).accepts("aaaaaaa"))
		assert(Dfa.fromPrenex(s).accepts("bbbbb"))
		assert(!Dfa.fromPrenex(s).accepts("aaabbbbbab"))
	}

	test("Test complex 7 (10p)") {
		val s = "CONCAT UNION a b UNION b a"
		assert(Dfa.fromPrenex(s).accepts("ab"))
		assert(Dfa.fromPrenex(s).accepts("aa"))
		assert(Dfa.fromPrenex(s).accepts("bb"))
		assert(Dfa.fromPrenex(s).accepts("ba"))
		assert(!Dfa.fromPrenex(s).accepts("a"))
		assert(!Dfa.fromPrenex(s).accepts("b"))
	}

	test("DFA map") {
		val regexes = List(
			("CONCAT UNION b STAR a STAR c", "abc"),
			("CONCAT a STAR a", "a"),
			("CONCAT a UNION b STAR CONCAT c d", "abcd")
		)

		assert(regexes.forall(p => { // run test for each tuple in "regexes"
			val regex = p._1
			val alphabet = p._2

			def f(x: Int): Int = x + 2

			val dfa = Dfa.fromPrenex(regex)
			val mapped_dfa = dfa.map(f)

			val states = dfa.getStates
			val mapped_states = mapped_dfa.getStates

			// check if the new set of states is the result of mapping f on the old set
			(states.map(f) == mapped_states) &&
			// check if the same applies to the set of final states
			(states.forall(s => dfa.isFinal(s) == mapped_dfa.isFinal(f(s)))) &&
			// check if f(old_delta(old_state,c)) = new_delta(new_state, c) for each state-character pair
			(
			alphabet.forall(c =>
				states.forall(s =>
					f(dfa.next(s, c)) == mapped_dfa.next(f(s), c)
				)
			)
			)
		}))
	}
}
