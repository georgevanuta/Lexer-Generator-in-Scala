class NfaTests extends munit.FunSuite {

	test("Nfa from Eps") {
		//println(Nfa.fromPrenex("eps"))
		//println(Nfa.fromPrenex("eps").accepts(""))
		assert(Nfa.fromPrenex("eps").accepts(""))
	}

	test("Nfa from Void") {
		//print(Nfa.fromPrenex("void"))
		assert(!Nfa.fromPrenex("void").accepts(""))
	}

	test("Nfa from char") {
		//print(Nfa.fromPrenex("a"))
		assert(Nfa.fromPrenex("a").accepts("a"))
	}

	test("Nfa from complex expression 1") {
		val s = "CONCAT a b"
		//print(Nfa.fromPrenex(s))
		assert(Nfa.fromPrenex(s).accepts("ab"))
	}

	test("Nfa from complex expression 1 with bad input") {
		val s = "CONCAT a b"
		//print(Nfa.fromPrenex(s))
		assert(!Nfa.fromPrenex(s).accepts("aba"))
	}

	test("Nfa from complex expression 2") {
		val s = "UNION a b"
		//print(Nfa.fromPrenex(s))
		assert(Nfa.fromPrenex(s).accepts("a"))
		assert(Nfa.fromPrenex(s).accepts("b"))
	}

	test("Nfa from complex expression 2 with bad input") {
		val s = "UNION a b"
		//print(Nfa.fromPrenex(s))
		//print(Nfa.fromPrenex(s).accepts("ab"))
		assert(!Nfa.fromPrenex(s).accepts("ab"))
		assert(!Nfa.fromPrenex(s).accepts("ba"))
	}

	test("Nfa from star expression 1") {
		val s = "STAR a"
		//print(Nfa.fromPrenex(s))
		//print(Nfa.fromPrenex(s).accepts("ab"))
		assert(Nfa.fromPrenex(s).accepts(""))
		assert(Nfa.fromPrenex(s).accepts("a"))
		assert(Nfa.fromPrenex(s).accepts("aaaaaaaaaaaaa"))

	}

	test("Nfa from star expression with bad string") {
		val s = "STAR a"
		//print(Nfa.fromPrenex(s))
		assert(!Nfa.fromPrenex(s).accepts("aaaabaaaaaaaa"))

	}

	test("Nfa from complex expression 3") {
		val s = "STAR UNION a b"
		//print(Nfa.fromPrenex(s))
		assert(Nfa.fromPrenex(s).accepts("aaababaaabaaaaa"))
	}

	test("Nfa from complex expression 4") {
		val s = "STAR CONCAT a b"
		//print(Nfa.fromPrenex(s))
		assert(Nfa.fromPrenex(s).accepts("ababababab"))
	}

	test("Nfa from complex expression 5 and bad input") {
		val s = "STAR CONCAT a b"
		//print(Nfa.fromPrenex(s))
		assert(!Nfa.fromPrenex(s).accepts("abababababa"))
	}

	test("Nfa from complex expression 5") {
		val s = "CONCAT STAR a STAR b"
		//print(Nfa.fromPrenex(s))
		assert(Nfa.fromPrenex(s).accepts("aaaaaaaaabbbbbb"))
	}

	test("Nfa from complex expression 6") {
		val s = "CONCAT UNION b STAR a STAR c"
		//print(Nfa.fromPrenex(s))
		assert(Nfa.fromPrenex(s).accepts("aaaaaaaaaccccc"))
		assert(Nfa.fromPrenex(s).accepts("bccccccccc"))

	}

	test("Nfa from complex expression 7") {
		val s = "CONCAT a STAR a"
		// print(Nfa.fromPrenex(s))
		assert(Nfa.fromPrenex(s).accepts("aaa"))
		assert(!Nfa.fromPrenex(s).accepts(""))

	}

	test("Nfa map") {
		// this is a class used just for this test
		// its get_mapping method recieves a 'state' and returns a unique id for it. if it reiceves the same state multiple time it returns the same id
		class TestIterator[A](start: Int) {
			var counter = start;
			var mapping = Map[A, Int]();

			def get_mapping(x: A): Int = {
				if (!mapping.contains(x)) {
					mapping += (x -> counter);
					counter += 1;
				}
				mapping(x)
			}
		}

		val it = new TestIterator[Int](5); // a new test_it which gives id's starting with '5'

		// a list of regexes, their alphabets and mapping functions to test the 'map' method on
		val regexes = List[(String, String, Int => Any)](
			("CONCAT UNION b STAR a STAR c", "abc", _ + 2),
			("CONCAT a STAR a", "a", _.toString()),
			("CONCAT a UNION b STAR CONCAT c d", "abcd", it.get_mapping)
		)

		assert(regexes.forall((p: (String, String, Int => Any)) => { // run test for each tuple in "regexes"

			val regex = p._1;
			val alphabet = p._2;
			val f = p._3;

			val nfa = Nfa.fromPrenex(regex);
			val mapped_nfa = nfa.map(f);

			val states = nfa.getStates;
			val mapped_states = mapped_nfa.getStates;
			// check if the new set of states is the result of mapping f on the old set
			(states.map(f) == mapped_states) &&
			// check if the same applies to the set of final states
			(states.forall(s => nfa.isFinal(s) == mapped_nfa.isFinal(f(s)))) &&
			// check if f(old_delta(old_state,c)) = new_delta(new_state, c) for each state-character pair
			// (epsilon-tranistions are not checked)
			(
			alphabet.forall(c =>
				states.forall(s =>
					nfa.next(s, c).map(f) == mapped_nfa.next(f(s), c)
				)
			)
			)
		})
		)
	}


}

