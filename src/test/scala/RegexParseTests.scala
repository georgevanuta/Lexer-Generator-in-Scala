class RegexParseTests extends munit.FunSuite {

	test("single char (1p)") {
		//assert(Regex.toPrenex("a") == "a")
		val s = Regex.toPrenex("a")
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(!Dfa.fromPrenex(s).accepts(""))
		assert(!Dfa.fromPrenex(s).accepts("aa"))
		assert(!Dfa.fromPrenex(s).accepts("b"))
	}

	test("single concat (1p)") {
		//assert(Regex.toPrenex("aa") == "CONCAT a a")
		val s = Regex.toPrenex("aa")
		assert(Dfa.fromPrenex(s).accepts("aa"))
		assert(!Dfa.fromPrenex(s).accepts("a"))
		assert(!Dfa.fromPrenex(s).accepts("aaa"))
		assert(!Dfa.fromPrenex(s).accepts("b"))
	}

	test("single union (1p)") {
		val str = "a|b"
		//assert(Regex.toPrenex(str) == "UNION a b")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("b"))
		assert(!Dfa.fromPrenex(s).accepts("aa"))
		assert(!Dfa.fromPrenex(s).accepts("ab"))
		assert(!Dfa.fromPrenex(s).accepts("ba"))
	}

	test("single star (1p)") {
		val str = "a*"
		//assert(Regex.toPrenex(str) == "STAR a")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("aa"))
		assert(Dfa.fromPrenex(s).accepts("aaaaaaaaa"))
		assert(Dfa.fromPrenex(s).accepts(""))
	}

	test("union concat 1 (2p)") {
		val str = "ab|c"
		//assert(Regex.toPrenex(str) == "UNION CONCAT a b c")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("c"))
		assert(Dfa.fromPrenex(s).accepts("ab"))
		assert(!Dfa.fromPrenex(s).accepts("ac"))
		assert(!Dfa.fromPrenex(s).accepts("abc"))
	}

	test("union concat 2 (2p)") {
		val str = "a|bc"
		//assert(Regex.toPrenex(str) == "UNION a CONCAT b c")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("bc"))
		assert(!Dfa.fromPrenex(s).accepts("ac"))
	}

	test("multiple union (3p)") {
		val str = "a|b|c|d"
		//assert(Regex.toPrenex(str) == "UNION UNION UNION a b c d")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("b"))
		assert(Dfa.fromPrenex(s).accepts("c"))
		assert(Dfa.fromPrenex(s).accepts("d"))
		assert(!Dfa.fromPrenex(s).accepts("ab"))
	}

	test("union concat with par 1 (3p)") {
		val str = "(a|b)c"
		//assert(Regex.toPrenex(str) == "CONCAT UNION a b c")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("ac"))
		assert(Dfa.fromPrenex(s).accepts("bc"))
		assert(!Dfa.fromPrenex(s).accepts("c"))
		assert(!Dfa.fromPrenex(s).accepts("a"))
		assert(!Dfa.fromPrenex(s).accepts("abc"))
	}

	test("union concat with par 2 (3p)") {
		val str = "a(b|c)"
		//assert(Regex.toPrenex(str) == "CONCAT a UNION b c")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("ab"))
		assert(Dfa.fromPrenex(s).accepts("ac"))
		assert(!Dfa.fromPrenex(s).accepts("c"))
		assert(!Dfa.fromPrenex(s).accepts("a"))
		assert(!Dfa.fromPrenex(s).accepts("abc"))
	}

	test("union star (2p)") {
		val str = "a|b*"
		//assert(Regex.toPrenex(str) == "UNION a STAR b")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("b"))
		assert(Dfa.fromPrenex(s).accepts("bbbbbbbbbb"))
		assert(Dfa.fromPrenex(s).accepts(""))
		assert(!Dfa.fromPrenex(s).accepts("ab"))
		assert(!Dfa.fromPrenex(s).accepts("ba"))
	}

	test("concat star (3p)") {
		val str = "ab*cd*"
		// a b STAR CONCAT c CONCAT d STAR CONCAT

		//assert(Regex.toPrenex(str) == "CONCAT a CONCAT STAR b CONCAT c STAR d")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("ac"))
		assert(Dfa.fromPrenex(s).accepts("acd"))
		assert(Dfa.fromPrenex(s).accepts("abc"))
		assert(Dfa.fromPrenex(s).accepts("abcd"))
		assert(Dfa.fromPrenex(s).accepts("abbbbbbbbcdddd"))
		assert(Dfa.fromPrenex(s).accepts("abbbbbbbbc"))
		assert(Dfa.fromPrenex(s).accepts("acddd"))
	}

	test("complex union concat (6p)") {
		val str = "a|(b|(c|de))"
		//assert(Regex.toPrenex(str) == "UNION a UNION b UNION c CONCAT d e")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("b"))
		assert(Dfa.fromPrenex(s).accepts("c"))
		assert(Dfa.fromPrenex(s).accepts("de"))
		assert(!Dfa.fromPrenex(s).accepts("ab"))
		assert(!Dfa.fromPrenex(s).accepts("abc"))
		assert(!Dfa.fromPrenex(s).accepts("abde"))
	}

	test("all basic 1 (6p)") {
		val str = "a(b|c)*"
		//assert(Regex.toPrenex(str) == "CONCAT a STAR UNION b c")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("abbbbbb"))
		assert(Dfa.fromPrenex(s).accepts("accccccccc"))
		assert(Dfa.fromPrenex(s).accepts("abccbbbbcbcbcbcb"))
		assert(Dfa.fromPrenex(s).accepts("acccbbcbcbbbc"))
	}

	test("all basic 2 (6p)") {
		val str = "(a|b)(c|d)*"
		//assert(Regex.toPrenex(str) == "CONCAT UNION a b STAR UNION c d")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("b"))
		assert(Dfa.fromPrenex(s).accepts("acdcdcdddc"))
		assert(Dfa.fromPrenex(s).accepts("bddddcddcddd"))
	}

	test("all basic 3 (6p)") {
		val str = "a*|b|c"
		//assert(Regex.toPrenex(str) == "UNION UNION STAR a b c")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts(""))
		assert(Dfa.fromPrenex(s).accepts("b"))
		assert(Dfa.fromPrenex(s).accepts("c"))
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("aaaaaa"))
	}

	test("all basic 4 (6p)") {
		val str = "a*|bc|d"
		//assert(Regex.toPrenex(str) == "UNION UNION STAR a CONCAT b c d")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts(""))
		assert(Dfa.fromPrenex(s).accepts("bc"))
		assert(Dfa.fromPrenex(s).accepts("d"))
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("aaaaaaaaaaaa"))
	}

	test("all basic 5 (6p)") {
		val str = "a*(b|c)d"
		//assert(Regex.toPrenex(str) == "CONCAT STAR a CONCAT UNION b c d")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("bd"))
		assert(Dfa.fromPrenex(s).accepts("cd"))
		assert(Dfa.fromPrenex(s).accepts("aaaaaacd"))
		assert(Dfa.fromPrenex(s).accepts("aaaaaaaaaaaabd"))
	}

	test("all basic 6 (6p)") {
		val str = "(ab(b|c)*)*"
		//assert(Regex.toPrenex(str) == "STAR CONCAT CONCAT a b STAR UNION b c")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts(""))
		assert(Dfa.fromPrenex(s).accepts("ab"))
		assert(Dfa.fromPrenex(s).accepts("ababab"))
		assert(Dfa.fromPrenex(s).accepts("abbababcabcababb"))
	}

	test("all basic 7 (6p)") {
		val str = "a(a|A)*((bcd*|ecf*)|aA)*"
		//assert(Regex.toPrenex(str) == "CONCAT CONCAT a STAR UNION a A STAR UNION CONCAT b CONCAT c UNION STAR d CONCAT d CONCAT c STAR b CONCAT a A")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("aAaAaaAAAAAAa"))
		assert(Dfa.fromPrenex(s).accepts("aAaAaAbcddddbcecffffaAaA"))
	}

	// 70%
	test("eps (1p)") {
		//assert(Regex.toPrenex("eps") == "eps")
		val str = Regex.toPrenex("eps")
		assert(Dfa.fromPrenex(str).accepts(""))
		assert(!Dfa.fromPrenex(str).accepts(" "))
	}

	test("escaped chars 1 (1p)") {
		val str = "\' \'\'a\'"
		//assert(Regex.toPrenex(str) == "CONCAT @ a")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts(" a"))
		assert(!Dfa.fromPrenex(s).accepts("@a"))
	}

	test("escaped chars 2 (1p)") {
		val str = "\'\n\'a\'\t\'b"
		//assert(Regex.toPrenex(str) == "CONCAT \n CONCAT a CONCAT \t b")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("\na\tb"))
	}

	test("0 to 9 (2p)") {
		val str = "[0-9]"
		//assert(Regex.toPrenex(str) == "UNION UNION UNION UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6 7 8 9")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("0"))
		assert(Dfa.fromPrenex(s).accepts("7"))
		assert(Dfa.fromPrenex(s).accepts("2"))
	}

	test("a to z (2p)") {
		val str = "[a-z]"
		//assert(Regex.toPrenex(str) == "UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION a b c d e f g h i j k l m n o p r s t u v w x y z")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("a"))
		assert(Dfa.fromPrenex(s).accepts("l"))
		assert(Dfa.fromPrenex(s).accepts("z"))
	}

	test("A to Z (2p)") {
		val str = "[A-Z]"
		//assert(Regex.toPrenex(str) == "UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION A B C D E F G H I J K L M N O P R S T U V W X Y Z")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("K"))
		assert(Dfa.fromPrenex(s).accepts("T"))
		assert(Dfa.fromPrenex(s).accepts("U"))
	}

	test("plus (1p)") {
		val str = "0+"
		//assert(Regex.toPrenex(str) == "CONCAT 0 STAR 0")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("0"))
		assert(Dfa.fromPrenex(s).accepts("0000000"))
		assert(!Dfa.fromPrenex(s).accepts(""))
	}

	test("question mark (1p)") {
		val str = "0?"
		//assert(Regex.toPrenex(str) == "UNION 0 eps")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts(""))
		assert(Dfa.fromPrenex(s).accepts("0"))
		assert(!Dfa.fromPrenex(s).accepts("00"))
	}

	test("q and p (1p)") {
		val str = "0?1+"
		//assert(Regex.toPrenex(str) == "CONCAT UNION 0 eps CONCAT 1 STAR 1")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("11111"))
		assert(Dfa.fromPrenex(s).accepts("011111111"))
		assert(!Dfa.fromPrenex(s).accepts("0"))
		assert(!Dfa.fromPrenex(s).accepts("001111"))
		assert(!Dfa.fromPrenex(s).accepts(""))
	}

	test("0 to 9 star (3p)") {
		val str = "[0-9]*|b"
		//assert(Regex.toPrenex(str) == "UNION STAR UNION UNION UNION UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6 7 8 9 b")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("012377777"))
		assert(Dfa.fromPrenex(s).accepts("98555561312"))
		assert(Dfa.fromPrenex(s).accepts("b"))
	}

	test("squared ops (3p)") {
		val str = "a([a-z]*|[A-Z]*)z"
		//assert(Regex.toPrenex(str) == "CONCAT a CONCAT UNION STAR UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION a b c d e f g h i j k l m n o p r s t u v w x y z STAR UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION A B C D E F G H I J K L M N O P R S T U V W X Y Z z")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("az"))
		assert(Dfa.fromPrenex(s).accepts("acetareetemaz"))
		assert(Dfa.fromPrenex(s).accepts("aFASTz"))
	}

	test("complex (6p)") {
		val str = "[0-9]+(\'-\'[0-9]+)*"
		//assert(Regex.toPrenex(str) == "CONCAT CONCAT UNION UNION UNION UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6 7 8 9 STAR UNION UNION UNION UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6 7 8 9 STAR CONCAT - CONCAT UNION UNION UNION UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6 7 8 9 STAR UNION UNION UNION UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6 7 8 9")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("0"))
		assert(Dfa.fromPrenex(s).accepts("1231212"))
		assert(Dfa.fromPrenex(s).accepts("777-333"))
		assert(Dfa.fromPrenex(s).accepts("7-3-5-6-7-8"))
		assert(Dfa.fromPrenex(s).accepts("11-23-94-312-413231"))
	}

	test("all (6p)") {
		val str = "([0-9]*|b+)c?d(da)(\' \'|[A-Z]|\'a\')?"
		//assert(Regex.toPrenex(str) == "CONCAT CONCAT CONCAT UNION STAR UNION UNION UNION UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6 7 8 9 CONCAT b STAR b UNION c eps d CONCAT CONCAT d a UNION UNION @ UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION UNION A B C D E F G H I J K L M N O P R S T U V W X Y Z a eps")
		val s = Regex.toPrenex(str)
		assert(Dfa.fromPrenex(s).accepts("bdda "))
		assert(Dfa.fromPrenex(s).accepts("28121274849cdda"))
		assert(Dfa.fromPrenex(s).accepts("dda"))
		assert(Dfa.fromPrenex(s).accepts("bbbbbbcddaa"))
		assert(Dfa.fromPrenex(s).accepts("bddaT"))
		assert(Dfa.fromPrenex(s).accepts("07cdda "))
		assert(!Dfa.fromPrenex(s).accepts("07bcdda "))
	}
}
