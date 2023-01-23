import SpecialAtoms.Epsilon

import scala.collection.immutable.HashMap
import scala.collection.mutable.Stack


/*
* 	Enum used for the union type in
* the transition function.
*/
sealed trait SpecialAtoms
object SpecialAtoms{
	case object Epsilon extends SpecialAtoms
	case object Void extends SpecialAtoms	// not used in this stage
}


/*
*  Main NFA class.
*/
class Nfa[A](states: Set[A],
			 alphabet: Set[Char],
			 // could have typed the transition function
			 // in a more concise way by replacing the Either
			 // with a simple String, but this has the same effect.
			 transitionFn: HashMap[(A, Either[Char, SpecialAtoms]), Set[A]],
			 startState: A,
			 finalStates: Set[A]) {


	def map[B](f: A => B): Nfa[B] = {
		val _states: Set[B] = this.states.map[B](f)
		val _transitionFn: HashMap[(B, Either[Char, SpecialAtoms]), Set[B]] =
			this.transitionFn.map(
				x => ((f(x._1._1), x._1._2), x._2.map(f))
			)
		val _startState: B = f(this.startState)
		val _finalStates: Set[B] = this.finalStates.map[B](f)


		new Nfa[B](_states, this.alphabet, _transitionFn, _startState, _finalStates)
	}


	/*
	* 	Epsilon closure function
	* used for correctly building
	* an NFA.
	*
	* 	Implicit argument currStates
	* is used so that the function doesn't
	* loop forever in the following case:
	* 	q0 -eps> q1 -eps> q0
	* 	(not the only case)
	* */
	def epsClosure(state: A, currStates: Set[A] = Set[A]()): Set[A] = {

		this.transitionFn.get((state, Right(Epsilon))) match {
			case Some(epsStates) => {
				var resStates: Set[A] = Set(state)

				for (epsState <- epsStates) {
					if (!resStates.contains(epsState) && !currStates.contains(epsState)) {
						resStates += epsState
						resStates = resStates union epsClosure(epsState, currStates union resStates)
					}
				}

				resStates
			}
			case None => Set(state)
		}
	}


	/*
	* 	Returns the next states given
	* a state and a character by getting the
	* epsilon closure of every state returned
	* from the transition function.
	*/
	def next(state: A, c: Char): Set[A] = {
		this.transitionFn.get((state, Left(c))) match {
			case Some(nextStates) => {
				nextStates.foldRight(nextStates)(
					(curr: A, acc: Set[A]) => acc union epsClosure(curr)
				)
			}
			case None => Set()
		}
	}


	def accepts(str: String): Boolean = {
		def accepts_aux(str: String, currentState: A = this.startState): Boolean = {
			/*
			* Got to the end of string
			* in an accept state
			*/
			if (str.isEmpty && isFinal(currentState)) return true

			/*
			* Got to the end of the string,
			* but not in a final state
			*/
			if (str.isEmpty) return false

			// Get the following states
			val nextStates: Set[A] = next(currentState, str.head)

			// No following state => fail
			if (nextStates.isEmpty) return false

			/*
			* Recursively check for
			* at least one configuration
			*/
			nextStates.foldRight(false)(
				(curr: A, acc: Boolean) => accepts_aux(str.tail, curr) || acc
			)
		}

		// at least one needs to accept
		epsClosure(this.startState).foldRight(false)(
			(curr, acc) => accepts_aux(str, curr) || acc
		)
	}


	// <Getters>
	def getStates: Set[A] = this.states

	def getAlphabet: Set[Char] = this.alphabet

	def getTransitionFn: HashMap[(A, Either[Char, SpecialAtoms]), Set[A]] = this.transitionFn

	def getStartState: A = this.startState

	def getFinalStates: Set[A] = this.finalStates
	// </Getters>


	def isFinal(state: A): Boolean = this.finalStates.contains(state)


	override def toString: String = {
		"States:\n" +
		this.states.toString() +
		"\nTransitions:\n" +
		this.transitionFn.toString() +
		"\nFinal States:\n" +
		this.finalStates.toString() +
		"\nStart State:\n" +
		this.startState.toString()
	}
}


/*
* 	Static methods of NFA.
* Main one is the PRENEX -> NFA transformation.
*/
object Nfa {
	def fromPrenex(str: String): Nfa[Int] = {
		/*
		* 	Met a single character PRENEX
		* which means this can be transformed
		* directly to an NFA which recognizes
		* that character.
		*/
		if (str.length == 1) return characterNFA(0, str.head)

		/*
		* 	Met a complex PRENEX so do the
		* reverse stack method.
		*/
		val listOfOperations: List[String] = {
			"\\w+|\'[\\w\\s]*\'|-|=|[*]|[+]|[?]|[>]|[<]|[(]|[)]".r
			.findAllIn(str)
			.toList
			.map(x => "^\'|\'$".r.replaceAllIn(x, ""))
			.reverse
		}




		val stackNFA: Stack[Nfa[Int]] = Stack()
		/*
		* 	Index used so that no two
		* states have the same identifier
		*/
		var maxIndex: Int = 0

		for (op <- listOfOperations) {
			if (op == "CONCAT") {
				val n1: Nfa[Int] = stackNFA.pop()
				val n2: Nfa[Int] = stackNFA.pop()
				val nCONCAT: Nfa[Int] = Nfa.concatNFA(n1, n2)
				stackNFA.push(nCONCAT)
			} else if (op == "UNION") {
				val n1: Nfa[Int] = stackNFA.pop()
				val n2: Nfa[Int] = stackNFA.pop()
				val nUNION: Nfa[Int] = Nfa.unionNFA(n1, n2)
				stackNFA.push(nUNION)
				maxIndex += 2
			} else if (op == "STAR") {
				val n: Nfa[Int] = stackNFA.pop()
				val nSTAR: Nfa[Int] = Nfa.starNFA(n)
				stackNFA.push(nSTAR)
				maxIndex += 2
			} else if (op == "PLUS") {
				val n: Nfa[Int] = stackNFA.pop()
				val nPLUS: Nfa[Int] = Nfa.plusNFA(n)
				stackNFA.push(nPLUS)
				maxIndex += n.getStates.size + 2
			} else if (op == "MAYBE") {
				val n: Nfa[Int] = stackNFA.pop()
				val nMAYBE: Nfa[Int] = Nfa.maybeNFA(n)
				stackNFA.push(nMAYBE)
				maxIndex += 4
			} else if (op == "void") {
				stackNFA.push(Nfa.voidNFA(maxIndex))
				maxIndex += 2
			} else if (op == "eps") {
				stackNFA.push(Nfa.epsilonNFA(maxIndex))
				maxIndex += 2
			} else { // base case -> single character
				stackNFA.push(Nfa.characterNFA(maxIndex, op.head))
				maxIndex += 2
			}
		}

		/*
		* 	If the PRENEX is correct,
		* at the end of iterating trough
		* the list of operations there will
		* be one NFA remaining on the stack,
		* which is the conversion of our
		* PRENEX to our desired NFA.
		*/
		stackNFA.pop()
	}


	//---------------------------STRUCTURAL INDUCTION BASE CASES---------------------------//
	//						   (the core of the Thompson algorithm)						   //

	/*
	* 	NFA which accepts void.
	*/
	private def voidNFA(index: Int): Nfa[Int] = {
		new Nfa[Int](
			states = Set(index, index + 1),
			alphabet = Set(),
			transitionFn = HashMap(),
			startState = index,
			finalStates = Set(index + 1)
		)
	}


	/*
	* 	NFA which accepts epsilon.
	*/
	private def epsilonNFA(index: Int): Nfa[Int] = {
		new Nfa[Int](
			states = Set(index, index + 1),
			alphabet = Set(),
			transitionFn = HashMap(((index, Right(Epsilon)), Set(index + 1))),
			startState = index,
			finalStates = Set(index + 1)
		)
	}


	/*
	* 	NFA which accepts a character.
	*/
	private def characterNFA(index: Int, c: Char): Nfa[Int] = {
		new Nfa[Int](
			states = Set(index, index + 1),
			alphabet = Set(c),
			transitionFn = HashMap(((index, Left(c)), Set(index + 1))),
			startState = index,
			finalStates = Set(index + 1)
		)
	}


	/*
	* 	NFA is a monoid.
	* We assume that the NFAs have
	* only one accept state each.
	* (this is the assumption made
	* in the Thompson transformation)
	* 	Also, we assume that the states
	* identifiers do not overlap.
	*/
	private def concatNFA(n1: Nfa[Int], n2: Nfa[Int]): Nfa[Int] = {
		new	Nfa[Int](
			states = n1.getStates union n2.getStates,
			alphabet = n1.getAlphabet union n2.getAlphabet,
			transitionFn =
				n1.getTransitionFn
				++ n2.getTransitionFn
				+ ((n1.getFinalStates.head, Right(Epsilon)) -> Set(n2.getStartState)),
			startState = n1.getStartState,
			finalStates = n2.getFinalStates
		)
	}


	/*	NFA which accepts the union of
	* two other NFAs.
	*/
	private def unionNFA(n1: Nfa[Int], n2: Nfa[Int]): Nfa[Int] = {
		val nStart: Int = n1.getStates.max[Int] + 1
		val nFinal: Int = nStart + 1
		val nTransitions: HashMap[(Int, Either[Char, SpecialAtoms]), Set[Int]] =
			HashMap(
				// from new start to old starts reading epsilon
				((nStart, Right(Epsilon)), Set(n1.getStartState, n2.getStartState)),
				// from old accept states to new accept state reading epsilon
				((n1.getFinalStates.head, Right(Epsilon)), Set(nFinal)),
				((n2.getFinalStates.head, Right(Epsilon)), Set(nFinal))
			)

		new Nfa[Int](
			states = n1.getStates union n2.getStates union Set(nStart, nFinal),
			alphabet = n1.getAlphabet union n2.getAlphabet,
			transitionFn =
				n1.getTransitionFn
				++ n2.getTransitionFn
				++ nTransitions,
			startState = nStart,
			finalStates = Set(nFinal)
		)
	}


	/*
	* 	NFA which accepts the klein star
	* of another NFA.
	*/
	private def starNFA(n: Nfa[Int]): Nfa[Int] = {
		val nStart: Int = n.getStates.max[Int] + 1
		val nFinal: Int = nStart + 1
		val nTransitions: HashMap[(Int, Either[Char, SpecialAtoms]), Set[Int]]  =
			HashMap(
				/*
				* in order to accept epsilon
				* new start to old start
				*/
				((nStart, Right(Epsilon)), Set(nFinal, n.getStartState)),
				/*
				* accept the NFA 1 or more times
				* from old accept state to old start state
				* reading epsilon
				* 	AND
				* old accept state to new accept state
				*/
				((n.getFinalStates.head, Right(Epsilon)), Set(nFinal, n.getStartState)),
			)
		new Nfa[Int](
			states = n.getStates union Set(nStart, nFinal),
			alphabet = n.getAlphabet,
			transitionFn =
				n.getTransitionFn ++ nTransitions,
			startState = nStart,
			finalStates = Set(nFinal)
		)
	}


	/*
	* 	NFA which accepts another NFA
	* under the plus operation.
	* 	This can be constructed by
	* concatenating the given NFA to itself
	* under the klein star.
	*/
	private def plusNFA(n: Nfa[Int]): Nfa[Int] =
		concatNFA(n, starNFA(n))


	/*
	* 	NFA which accepts another NFA
	* under the MAYBE operation.
	*/
	private def maybeNFA(n: Nfa[Int]): Nfa[Int] =
		unionNFA(n, epsilonNFA(n.getStates.max[Int] + 2))


}


