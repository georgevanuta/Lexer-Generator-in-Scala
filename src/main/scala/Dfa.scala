import SpecialAtoms.Epsilon

import scala.collection.immutable.HashMap
import scala.collection.mutable.Stack

class Dfa[A](states: Set[A],
			 transitionFn: HashMap[(A, Char), A],
			 alphabet: Set[Char],
			 startState: A,
			 finalStates: Set[A],
			 sink: A) {


	def map[B](f: A => B) : Dfa[B] = {
		val _states: Set[B] = this.states.map(f)
		val _transitionFn: HashMap[(B, Char), B] =
			this.transitionFn.map(x => ((f(x._1._1), x._1._2), f(x._2)))
		val _startState: B = f(this.startState)
		val _finalStates: Set[B] = this.finalStates.map(f)
		val _sink: B = f(sink)
		new Dfa[B](_states, _transitionFn, alphabet, _startState, _finalStates, _sink)
	}


	def next(state: A, c: Char): A = {
		this.transitionFn.get((state, c)) match {
			case Some(nextState) => nextState
			case None => sink
		}
	}


	def accepts(str: String, currentState: A = this.startState): Boolean = {
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

		// Go to the next configuration
		val nextState: A = next(currentState, str.head)
		if (nextState == sink) return false

		accepts(str.tail, nextState)
	}


	// <Getters>
	def getStates : Set[A] = this.states

	def getTransitionFn: HashMap[(A, Char), A] = this.transitionFn

	def getAlphabet: Set[Char] = this.alphabet

	def getFinalStates: Set[A] = this.finalStates

	def getStartState: A = this.startState

	def getSink: A = this.sink
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


object Dfa {
	//	 DFA[INT]
	//      ^
	//		| <------------- Encoding of states
	//		|
	// DFA[Set[Int]]
	//   	^
	//		| <------------- Subset Construction
	//		|
	//	 NFA[Int]
	//      ^
	//		| <------------- Thompson's Construction
	//		|
	//	 POSTNEX
	//		^
	// 		| <------------- Conversion from Prefix to Postfix
	//		|
	//	 PRENEX
	def fromPrenex(str: String): Dfa[Int] = {
		val dfa: Dfa[Set[Int]] = fromNFAtoDFA(Nfa.fromPrenex(str))
		/*
		* Q: Why do this?
		* A: Because our subset construction generated
		* a dfa where each state is represented by a set
		* (a group of states). It will be much more convenient
		* to have a dfa with states directly represented by
		* indexes. But, because this mapping has to be unique
		* (a bijective function from sets to ints), we have
		* to be careful with the one we choose. For simplicity,
		* I chose a function with maps sets to its index.
		*/
		dfa.map(
			x => dfa.getStates
				    .toList
				    .indexOf(x)
		)
	}

	def encodeFromSets(dfa: Dfa[Set[Int]]): Dfa[Int] = {
		dfa.map(
			x => dfa.getStates
			.toList
			.indexOf(x)
		)
	}


	/*
	* 	Returns the subset which represents
	* the start state group.
	*/
	private def getStartSubSet(nfa: Nfa[Int]): Set[Int] =
		nfa.epsClosure(nfa.getStartState)


	/*
	* 	Given a state group, returns the
	* next state group from the nfa for
	* the given character.
	*/
	private def getNextSubSet(nfa: Nfa[Int], char: Char, subSet: Set[Int]): Set[Int] = {
		subSet.foldRight(Set[Int]())(
			(curr, acc) => nfa.next(curr, char) union acc
		)
	}


	/*
	* 	The Subset Construction algorithm.
	*/
	def fromNFAtoDFA(nfa: Nfa[Int]): Dfa[Set[Int]] = {
		// First step -> get the start group
		val startState: Set[Int] = getStartSubSet(nfa)
		var transitionFn: HashMap[(Set[Int], Char), Set[Int]] = HashMap()
		var states: Set[Set[Int]] = Set()

		/*
		* Stack which remembers the group state
		* to be evaluated next.
		*/
		val stack: Stack[Set[Int]] = Stack(startState)


		while (!stack.isEmpty) {
			// get the next state group to investigate for
			// transitions and other state groups
			val stateToInspect: Set[Int] = stack.pop()
			states = states + stateToInspect

			for (ch <- nfa.getAlphabet) {
				// valid transition -> add it to the dfa transition function
				val nextStateOnCh: Set[Int] = getNextSubSet(nfa, ch, stateToInspect)

				transitionFn = transitionFn + ((stateToInspect, ch) -> nextStateOnCh)

				// this is so that this doesn't loop forever
				// in the case that a group state goes to itself
				if (!nextStateOnCh.isEmpty && !states.contains(nextStateOnCh)) {
					// push the next group to the stack for later processing
					stack.push(nextStateOnCh)
				}
			}
		}

		// look trough the subset states
		// for at least one final state
		val finalStates: Set[Set[Int]] =
			states.filter(
				stateSet => stateSet.exists(
					state => nfa.isFinal(state)
				)
			)


		// the construction is finished
		new Dfa[Set[Int]](
			states,
			transitionFn,
			nfa.getAlphabet,
			startState,
			finalStates,
			Set(-1)
		)
	}

	def reverseDFA(dfa: Dfa[Int]): Nfa[Int] = {
		val ReversedStartState: Int =
			dfa.getStates.max + 1

		val ReversedTransitionFn: HashMap[(Int, Either[Char, SpecialAtoms]), Set[Int]] = {

			def addToMultimap(
							 	m: HashMap[(Int, Either[Char, SpecialAtoms]), Set[Int]],
							 	key: (Int, Either[Char, SpecialAtoms]),
							 	value: Int
							 ): HashMap[(Int, Either[Char, SpecialAtoms]), Set[Int]] = {

				m.get(key) match {
					case Some(values) => m.updated(key, values union Set(value))
					case None => m.updated(key, Set(value))
				}
			}

			var res: HashMap[(Int, Either[Char, SpecialAtoms]), Set[Int]] =
				dfa.getTransitionFn.foldRight(HashMap[(Int, Either[Char, SpecialAtoms]), Set[Int]]())(
					(curr, acc) => {
						val FromState: Int = curr._1._1
						val ReadingChar: Char=  curr._1._2
						val ToState: Int = curr._2

						addToMultimap(acc, (ToState, Left(ReadingChar)), FromState)
					}
				)

			for (f <- dfa.getFinalStates) {
				res = addToMultimap(res, (ReversedStartState, Right(Epsilon)), f)
			}

			res
		}

		val ReversedStates: Set[Int] =
			dfa.getStates union Set(ReversedStartState)

		val ReversedFinalStates: Set[Int] =
			Set(dfa.getStartState)

		new Nfa[Int](
			ReversedStates,
			dfa.getAlphabet,
			ReversedTransitionFn,
			ReversedStartState,
			ReversedFinalStates
		)
	}


	def minDFA(dfa: Dfa[Int]): Dfa[Int] = {
		def reverseAndDeterminate(d: Dfa[Int]): Dfa[Int] = {
			encodeFromSets(
				fromNFAtoDFA(
					reverseDFA(
						d
					)
				)
			)
		}

		val DFA1: Dfa[Int] = reverseAndDeterminate(dfa)
		val DFA2: Dfa[Int] = reverseAndDeterminate(DFA1)

		DFA2
	}
}
