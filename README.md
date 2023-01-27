# Lexer Generator

`Language: Scala`

**`Tests passed:`**

- `1st stage: all`
- `2nd stage: all`
- `3rd stage: all`

## Table of Contents

- [LFA - Project 2022](#lexer-generator)
	- [Table of Contents](#table-of-contents)
	- [Stage 1 - Prenex to NFA to DFA](#stage-1---prenex-to-nfa-to-dfa)
		- [NFA](#nfa)
		- [DFA](#dfa)
		- [Thompson's Construction](#thompsons-construction)
		- [Powerset Construction](#powerset-construction)
			- [Encoding of the states](#encoding-of-the-states)
			- [Actual algorithm](#actual-algorithm)
	- [Stage 2 - Regex to Prenex](#stage-2---regex-to-prenex)
		- [Preprocessing](#preprocessing)
		- [Actual Algorithm](#actual-algorithm-1)
	- [Stage 3 - Lexer Generator](#stage-3---lexer-generator)
		- [Minimum DFA](#minimum-dfa)
		- [Rules](#rules)
			- [Regex Rules](#regex-rules)
			- [NFA Rules](#nfa-rules)
			- [DFA Rules](#dfa-rules)
			- [ENC Rules](#enc-rules)
			- [Min DFA Rules](#min-dfa-rules)
		- [Lexer automata](#lexer-automata)
	- [References](#references)

## Stage 1 - Prenex to NFA to DFA

### NFA

My implementation of the generic class `Nfa[A]` has **5** class members:

- $Q: $
`states: Set[A]` which holds the states of the automata. I chose the type
to be of `Set` because it makes no sense to have duplicate states to do
different things.
- $\Sigma: $ 
`alphabet: Set[Char]` which holds the alphabet of the *NFA*. Same reason as
above to why I chose this to be a `Set`.
- $\Delta: $
`transitionFn: HashMap[(A, Either[Char, SpecialAtoms]), Set[A]]`, which is
is a `HashMap` with keys of type state (`A`) and character read, which is either a simple
character (`Left[Char]`) or special expression `Right[SpecialAtoms]`. I choose this instead
of `String` because I wanted to make clear the existence of the special atoms.
- $q_0: $
`startState: A`.
- $F: $ 
`finalStates: Set[A]`.

### DFA

My implementation of the generic class `Dfa[A]` has **6** class members:

- $Q: $
`states: Set[A]`, same explanation as at *NFA*.
- $\delta: $
`transitionFn: HashMap[(A, Char), A]`, which takes a pair of current state and
character read, and returns the next state if present in the transition function, or `sink`.
- $\Sigma: $
`alphabet: Set[Char]`.
- $q_0: $
`startState: A`.
- $F: $
`finalStates: Set[A]`.
- $SINK: $
`sink: A`, which represents the sink state. This was added because of the
given signature of the `next` function, making it impossible to not return a state
when not found in the transition function. This `sink` state is hardcoded to **not**
overlap with **ANY** other states.

### Thompson's Construction

- Before processing the *prenex*, we need to split the given *prenex* string into
members. This is done via find all matches with a *regex*, which splits on whitespaces,
excluding commas.

```scala
val listOfOperations: List[String] = {
    "\\w+|\'[\\w\\s]*\'|-".r
    .findAllIn(str)
    .toList
    .map(x => "^\'|\'$".r.replaceAllIn(x, ""))
.reverse
}
```

- There are a multitude of ways to process a *prenex*, but I found the easiest
one to be transforming it into a *postnex* (*Regex* in postfix notation - $RPN^1$).
- The transformation algorithm in very simple, just reversing the prefix notation
does the job.
- After that, I keep a *stack* of type `Stack[Nfa[Int]]`. For each parsed member of
the *postnex* there are two possible cases:
  - if it parses a `character`, `void` or `eps`, I **just** push it to the *stack* after converting
it to the appropriate *NFA*.
  - if it parses an operation of type `CONCAT` or `UNION`, I pop **two** *NFA's* from the *stack*,
construct the appropiate *NFA* in terms of the previous operation, and then push
the result to the *stack*.
  - if it parses an operation of type `STAR`, `PLUS` or `MAYBE`, I pop **one** *NFA* from the *stack*,
construct the appropiate *NFA* given the operation, and then push the result to the *stack*.
- After parsing **ALL** of the *postnex*, if it was a valid *postnex* (ex: not `UNION c`) there will be one
remaining element on the *stack*, that being the resulting *NFA*.

### Powerset Construction

#### Encoding of the states

This construction is very easy to do both in code and on paper. The only small challenge in code
is that this will return you a `Dfa[Set[Int]]`, not the desired `Dfa[Int]`. In order to achieve this, we
need a function of signature:

```scala
Set[Int] => Int
```

which is bijective (aka have a mapping of each element to a unique value). It needs to have this property
in order to not let any two states overlap. For example, if we choose this function to be the sum of the elements
of the given set, it will not have an unique output for every input (`Set(1,2,3).sum == Set(1, 5).sum`).
The function is the following:

```scala
dfa.map(
  x => dfa.getStates
          .toList
          .indexOf(x)
)
```

This function basically transforms a set to its index in the *DFA's* set of states ($Q$).

#### Actual algorithm

Now that we got the encoding part out of the way, let's get to the actual core of the algorithm.

- First of all, let's look at how to get the start state of the *DFA*:

```scala
private def getStartSubSet(nfa: Nfa[Int]): Set[Int] =
    nfa.epsClosure(nfa.getStartState)
```

This is exactly as states by the algorithm: the epsilon closure of the start state of the *NFA*.

- Next, we need a function which takes as from a group of states, reading a character to the
next group of states:

```scala
private def getNextSubSet(nfa: Nfa[Int], char: Char, subSet: Set[Int]): Set[Int] = {
    subSet.foldRight(Set[Int]())(
        (curr, acc) => nfa.next(curr, char) union acc
    )
}
```

This function collects the next states of each function from the input group.

- Now, we need to actually build the states and the transition function of the constructed *DFA*.
This is done via the following function:

```scala
private def fromNFAtoDFA(nfa: Nfa[Int]): Dfa[Set[Int]]
```

I didn't add the implementation here because it's quite long. What it does is, having a start group
and a stack to keep account of which state to group is being analyzed, it looks through all of the possible
transitions from that group, building the `Dfa[Set[Int]]` along the way.

- Finally, having all of the group states, it's very easy to get the final ones:

```scala
    val finalStates: Set[Set[Int]] =
        states.filter(
            stateSet => stateSet.exists(
                state => nfa.isFinal(state)
            )
        )
```

This lambda returns the group states in which there exists at least one final state.

The `alphabet` is the same as the *NFA's* and the `sink` state is hardcoded to be `-1`.

Now, we have all the elements to build our *DFA*:

```scala
  def fromPrenex(str: String): Dfa[Int] = {
    val dfa: Dfa[Set[Int]] = fromNFAtoDFA(Nfa.fromPrenex(str))
      dfa.map(
        x => dfa.getStates
                .toList
                .indexOf(x)
      )
  }
```

## Stage 2 - Regex to Prenex

### Preprocessing

The preprocessing of the **Regex** is done in the `preprocess` method. It iterates through
each character of the regex, one by one, classifying it as either as a control character
or a normal one. It returns a `List[Either[Char, Char]]`, which has the following
significance:

- `Left(char)`: This holds a normal character, which could also be a character used
 as an operation (*, | etc.).
- `Right(control)`: This holds a character with a special meaning, such as an operation,
 parenthesis, epsilon.

This function also has the functionality of desugarizing syntax. A range written as `[0-2]`
will be translated to `0|1|2`. Also, it concatanates explicitly, by adding `Right('.')` between
operands.

### Actual Algorithm

The algorithm for converting a **Regex** to a **Prenex** is based on the $Shunting \ Yard \ Algorithm^2$
by **Dijkstra**. It first converts it to a **Postnex** because it's easier, after which it will simply convert
the **Postnex** to an equivalent **Prenex** using a function with the following signature:

```scala
private def fromPostfixToPrefix(postnex: String): String
```

## Stage 3 - Lexer Generator

### Minimum DFA

I implemented a DFA minimization algorithm mostly because after minimizing a DFA, there will remain
a single sink state.

The algorithm used is $Brzozowski's\ algorithm^3$.
The formula is the following:

$reverse(det(reverse(det(DFA))))$

And translated in code it looks like this:

```scala
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
```

### Rules

Rules are a simple mapping from an automata/regex to its token. They are applied in the following order:

#### Regex Rules

Found in the object ```Rules```, they map a Regex to a Token.

#### NFA Rules

Also found in the object ```Rules```, they map an NFA to a Token.

#### DFA Rules

Found in the object ```RulesDeterminizer```, they map a `DFA[Set[Int]]` to a Token.

#### ENC Rules

Found in the object ```RulesEncoder```, they map a `DFA[Int]` to a Token.

#### Min DFA Rules

Found in the object ```RulesMinimizer```, they map a `minDFA` to a Token.

### Lexer automata

I chose to implement the lexer not as a big DFA, but as a set of DFAs which all work in parallel.
The algorithm works on the same principle as a monadic parser, having the following type signature:

```scala
private def lexWord(word: String, fullWord: String): Either[ErrorMessage, (Token, String, String)]
```

Which means that it returns a tuple which holds the Token lexed, the word lexed and the rest of the string
which is to be lexed.

After that, the `lexWord` function is used on all of the sentence to generate the lexemes.

## References

1. [Reverse Polish Notation - RPN](https://en.wikipedia.org/wiki/Reverse_Polish_notation)
2. [Shunting Yard Algorithm](https://en.wikipedia.org/wiki/Shunting_yard_algorithm)
3. [Brzozowski's algorithm](https://alexandrasilva.org/files/RechabilityObservability.pdf)
