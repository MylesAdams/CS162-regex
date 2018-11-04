// Provides a derivative-based static analysis of regular expressions that
// yields a DFA describing the language recognized by an expression.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.dfa._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

object DerivativeAnalysis {
  import Derive._
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Statically analyzes 're' using derivatives in order to compute the DFA of
  // the language recognized by 're'. The resulting DFA has an explicit error
  // state and is approximately minimal.
  def analyze(re: Regex): Dfa[Regex] = ???

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  implicit class CharSetPairwise(charsets: Set[CharSet]) {
    def ^(other: Set[CharSet]): Set[CharSet] = {
      charsets.flatMap(cs => other.foldLeft(Set[CharSet]()){(set, csi) => set + (csi & cs)})
    }
  }

  // Compute the transitions and set of reachable states (i.e., Regexes) for all
  // Regexes in 'todo'.
  @annotation.tailrec
  private def computeDfa(todo: Set[Regex], visitedStates: Set[Regex],
                         transitions: Transitions[Regex]) : (Set[Regex], Transitions[Regex]) = {
    if (!todo.isEmpty) {
      val x = computeNext(todo.head)
      computeDfa(todo.drop(1) ++ x._1, visitedStates + todo.head, transitions ++ x._2)
    }
    else {
      (visitedStates, transitions)
    }

  }

  // Compute the transitions and destination states from the given regex.
  private def computeNext(state: Regex): (Set[Regex], Transitions[Regex]) = {
    def partition(re: Regex): Set[CharSet] = re match {
      case `∅` => Set(α.chars)
      case `ε` => Set(α.chars)
      case Chars(s) => Set(s, α.chars & s)
      case KleeneStar(r) => partition(r)
      case Complement(r) => partition(r)
      case Union(r, s) => partition(r) ^ partition(s)
      case Intersect(r, s) => partition(r) ^ partition(s)
      case Concatenate(r, s) if (r.nullable == `∅`) => partition(r)
      case Concatenate(r, s) => partition(r) ^ partition(s)
    }

    val derivativeMachine = new DerivativeMachine(state)

    val endStatesAndTransitionSeq = partition(state).foldLeft((Set[Regex](), Seq[(CharSet, Regex)]())) {
      (acc, cs) => {
        val derivative = derivativeMachine.derive(cs.minValue)
        (acc._1 + derivative, acc._2 :+ (cs, derivative))
      }
    }

    (endStatesAndTransitionSeq._1, Map(state -> endStatesAndTransitionSeq._2))
  }
}
