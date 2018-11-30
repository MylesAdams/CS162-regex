// Provides the Dfa class for deterministic finite automata.

package edu.ucsb.cs.cs162.dfa

import edu.ucsb.cs.cs162.range_set._

object `package` {
  type Transitions[State] = Map[State, Seq[(CharSet, State)]]
}

// The DFA. 'delta' is the state transition map; 'init' is the initial state;
// 'fin' is the set of final states. The DFA is assumed to have an explicit
// error state and the transitions are assumed to cover the complete range of
// possible characters.
case class Dfa[State](delta: Transitions[State], init: State, fin: Set[State]) {
  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Returns true iff the given string is recognized by the DFA.
  def matches(str: String): Boolean =
    fin.contains(trace(init, str))

  // Returns a string that causes an arbitrary but non-looping path from the
  // init state to a final state, if such a path exists.
  def getString: Option[String] = {
    def BFS(
      todo: Set[State],
      visited: Set[State],
      strings: Map[State, String]
    ): Map[State, String] = {
      val todoAndStrings = delta(todo.head).foldLeft((todo.tail, strings)) {
        (acc, csTuple) => {
          visited contains csTuple._2 match {
            case false if (todo.head != csTuple._2 && !csTuple._1.isEmpty)  =>
              (acc._1 + csTuple._2,
               acc._2 +
                 (csTuple._2 -> (strings(todo.head) + csTuple._1.minElement)))

            case _ => acc
          }
        }}

      todoAndStrings._1.isEmpty match {
        case true => strings

        case false =>
          BFS(todoAndStrings._1, visited + todo.head, todoAndStrings._2)
      }

    }

    val stringsMap = BFS(Set[State](init), Set[State](), Map[State, String](init -> ""))

    val validStrings = stringsMap.filterKeys(key => fin contains key)

    validStrings.isEmpty match {
      case false => Some(validStrings.head._2)

      case true => None
    }
  }



  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Compute the state reached by tracing the given string through the DFA.
  @annotation.tailrec
  private def trace(state: State, str: String): State =
    if (str.isEmpty) state
    else delta(state).find(_._1.contains(str.head)) match {
      case Some((_, next)) => trace(next, str.tail)
      case None => {
        assert(false, "should be unreachable")
        state
      }
    }
}
