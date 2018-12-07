// Provides the Dfa class for deterministic finite automata.

package edu.ucsb.cs.cs162.dfa

import edu.ucsb.cs.cs162.range_set._
import scala.collection.immutable.Queue

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
      todo: Queue[(State, String)],
      visited: Set[State],
    ): Option[String] = fin contains todo.head._1 match {
      case false if delta contains todo.head._1 => {
        val newTodo = delta(todo.head._1).foldLeft(todo.tail) {
          (acc, csTuple) => {
            visited contains csTuple._2 match {

              case false if (todo.head != csTuple._2 && !csTuple._1.isEmpty) =>
                acc :+ (csTuple._2, todo.head._2 + csTuple._1.minElement.get)

              case true => acc
            }
          }
        }

        newTodo.isEmpty match {

          case false => BFS(newTodo, visited + todo.head._1)

          case true => None
        }
      }

      case true => Some(todo.head._2)

      case _ => None
    }

    BFS(Queue[(State, String)]((init, "")), Set[State]())

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
