// This is the compiler that translates regex abstract syntax trees into regular
// expression matching virtual machine programs.

package edu.ucsb.cs.cs162.regex.vm.compiler

import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._
import Regex._

object Compiler {
  // Return a virtual machine program that implements the given regex.
  def compile(re: Regex): Program = {
    def compileRecursively(re: Regex): Program = re match {
      case `∅` => IndexedSeq(Reject)

      case `ε` => IndexedSeq(PushEmpty)

      case Chars(cs) => IndexedSeq(MatchSet(cs), PushChar)

      case Concatenate(r1, r2) =>
        (compileRecursively(r1) ++ (compileRecursively(r2)) :+ PushConcat)

      case Union(r1, r2) => {
        val right = compileRecursively(r2) :+ PushRight

        val left =
          compileRecursively(r1) ++ IndexedSeq(PushLeft, Jump(right.length + 1))

        Fork(1, left.length + 1) +: (left ++ right)
      }

      case KleeneStar(r) => {
        val rBody = compileRecursively(r) :+ PushStar

        val forkSize = rBody.length + 2

        r.nullable match {
          case `ε` =>
            IndexedSeq(
              InitStar,
              CheckProgress,
              Fork(1, forkSize)) ++ (rBody :+ Jump(-1 * forkSize))

          case `∅` =>
            IndexedSeq(
              InitStar,
              Fork(1, forkSize)) ++ (rBody :+ Jump(-1 * (forkSize - 1)))
        }
      }

      case Capture(name, r) => compileRecursively(r) :+ PushCapture(name)
    }

    compileRecursively(re) :+ Accept
  }
}
