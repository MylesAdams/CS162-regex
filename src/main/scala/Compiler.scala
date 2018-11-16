// This is the compiler that translates regex abstract syntax trees into regular
// expression matching virtual machine programs.

package edu.ucsb.cs.cs162.regex.vm.compiler

import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._
import Regex._

object Compiler {
  // Return a virtual machine program that implements the given regex.
  def compile(re: Regex): Program = re match {
    case `∅` => IndexedSeq(Reject)
    case `ε` => IndexedSeq(PushEmpty, Accept)
    case Chars(cs) => IndexedSeq(Reject)
  }

}
