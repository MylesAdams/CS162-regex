package edu.ucsb.cs.cs162.regex.vm.compiler

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._

class CompileSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')

  behavior of "compile"

  it should "correctly compile the empty language" in { pending }

  it should "correctly compile ε" in { pending }

  it should "correctly compile concatenation" in  { pending }

  it should "correctly compile union" in  { pending }

  it should "correctly compile kleene star" in  { pending }
  // more tests...

  it should "correctly compile complex regexes 1" in {
    Compiler.compile(Concatenate(Union(b,c), b)) should equal (IndexedSeq(Fork(1,5), MatchSet(b.chars), PushChar, PushLeft, Jump(4), MatchSet(c.chars), PushChar, PushRight, MatchSet(b.chars), PushChar, PushConcat, Accept))
  }

  it should "correctly compile complex regexes 2" in { pending }

  // more tests...
}
