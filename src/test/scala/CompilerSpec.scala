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
  val bSet = b.chars
  val cSet = c.chars
  val dSet = d.chars

  behavior of "compile"

  it should "correctly compile the empty language" in {
    val re = ∅
    val correctProgram = IndexedSeq(
      Reject,
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }

  it should "correctly compile ε" in {
    val re = ε
    val correctProgram = IndexedSeq(
      PushEmpty,
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }

  it should "correctly compile concatenation" in  {
    val re = b ~ c
    val correctProgram = IndexedSeq(
      MatchSet(bSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      PushConcat,
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }

  it should "correctly compile union" in  {
    val re = Union(b,c)
    val correctProgram = IndexedSeq(
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(cSet),
      PushChar,
      PushRight,
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }

  it should "correctly compile kleene star" in  {
    val re = b.*
    val correctProgram = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }
  // more tests...

  it should "correctly compile complex regexes 1" in {
    val re = Concatenate(Union(b,c), b)
    val correctProgram = IndexedSeq(
      Fork(1,5),
      MatchSet(b.chars),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(c.chars),
      PushChar,
      PushRight,
      MatchSet(b.chars),
      PushChar,
      PushConcat,
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }

  it should "correctly compile complex regexes 2" in {
    val re = (ε | b).*
    val correctProgram = IndexedSeq(
      InitStar,
      CheckProgress,
      Fork(1, 10),
      Fork(1, 4),
      PushEmpty,
      PushLeft,
      Jump(4),
      MatchSet(bSet),
      PushChar,
      PushRight,
      PushStar,
      Jump(-10),
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }

  it should "correctly compile complex regexes 3" in {
    val re = (b | c).* ~ d.+
    val correctProgram = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(Chars('b', 'c').chars),
      PushChar,
      PushStar,
      Jump(-4),
      MatchSet(dSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushConcat,
      Accept)

      Compiler.compile(re) should equal (correctProgram)
  }

  it should "correctly compile complex regexes 4" in {
    val re = (b.* | d) ~ (d.* | b)
    val correctProgram = IndexedSeq(
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushLeft,
      Jump(8),
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushRight,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushLeft,
      Jump(8),
      InitStar,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushRight,
      PushConcat,
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }

  it should "correctly compile complex regexes 5" in {
    val re = (b | (c | d.*)) ~ d.*
    val correctProgram = IndexedSeq(
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushLeft,
      Jump(14),
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushLeft,
      Jump(8),
      InitStar,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushRight,
      PushRight,
      InitStar,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }

  it should "correctly compile complex regexes 6" in {
    val re = ((d | c.+)^4) ~ (d | b).+
    val correctProgram = IndexedSeq(
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushLeft,
      Jump(11),
      MatchSet(cSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushRight,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushLeft,
      Jump(11),
      MatchSet(cSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushRight,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushLeft,
      Jump(11),
      MatchSet(cSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushRight,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushLeft,
      Jump(11),
      MatchSet(cSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushRight,
      MatchSet(Chars('b', 'd').chars),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(Chars('b', 'd').chars),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushConcat,
      PushConcat,
      PushConcat,
      PushConcat,
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }
  it should "correctly compile complex regexes 7" in {
    val re = (d.? | c).+ ~ b
    val correctProgram = IndexedSeq(
      Fork(1,4),
      PushEmpty,
      PushLeft,
      Jump(10),
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(dSet),
      PushChar,
      PushRight,
      PushRight,
      InitStar,
      CheckProgress,
      Fork(1,16),
      Fork(1,4),
      PushEmpty,
      PushLeft,
      Jump(10),
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(dSet),
      PushChar,
      PushRight,
      PushRight,
      PushStar,
      Jump(-16),
      MatchSet(bSet),
      PushChar,
      PushConcat,
      PushConcat,
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }

  it should "correctly compile complex regexes 8" in {
    val re = Concatenate(ε, Union(∅, d))
    val correctProgram = IndexedSeq(
      PushEmpty,
      Fork(1,4),
      Reject,
      PushLeft,
      Jump(4),
      MatchSet(dSet),
      PushChar,
      PushRight,
      PushConcat,
      Accept)

    Compiler.compile(re) should equal (correctProgram)
  }

  // more tests...
}
