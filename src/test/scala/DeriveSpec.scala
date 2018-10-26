package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class DeriveSpec extends FlatSpec with Matchers {
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

  val r = Chars('a') | Chars('b').+
  val r1 = Chars('x', 'y').* ~ r
  val r2 = Chars('y', 'x').+ ~ r

  behavior of "matches"

  it should "recognize strings in the language 1" in {
    Derive.matches((!((c.* ~ d)) & b), "b") should equal (true)
  }

  it should "recognize strings in the language 2" in {
    Derive.matches(KleeneStar(b), "") should equal (true)
  }

  it should "recognize strings in the language 3" in {
    Derive.matches((b & (b | c)) ~ c.*, "bccc") should equal (true)
  }

  it should "recognize strings in the language 4" in {
    Derive.matches(!(b.*) ~ (b ~ c).* ~ c.*, "dbcbcbccc") should equal (true)
  }

  it should "recognize strings in the language 5" in {
    Derive.matches(d.+ ~ (b<>(2,3) | d^2).*, "dddbbddbbbddddbb") should equal (true)
  }

  it should "recognize strings in the language 6" in {
    Derive.matches(`ε` | (c.* | (b ~ d)^2)<>(3,5), "ccccbdbdbdbdccccccbdbd") should equal (true)
  }

  it should "recognize strings in the language 7" in {
    Derive.matches(!(b.* | c.+), "d") should equal (true)
  }

  it should "not recognize strings not in the language 1" in {
    Derive.matches(`ε` | (c.* | (b ~ d)^2)<>(3,5), "ccccbdbdbdbdccccccbdbdcccc") should equal (false)
  }

  it should "not recognize strings not in the language 2" in {
    Derive.matches(KleeneStar(b), "c") should equal (false)
  }

  it should "not recognize strings not in the language 3" in {
    Derive.matches(!d, "d") should equal (false)
  }

  it should "not recognize strings not in the language 4" in {
    Derive.matches((b | c) & (d | d ~ d), "") should equal (false)
  }

  it should "not recognize strings not in the language 5" in {
    Derive.matches(`∅` ~ b ~ d, "bd") should equal (false)
  }

  it should "not recognize strings not in the language 6" in {
    Derive.matches(`∅` ~ b ~ d, "bd") should equal (false)
  }

  it should "not recognize strings not in the language 7" in {
    Derive.matches(!b ~ d, "bd") should equal (false)
  }

  behavior of "eval"

  it should "recognize strings in the language 1" in {
    DerivativeMachine((!((c.* ~ d)) & b)).eval("b") should equal (true)
  }

  it should "recognize strings in the language 2" in {
    DerivativeMachine(KleeneStar(b)).eval("") should equal (true)
  }

  it should "recognize strings in the language 3" in {
    DerivativeMachine((b & (b | c)) ~ c.*).eval("bccc") should equal (true)
  }

  it should "recognize strings in the language 4" in {
    DerivativeMachine(!(b.*) ~ (b ~ c).* ~ c.*).eval("dbcbcbccc") should equal (true)
  }

  it should "recognize strings in the language 5" in {
    DerivativeMachine(d.+ ~ (b<>(2,3) | d^2).*).eval("dddbbddbbbddddbb") should equal (true)
  }

  it should "recognize strings in the language 6" in {
    DerivativeMachine(`ε` | (c.* | (b ~ d)^2)<>(3,5)).eval("ccccbdbdbdbdccccccbdbd") should equal (true)
  }

  it should "recognize strings in the language 7" in {
    DerivativeMachine(!(b.* | c.+)).eval("d") should equal (true)
  }

  it should "not recognize strings not in the language 1" in {
    DerivativeMachine(`ε` | (c.* | (b ~ d)^2)<>(3,5)).eval("ccccbdbdbdbdccccccbdbdcccc") should equal (false)
  }

  it should "not recognize strings not in the language 2" in {
    DerivativeMachine(KleeneStar(b)).eval("c") should equal (false)
  }

  it should "not recognize strings not in the language 3" in {
    DerivativeMachine(!d).eval("d") should equal (false)
  }

  it should "not recognize strings not in the language 4" in {
    DerivativeMachine((b | c) & (d | d ~ d)).eval("") should equal (false)
  }

  it should "not recognize strings not in the language 5" in {
    DerivativeMachine(`∅` ~ b ~ d).eval("bd") should equal (false)
  }

  it should "not recognize strings not in the language 6" in {
    DerivativeMachine(`∅` ~ b ~ d).eval("bd") should equal (false)
  }

  it should "not recognize strings not in the language 7" in {
    DerivativeMachine(!b ~ d).eval("bd") should equal (false)
  }

  behavior of "derive"

  it should "compute derivative correctly 1" in {
    DerivativeMachine(c.*).derive('c') should equal (c.*)
  }

  it should "compute derivative correctly 2" in {
    DerivativeMachine(b ~ c ~ d.*).derive('b') should equal (c ~ d.*)
  }

  it should "compute derivative correctly 3" in {
    DerivativeMachine((c | d) ~ (c | b).*).derive('c') should equal ((c | b).*)
  }

  it should "compute derivative correctly 4" in {
    DerivativeMachine((c.+ | d.*) ~ b).derive('c') should equal (c.* ~ b)
  }

  it should "compute derivative correctly 5" in {
    DerivativeMachine(`ε`).derive('c') should equal (`∅`)
  }

  it should "compute derivative correctly 6" in {
    DerivativeMachine(`ε` ~ (!c.* | d.+)).derive('d') should equal (!`∅` | d.*)
  }

  it should "compute derivative correctly 7" in {
    DerivativeMachine(((b^2) ~ c.*) & (b.* ~ c)).derive('b') should equal ((b ~ c.*) & (b.* ~ c))
  }

}
