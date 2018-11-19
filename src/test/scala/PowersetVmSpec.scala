package edu.ucsb.cs.cs162.regex.vm

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class PowersetVmSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "eval"

  // Replace {language 1} with a more descriptive name for what you're testing.
  // Feel free to add more tests, or write many shorter ones.

  it should "parse strings in {language 1}" in {
    import compiler._
    import parse_tree._

    val username = (Chars('a'->'z').+).capture("username")

    val date = ((Chars('0'->'9') <= 2).capture("day") ~ Chars('/', '-') ~
                  (Chars('0'->'9') <= 2).capture("month") ~ Chars('/', '-') ~
                  (Chars('0'->'9') ^ 4).capture("year")).capture("date")

    val row = username ~ Chars(',') ~ date

    val prog = Compiler.compile(row)
    println(prog)
    val tree = (new PowersetVm(prog)).eval("tom,25/5/2002").get
    val extractor = (new Extractor(tree))
    extractor.extract("username") shouldEqual List("tom")
  }

  // more tests...

  it should "not parse strings not in {language 1}" in { pending }

  // more tests...
}
