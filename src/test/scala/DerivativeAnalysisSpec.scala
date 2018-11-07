package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.util._
import edu.ucsb.cs.cs162.range_set._

class DerivativeAnalysisSpec extends FlatSpec with Matchers with Timeout {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  // The timeout in milliseconds for potentially slow code.
  val timeout = 2000

  // Analyze the given expression subject to a timeout.
  def analyzeWithTimeout(re: Regex) =
    timeoutAfter(timeout) { DerivativeAnalysis.analyze(re) }

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------
  val Σ = α.chars
  val charA = Chars('a')
  val charB = Chars('b')
  val charC = Chars('c')
  val aSet = CharSet('a')
  val bSet = CharSet('b')
  val cSet = CharSet('c')
  val abSet = CharSet('a', 'b')
  val acSet = CharSet('a', 'c')
  val bcSet = CharSet('b', 'c')
  val abcSet = CharSet('a', 'b', 'c')

  behavior of "the analysis"

  it should "should always terminate 1" in {
    // Causes a timeout or stack overflow if expression similarity isn't
    // implemented correctly.
    val dfa = analyzeWithTimeout((charA | (charA ~ charA)).*)
  }

  it should "should always terminate 2" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.
    val dfa = analyzeWithTimeout((((charA).+)^2).*)
  }

  it should "should always terminate 3" in {
    val dfa = analyzeWithTimeout((((Chars(abcSet) & Chars(abSet))) | charA<>(1,5)).*)
  }

  it should "should always terminate 4" in {
    val dfa = analyzeWithTimeout((((charA^2) | ((charA^2) ~ charA)).*).*)
  }

  it should "should always terminate 5" in {
    val dfa = analyzeWithTimeout(((charA | charB).*)>=(3))
  }

  it should "should always terminate 6" in {
    val dfa = analyzeWithTimeout(((!charA | charB).+) & ((charA | !charB).*))
  }

  it should "should always terminate 7" in {
    val dfa = analyzeWithTimeout((!((charC.?).*) | (charC.* ~ charA.*).+).*)
  }

  it should "should always terminate 8" in {
    val dfa = analyzeWithTimeout(((charA | charB).*).+)
  }

  it should "produce a DFA that recognizes the strings in language 1" in {
    val dfa = analyzeWithTimeout(ε | charA)

    dfa.matches("") should equal (true)
    dfa.matches("a") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 2" in {
    val dfa = analyzeWithTimeout(charA.* ~ charB.*)

    dfa.matches("") should equal (true)
    dfa.matches("aaaa") should equal (true)
    dfa.matches("bbb") should equal (true)
    dfa.matches("aabbbb") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 3" in {
    val dfa = analyzeWithTimeout((charA | (charB | charC)).* & (charA ~ (charB.+ | charC).+))

    dfa.matches("ab") should equal (true)
    dfa.matches("ac") should equal (true)
    dfa.matches("abbccbb") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 4" in {
    val dfa = analyzeWithTimeout(charA.* ~ (charB.+ | charC).+)

    dfa.matches("abbbbb") should equal (true)
    dfa.matches("aaaaac") should equal (true)
    dfa.matches("abbcbbcbbbbb") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 5" in {
    val dfa = analyzeWithTimeout(!(charB.+ | charC)^2)

    dfa.matches("cccaaa") should equal (true)
    dfa.matches("a") should equal (true)
    dfa.matches("bbbbcbbb") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 6" in {
    val dfa = analyzeWithTimeout(((charB.+ ~ charC)^2) & ((charB^3) ~ (charC | charB^4) ~ (charB | charC) ~ charB.* ~ charC))

    dfa.matches("bbbcbc") should equal (true)
    dfa.matches("bbbbbbbcbbc") should equal (true)
    dfa.matches("bbbcbbbbbbbbbbbbc") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 7" in {
    val dfa = analyzeWithTimeout(ε ~ charC.? ~ ε)

    dfa.matches("c") should equal (true)
    dfa.matches("") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 8" in {
    val dfa = analyzeWithTimeout(!(charC.* ~ charA) & (charB ~ (charC.? | charB)))

    dfa.matches("b") should equal (true)
    dfa.matches("bb") should equal (true)
    dfa.matches("bc") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 9" in {
    val dfa = analyzeWithTimeout(charA.+ ~ (charB<>(2,3) | charA^2).*)

    dfa.matches("aa") should equal (true)
    dfa.matches("abbbaabbaaaaaaaa") should equal (true)
    dfa.matches("aaaaaaaabbbbbbbbbbaa") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 10" in {
    val dfa = analyzeWithTimeout(!charB)

    dfa.matches("aa") should equal (true)
    dfa.matches("abbbaabbaaaaaaaa") should equal (true)
    dfa.matches("") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 11" in {
    val dfa = analyzeWithTimeout(((charA | ε) ~ !(charB | ε)) & !charC)

    dfa.matches("ac") should equal (true)
    dfa.matches("abbbbbc") should equal (true)
    dfa.matches("a") should equal (true)
  }

  it should "produce a DFA that should not recognize strings not in the language 1" in {
    val dfa = analyzeWithTimeout(ε | charA)

    dfa.matches("b") should equal (false)
    dfa.matches("aa") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 2" in {
    val dfa = analyzeWithTimeout(!(charA | ε))

    dfa.matches("a") should equal (false)
    dfa.matches("") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 3" in {
    val dfa = analyzeWithTimeout(charA.*)

    dfa.matches("b") should equal (false)
    dfa.matches("bca") should equal (false)
    dfa.matches("aaaaaab") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 4" in {
    val dfa = analyzeWithTimeout(charB.+ | (charA ~ charC))

    dfa.matches("") should equal (false)
    dfa.matches("a") should equal (false)
    dfa.matches("caaaa") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 5" in {
    val dfa = analyzeWithTimeout(charB.* | (((charA ~ charB)^2) ~ charC))

    dfa.matches("abab") should equal (false)
    dfa.matches("bbba") should equal (false)
    dfa.matches("c") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 6" in {
    val dfa = analyzeWithTimeout(ε | (charC.* | (charB ~ charA)^2)<>(3,5))

    dfa.matches("a") should equal (false)
    dfa.matches("babaccbabaccbabaccbaba") should equal (false)
    dfa.matches("bb") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 7" in {
    val dfa = analyzeWithTimeout((charA.+ ~ charB) & ((charA^2) ~ charB))

    dfa.matches("ab") should equal (false)
    dfa.matches("aaab") should equal (false)
    dfa.matches("") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 8" in {
    val dfa = analyzeWithTimeout((charA ~ charB).*)

    dfa.matches("abb") should equal (false)
    dfa.matches("aaab") should equal (false)
    dfa.matches("c") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 9" in {
    val dfa = analyzeWithTimeout(!(charA ~ charB).*)

    dfa.matches("") should equal (false)
    dfa.matches("ab") should equal (false)
    dfa.matches("abababab") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 10" in {
    val dfa = analyzeWithTimeout(!charC.+ ~ charA)

    dfa.matches("ca") should equal (false)
    dfa.matches("cca") should equal (false)
    dfa.matches("ccccccccca") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 11" in {
    val dfa = analyzeWithTimeout((charA ~ charB^2)<=(2) & !((charB | charC).*))

    dfa.matches("") should equal (false)
    dfa.matches("abb") should equal (false)
    dfa.matches("cccca") should equal (false)
  }

  it should "produce a DFA that has the correct structure 1" in {
    val re = charA ~ charB
    val dfa = DerivativeAnalysis.analyze(re)
    val dfaStates = Seq(re, charB, ε, ∅)

    dfa.init should equal (re)

    dfa.fin should equal (Set[Regex](ε))

    dfa.delta.keys should contain theSameElementsAs(dfaStates)

    dfa.delta(re) should contain theSameElementsAs Seq((!aSet, ∅), (aSet, charB))
    dfa.delta(charB) should contain theSameElementsAs Seq((!bSet, ∅), (bSet, ε))
    dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }

  it should "produce a DFA that has the correct structure 2" in {
    val re = charA | charB
    val dfa = DerivativeAnalysis.analyze(re)
    val dfaStates = Seq(re, ε, ∅)

    dfa.init should equal (re)

    dfa.fin should equal (Set[Regex](ε))

    dfa.delta.keys should contain theSameElementsAs(dfaStates)

    dfa.delta(re) should contain theSameElementsAs Seq((abSet, ε), (!abSet, ∅))
    dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }

  it should "produce a DFA that has the correct structure 3" in {
    val re = charA.*
    val dfa = DerivativeAnalysis.analyze(re)
    val dfaStates = Seq(re, ∅)

    dfa.init should equal (re)

    dfa.fin should equal (Set[Regex](charA.*))

    dfa.delta.keys should contain theSameElementsAs(dfaStates)

    dfa.delta(re) should contain theSameElementsAs Seq((!aSet, ∅), (aSet, re))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }

  it should "produce a DFA that has the correct structure 4" in {
    val re = !charA
    val dfa = DerivativeAnalysis.analyze(re)
    val dfaStates = Seq(re, α.*, α.+)

    dfa.init should equal (re)

    dfa.fin should equal (Set[Regex](re, α.*))

    dfa.delta.keys should contain theSameElementsAs(dfaStates)

    dfa.delta(re) should contain theSameElementsAs Seq((!aSet, α.*), (aSet, α.+))
    dfa.delta(α.*) should contain theSameElementsAs Seq((Σ, α.*))
    dfa.delta(α.+) should contain theSameElementsAs Seq((Σ, α.*))
  }

  it should "produce a DFA that has the correct structure 5" in {
    val re = charA & charB
    val dfa = DerivativeAnalysis.analyze(re)
    val dfaStates = Seq(re)

    dfa.init should equal (re)

    dfa.fin should equal (Set[Regex]())

    dfa.delta.keys should contain theSameElementsAs(dfaStates)

    dfa.delta(re) should contain theSameElementsAs Seq((Σ, re))
  }

  it should "produce a DFA that has the correct structure 6" in {
    val re = charA
    val dfa = DerivativeAnalysis.analyze(re)
    val dfaStates = Seq(re, ε, ∅)

    dfa.init should equal (re)

    dfa.fin should equal (Set[Regex](ε))

    dfa.delta.keys should contain theSameElementsAs(dfaStates)

    dfa.delta(re) should contain theSameElementsAs Seq((!aSet, ∅), (aSet, ε))
    dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }

  it should "produce a DFA that has the correct structure 7" in {
    val re = ε
    val dfa = DerivativeAnalysis.analyze(re)
    val dfaStates = Seq(re, ∅)

    dfa.init should equal (re)

    dfa.fin should equal (Set[Regex](ε))

    dfa.delta.keys should contain theSameElementsAs(dfaStates)

    dfa.delta(re) should contain theSameElementsAs Seq((Σ, ∅))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }

  it should "produce a DFA that has the correct structure 8" in {
    val re = ((charA.* | (charB.* ~ charC.+).*) ~ charB ~ charA)
    val dfa = DerivativeAnalysis.analyze(re)

    val partialState0 = charB ~ charA
    val partialState1 = (charB.* ~ charC.+)

    val cState0 = charA.* ~ partialState0
    val cState1 = charA | (partialState1.+ ~ partialState0)
    val cState2 = partialState1.+ ~ partialState0
    val cState3 = charC.* ~ partialState1.* ~ partialState0
    val dfaStates = Seq(re, cState0, cState1, cState2, cState3, charA, ε, ∅)

    dfa.init should equal (re)

    dfa.fin should equal (Set[Regex](ε))

    dfa.delta.keys should contain theSameElementsAs(dfaStates)

    dfa.delta(re) should contain theSameElementsAs Seq((aSet, cState0), (bSet, cState1), (cSet, cState3), (!abcSet, ∅))

    dfa.delta(cState0) should contain theSameElementsAs Seq((aSet, cState0), (bSet, charA), (!abSet, ∅))

    dfa.delta(cState1) should contain theSameElementsAs Seq((aSet, ε), (bSet, cState2), (cSet, cState3), (!abcSet, ∅))

    dfa.delta(cState2) should contain theSameElementsAs Seq((bSet, cState2), (cSet, cState3), (!bcSet, ∅))

    dfa.delta(cState3) should contain theSameElementsAs Seq((bSet, cState1), (cSet, cState3), (!bcSet, ∅))

    dfa.delta(charA) should contain theSameElementsAs Seq((aSet, ε), (!aSet, ∅))

    dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))

    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }
}
