package edu.ucsb.cs.cs162.dfa

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex.derivative.DerivativeAnalysis

class DfaSpec extends FlatSpec with Matchers with OptionValues {
  import Regex._

  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')
  val e = Chars('e')
  val bSet = CharSet('b')
  val cSet = CharSet('c')
  val dSet = CharSet('d')

  behavior of "Dfa.getString"

  // Test DFA with 1 non accepting state and no accepting states
  it should "return None if the DFA's language is empty 1" in {
    val δ: Transitions[Regex] = Map(∅ → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, ∅, Set.empty)
    dfa.getString shouldEqual None
  }

  // Test DFA with multiple non accepting states and no accepting states
  it should "return None if the DFA's language is empty 2" in {
    val δ: Transitions[Regex] = Map(
      ∅ → Seq(!CharSet() → ∅),
      b → Seq(!cSet → ∅, cSet → c),
      c → Seq(!dSet → ∅, dSet → d),
      d → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, b, Set.empty)
    dfa.getString shouldEqual None
  }

  // Test poorly formed DFA with multiple non accepting states
  // and 1 accepting state which cannot be reached
  it should "return None if the DFA's language is empty 3" in {
    val δ: Transitions[Regex] = Map(
      ∅ → Seq(!CharSet() → ∅),
      b → Seq(!cSet → ∅, cSet → c),
      c → Seq(!dSet → ∅, dSet → d),
      d → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, b, Set(b.*))
    dfa.getString shouldEqual None
  }

  // Test nullset regex
  it should "return None if the DFA's language is empty 4" in {
    val dfa = DerivativeAnalysis.analyze(∅)
    dfa.getString shouldEqual None
  }

  // Test Concatenate with ∅ regex
  it should "return None if the DFA's language is empty 5" in {
    val dfa = DerivativeAnalysis.analyze(Concatenate(b, ∅))
    dfa.getString shouldEqual None
  }

  // Test Intersect with ∅ regex
  it should "return None if the DFA's language is empty 6" in {
    val dfa = DerivativeAnalysis.analyze(Intersect(b, ∅))
    dfa.getString shouldEqual None
  }

  // Test Complement of regex which accepts all strings
  it should "return None if the DFA's language is empty 7" in {
    val dfa = DerivativeAnalysis.analyze(Complement(KleeneStar(α)))
    dfa.getString shouldEqual None
  }

  // Test complex regex 1
  it should "return None if the DFA's language is empty 8" in {
    val dfa = DerivativeAnalysis.analyze(
      Concatenate(
        Union(
          KleeneStar(Union(d, c)),
          Concatenate(Union(d, ε), ε)),
        ∅))
    dfa.getString shouldEqual None
  }

  // Test complex regex 2
  it should "return None if the DFA's language is empty 9" in {
    val dfa = DerivativeAnalysis.analyze(
      Concatenate(
        KleeneStar(
          KleeneStar(Union(d, c))),
        Concatenate(
          KleeneStar(Union(ε, Concatenate(c, d))),
          Intersect(b, c))))
    dfa.getString shouldEqual None
  }

  it should "return None if the DFA's language is empty 10" in {
    val δ: Transitions[Regex] = Map()
    val dfa = Dfa(δ, ∅, Set.empty)
    dfa.getString shouldEqual None
  }


  // Test empty string
  it should "return a string that the DFA recognizes if the DFA's language is not empty 1" in {
    val δ: Transitions[Regex] = Map(ε → Seq(!CharSet() → ∅), ∅ → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, ε, Set[Regex](ε))
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  // Test Chars
  it should "return a string that the DFA recognizes if the DFA's language is not empty 2" in {
    val dfa = DerivativeAnalysis.analyze(c)
    val s = dfa.getString.value
    s should equal ("c")
    dfa matches s shouldEqual true
  }

  // Test Concatenate
  it should "return a string that the DFA recognizes if the DFA's language is not empty 3" in {
    val dfa = DerivativeAnalysis.analyze(Concatenate(b, c))
    val s = dfa.getString.value
    s should equal ("bc")
    dfa matches s shouldEqual true
  }

  // Test Union
  it should "return a string that the DFA recognizes if the DFA's language is not empty 4" in {
    val dfa = DerivativeAnalysis.analyze(Union(b, c))
    val s = dfa.getString.value
    Set("b", "c") should contain (s)
    dfa matches s shouldEqual true
  }

  // Test KleeneStar
  it should "return a string that the DFA recognizes if the DFA's language is not empty 5" in {
    val dfa = DerivativeAnalysis.analyze(KleeneStar(d))
    val s = dfa.getString.value
    s should equal ("")
    dfa matches s shouldEqual true
  }

  // Test Complement
  it should "return a string that the DFA recognizes if the DFA's language is not empty 6" in {
    val dfa = DerivativeAnalysis.analyze(Complement(d))
    val s = dfa.getString.value
    s should not equal ("d")
    dfa matches s shouldEqual true
  }

  // Test Intersect
  it should "return a string that the DFA recognizes if the DFA's language is not empty 7" in {
    val dfa = DerivativeAnalysis.analyze(Intersect(Chars('b', 'c'), Chars('c', 'd')))
    val s = dfa.getString.value
    s should equal ("c")
    dfa matches s shouldEqual true
  }

  // Complex case 1
  it should "return a string that the DFA recognizes if the DFA's language is not empty 8" in {
    val dfa = DerivativeAnalysis.analyze(
      Union(
        b,
        Concatenate(
          b,
          KleeneStar(Union(b, c)))))
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  // Complex case 2
  it should "return a string that the DFA recognizes if the DFA's language is not empty 9" in {
    val dfa = DerivativeAnalysis.analyze(
      KleeneStar(
        Concatenate(
          Concatenate(
            Union(
              Union(c, d),
              KleeneStar(d)),
            KleeneStar(Concatenate(c, b))),
          KleeneStar(Union(c, ε)))))
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  // Test poorly formed DFA with a state which isn't in transitions
  // that is both the start start and an accepting state
  it should "return a string that the DFA recognizes if the DFA's language is not empty 10" in {
    val δ: Transitions[Regex] = Map(ε → Seq(!CharSet() → ∅), ∅ → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, b, Set[Regex](b))
    val s = dfa.getString.value
    s should equal ("")
    dfa matches s shouldEqual true
  }
}
