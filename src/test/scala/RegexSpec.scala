package edu.ucsb.cs.cs162.regex

import org.scalatest._
import edu.ucsb.cs.cs162.regex.derivative._

class RegexSpec extends FlatSpec with Matchers with OptionValues {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val charA = Chars('a')
  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')
  val e = Chars('e')
  val f = Chars('f')

  val r = Chars('a') | Chars('b').+
  val r1 = Chars('x', 'y').* ~ r
  val r2 = Chars('y', 'x').+ ~ r
  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "a regex"

  it should "be buildable using `~`" in {
    (r1 ~ r2) should equal (Chars('x', 'y').* ~ r ~ Chars('y', 'x').+ ~ r)
    // simplifications
    (r ~ ∅) should equal(∅)
    (∅ ~ r) should equal(∅)
    (r ~ ε) should equal(r)
    (ε ~ r) should equal(r)
  }


  it should "be buildable using `|`" in {
    (r1 | r2) should equal(Union(r2, r1)) // also testing normalization due to lengths of r1 and r2
    // simplifications
    (r | ∅) should equal(r)
    (∅ | r) should equal(r)
    (Chars('a' -> 'c') | Chars('c' -> 'f')) should equal(Chars('a'->'f'))
    (r.* |   ε) should equal(r.*)
    (ε   | r.*) should equal(r.*)
    (α.* |   r) should equal(α.*)
    (r |   α.*) should equal(α.*)
    (r | r)     should equal(r)
  }

  it should "be buildable using `*`" in {
    r.* should equal(KleeneStar(r))
    // simplifications
    ∅.* should equal(ε)
    ε.* should equal(ε)
    (r.*).* should equal(r.*)
  }

  it should "be buildable using `!`" in {
    !r should equal(Complement(r))
    // Simplifications
    !(!r) should equal(r)
    !(∅) should equal(α.*)
    !ε should equal(α.+)
  }

  it should "be buildable using `&`" in {
    (r1 & r2) should equal(Intersect(r2, r1)) // also testing normalization due to lengths of r1 and r2
    // Simplifications
    (∅ & r) should equal(∅)
    (r & ∅) should equal(∅)
    (Chars('a'->'d') & Chars('c'->'f')) should equal (Chars('c'->'d'))
    (α.* & r) should equal(r)
    (r & α.*) should equal(r)
    (r & r) should equal(r)
  }

  it should "be buildable using `^`" in {
    (r^5) should equal(r ~ r ~ r ~ r ~ r)
  }

  it should "be buildable using `>=`" in {
    (r >= 3) should equal(r ~ r ~ r ~ r.*)
  }

  it should "be buildable using `<=`" in {
    (r <= 3) should equal(ε | r | (r ~ r) | (r ~ r ~ r))
  }

  it should "be buildable using `<>`" in {
    (r <>(2, 3)) should equal((r ~ r ~ r.*) & (ε | r | (r ~ r) | (r ~ r ~ r)))
  }


  it should "be buildable using convenience methods 1" in {
    (b ~ c) should equal (Concatenate(b, c))
  }

  it should "be buildable using convenience methods 2" in {
    (b | (b ~ c)) should equal (Union(b, Concatenate(b, c)))
  }

  it should "be buildable using convenience methods 3" in {
    b.* should equal (KleeneStar(b))
  }

  it should "be buildable using convenience methods 4" in {
    !b should equal (Complement(b))
  }

  it should "be buildable using convenience methods 5" in {
    (b & (b ~ c)) should equal (Intersect(b, Concatenate(b, c)))
  }

  it should "be buildable using convenience methods 6" in {
    b.+ should equal (Concatenate(b, KleeneStar(b)))
  }

  it should "be buildable using convenience methods 7" in {
    b.? should equal (Union(ε, b))
  }

  it should "be buildable using convenience methods 8" in {
    b^3 should equal (Concatenate(b, Concatenate(b, b)))
  }

  it should "be buildable using convenience methods 9" in {
    (b >= 2) should equal (Concatenate(b, Concatenate(b, KleeneStar(b))))
  }

  it should "be buildable using convenience methods 10" in {
    (b <= 2) should equal (Union(ε, Union(b, Concatenate(b, b))))
  }

  it should "be buildable using convenience methods 11" in {
    (b <> (1, 3)) should equal (Intersect(Concatenate(b, KleeneStar(b)), Union(ε, Union(b, Union(Concatenate(b, b), Concatenate(b, Concatenate(b, b)))))))
  }

  it should "be buildable from strings" in {
    "ab".charset ~ "cd".concatenate should equal (Concatenate(Chars('a', 'b'),
      Concatenate(Chars('c'), Chars('d'))))
  }

  it should "pretty-print correctly" in {
    (b.? | (c >= 1)).prettyPrint should equal ("""Union
                                                 |├─ ε
                                                 |└─ Union
                                                 |   ├─ b
                                                 |   └─ Concatenate
                                                 |      ├─ c
                                                 |      └─ KleeneStar
                                                 |         └─ c
                                                 |""".stripMargin)
  }

  it should "normalize correctly 1" in {
    val re = ((charA ~ b) ~ (c ~ d)) ~ (e ~ f)

    val norm = Concatenate(charA, Concatenate(b, Concatenate(c,
      Concatenate(d, Concatenate(e, f)))))

    re should equal (norm)
  }

  it should "normalize correctly 2" in {
    val re = (((b | ε) & charA) | !charA | charA.*) | ((charA ~ b) |
      charA | ε)

    val norm = Union(ε, Union(charA, Union(Concatenate(charA, b),
      Union(KleeneStar(charA), Union(Complement(charA), Intersect(charA,
        Union(ε, b)))))))

    re should equal (norm)
  }

  it should "normalize correctly 3" in {
    ((b^2) ~ b) should equal (Concatenate(b, b^2))
  }

  it should "normalize correctly 4" in {
    ((b ~ c) ~ (c ~ d)) should equal (Concatenate(b, Concatenate(c, Concatenate(c, d))))
  }

  it should "normalize correctly 5" in {
    ((b | c.*) | d.*) should equal (Union(b, Union(c.*, d.*)))
  }

  it should "normalize correctly 6" in {
    ((b | c.*) | (b.* | d)) should equal(Union(b, Union(d, Union(b.*, c.*))))
  }

  it should "normalize correctly 7" in {
    ((c ~ d) & (c.* ~ d.*) & (c.* ~ d.?)) should equal (Intersect((c ~ d), Intersect((c.* ~ d.?), (c.* ~ d.*))))
  }

  it should "normalize correctly 8" in {
    ((!(b ~ c) & ((c.* ~ d.*) & (((c^2) ~ ((d.?)^3)) & ((c^2) ~ d))))) should equal (Intersect((c^2) ~ d, Intersect((c^2) ~ ((d.?)^3), Intersect(c.* ~ d.*, !(b ~ c)))))
  }

  it should "normalize correctly 9" in {
    (((c^3) ~ c.*) & ((!c & !b).* ~ Chars('b' -> 'c')) | (ε ~ b.*)) should equal (Union(b.*, Intersect(Concatenate(c, Concatenate(c, Concatenate(c, c.*))), Concatenate(Intersect(!b, !c).*, Chars('b' -> 'c')))))
  }

  behavior of "nullable"

  it should "recognize a nullable regex 1" in {
    `ε`.nullable should equal (`ε`)
  }

  it should "recognize a nullable regex 2" in {
    c.*.nullable should equal (`ε`)
  }

  it should "recognize a nullable regex 3" in {
    (c.* ~ d.*).nullable should equal (`ε`)
  }

  it should "recognize a nullable regex 4" in {
    (`ε` ~ d.*).nullable should equal (`ε`)
  }

  it should "recognize a nullable regex 5" in {
    (`ε` | b).nullable should equal (`ε`)
  }

  it should "recognize a nullable regex 6" in {
    (!b).nullable should equal (`ε`)
  }

  it should "recognize a nullable regex 7" in {
    (`ε` & d.*).nullable should equal (`ε`)
  }

  it should "recognize a non-nullable regex 1" in {
    `∅`.nullable should equal (`∅`)
  }

  it should "recognize a non-nullable regex 2" in {
    (!c.*).nullable should equal (`∅`)
  }

<<<<<<< HEAD
  it should "recognize a non-nullable regex 3" in {
    (c.* ~ `∅`).nullable should equal (`∅`)
  }

  it should "recognize a non-nullable regex 4" in {
    (b.+).nullable should equal (`∅`)
  }

  it should "recognize a non-nullable regex 5" in {
    (c>=6 | d>=1).nullable should equal (`∅`)
  }

  it should "recognize a non-nullable regex 6" in {
    (c<= 2 & c>=3).nullable should equal (`∅`)
  }

  it should "recognize a non-nullable regex 7" in {
    (`ε` ~ `∅`).nullable should equal (`∅`)
  }

  behavior of "ambiguity type checker"

  it should "find the ambiguous subexpression and a witness string in an ambiguous regex" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = a ~ (b | ε) ~ (b | ε)
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal ((b | ε) ~ (b | ε))
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  // more tests...

  it should "return None if the string is unambiguous" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = a ~ (b | ε)
    r.unambiguous shouldEqual None
  }

  // more tests...
}
