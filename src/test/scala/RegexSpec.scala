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

  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 1" in {
    val a = Chars('a')
    val b = Chars('b')
    val expr =(b | ε)
    val ambRegex = expr ~ expr
    val r = a ~ ambRegex
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    new DerivativeMachine(expr.overlap(expr)).eval(witness) shouldEqual true
  }

  // Ambiguous because one of the inner expressions of Union is ambiguous
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 2" in {
    val innerExpr = c.?
    val ambRegex = KleeneStar(innerExpr)
    val r1 = Union(d, ambRegex)
    val r2 = Union(ambRegex, b ~ c)
    val (ambiguousSubexpr1, witness1) = r1.unambiguous.value
    val (ambiguousSubexpr2, witness2) = r2.unambiguous.value
    ambiguousSubexpr1 should equal (ambRegex)
    ambiguousSubexpr2 should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr1).eval(witness1) shouldEqual true
    new DerivativeMachine(ambiguousSubexpr2).eval(witness2) shouldEqual true
    // Use ambRegex instead of innerExpr.overlap(ambRegex) because there is
    // no overlap but innerExpr is nullable
    new DerivativeMachine(ambRegex).eval(witness1) shouldEqual true
    new DerivativeMachine(ambRegex).eval(witness2) shouldEqual true
  }

  // Ambiguous because left and right inner expressions of Union intersect
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 3" in {
    val ambRegex = Union(c, c)
    val r = ambRegex
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    new DerivativeMachine(c & c).eval(witness) shouldEqual true
  }

  // Ambiguous because one of the inner expressions of Concatenate is ambiguous
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 4" in {
    val ambRegex = Union(d, d)
    val r1 = Concatenate(c, ambRegex)
    val r2 = Concatenate(ambRegex, d ~ b)
    val (ambiguousSubexpr1, witness1) = r1.unambiguous.value
    val (ambiguousSubexpr2, witness2) = r2.unambiguous.value
    ambiguousSubexpr1 should equal (ambRegex)
    ambiguousSubexpr2 should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr1).eval(witness1) shouldEqual true
    new DerivativeMachine(ambiguousSubexpr2).eval(witness2) shouldEqual true
    new DerivativeMachine(d & d).eval(witness1) shouldEqual true
    new DerivativeMachine(d & d).eval(witness2) shouldEqual true
  }

  // Ambiguous because left and right inner expressions of Concatenate overlap
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 5" in {
    val left = e.?
    val right = (e | b).*
    val ambRegex = Concatenate(left, right)
    val r = ambRegex
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    new DerivativeMachine(left.overlap(right)).eval(witness) shouldEqual true
  }

  // Ambiguous because inner expression of KleeneStar is ambiguous
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 6" in {
    val unionInnerExpr = (b ~ c.?)
    val ambRegex = Union(unionInnerExpr, unionInnerExpr)
    val r = KleeneStar(ambRegex)
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    new DerivativeMachine(unionInnerExpr & unionInnerExpr).eval(witness) shouldEqual true
  }

  // Ambiguous because inner expression of KleeneStar is nullable but doesn't overlap
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 7" in {
    val innerExpr = ε
    val ambRegex = KleeneStar(innerExpr)
    val r = ambRegex
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    r.a.nullable should equal (ε)
    ((r.a overlap r).empty) should equal (true)
    ambiguousSubexpr should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    // Use ambRegex instead of innerExpr.overlap(ambRegex) because
    // innerExpr is nullable and no overlap
    new DerivativeMachine(ambRegex).eval(witness) shouldEqual true
  }

  // Ambiguous because inner expression of KleeneStar overlaps but is not nullable
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 8" in {
    val innerExpr = Union(b ~ b, b)
    val ambRegex = KleeneStar(innerExpr)
    val r = ambRegex
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ((r.a overlap r).empty) should equal (false)
    r.a.nullable should equal (∅)
    ambiguousSubexpr should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    new DerivativeMachine(innerExpr.overlap(ambRegex)).eval(witness) shouldEqual true
  }

  // Ambiguous because inner expression of KleeneStar overlaps and is nullable
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 9" in {
    val innerExpr = Union(c ~ c, c).?
    val ambRegex = KleeneStar(innerExpr)
    val r = ambRegex
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ((r.a overlap r).empty) should equal (false)
    r.a.nullable should equal (ε)
    ambiguousSubexpr should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    new DerivativeMachine(innerExpr.overlap(ambRegex)).eval(witness) shouldEqual true
  }

  // Ambiguous because expression inside Capture is ambiguous
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 10" in {
    val concatInnerExpr = c.?
    val ambRegex = Concatenate(concatInnerExpr, concatInnerExpr)
    val r = Capture("Test", ambRegex)
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    new DerivativeMachine(concatInnerExpr.overlap(concatInnerExpr)).eval(witness) shouldEqual true
  }

  // Ambiguous subexpression should be "first" one (left expression of Union)
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 11" in {
    val ambRegex1InnerLeft = d
    val ambRegex1InnerRight = d.?
    val ambRegex1 = Union(ambRegex1InnerLeft, ambRegex1InnerRight)
    val ambRegex2 = Concatenate(e.?, e.?)
    val r = KleeneStar(Union(ambRegex1, ambRegex2))
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal (ambRegex1)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    new DerivativeMachine(ambRegex1InnerLeft & ambRegex1InnerRight).eval(witness) shouldEqual true
  }

  // Ambiguous subexpression found should be "first" one (left expression of Concatenate)
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 12" in {
    val ambRegex1 = Union(d, d.?)
    val ambRegex2 = Concatenate(e.?, e.?)
    val r = KleeneStar(Concatenate(ambRegex1, ambRegex2))
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal (ambRegex1)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  // Ambiguous subexpression found should be "first" one (left expression of innermost Union)
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 13" in {
    val ambRegex1InnerExpr = d.?
    val ambRegex1 = KleeneStar(ambRegex1InnerExpr)
    val ambRegex2 = Union(e.?, e.?)
    val ambRegex3 = Concatenate(f.?, f.*)
    val r = KleeneStar(
      Union(
        KleeneStar(b | e),
        Union(
          ambRegex1,
          Concatenate(ambRegex2, ambRegex3))))
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal (ambRegex1)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    new DerivativeMachine(ambRegex1InnerExpr.overlap(ambRegex1)).eval(witness) shouldEqual true
  }

  // Very complex regex ambiguity test
  it should "find the ambiguous subexpression and a witness string in an ambiguous regex 14" in {
    val innerExpr = Union(b, b ~ b)
    val ambRegex = KleeneStar(innerExpr)
    val r = Capture(
      "Test",
      Union(
        KleeneStar(
          Concatenate(b, d^2)),
        KleeneStar(
          Union(
            Concatenate(
              KleeneStar(
                Union(b, d)),
              e),
            Union(
              ambRegex,
              KleeneStar(Concatenate(e, c)))))))
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr should equal (ambRegex)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
    new DerivativeMachine(innerExpr.overlap(ambRegex)).eval(witness) shouldEqual true
  }

  it should "return None if the regex is unambiguous 1" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = a ~ (b | ε)
    r.unambiguous shouldEqual None
  }

  // ∅ base case
  it should "return None if the regex is unambiguous 2" in {
    val r = ∅
    r.unambiguous shouldEqual None
  }

  // ε base case
  it should "return None if the regex is unambiguous 3" in {
    val r = ε
    r.unambiguous shouldEqual None
  }

  // Chars base case
  it should "return None if the regex is unambiguous 4" in {
    val r = b
    r.unambiguous shouldEqual None
  }

  // Union case
  it should "return None if the regex is unambiguous 5" in {
    val r = Union(e ~ c^2, d)
    r.unambiguous shouldEqual None
  }

  // Concatenate case
  it should "return None if the regex is unambiguous 6" in {
    val r = Concatenate(c^2 | d^2, e^5)
    r.unambiguous shouldEqual None
  }

  // KleeneStar case
  it should "return None if the regex is unambiguous 7" in {
    val r = KleeneStar(Union(b, ((d ~ e^2) ~ f^2)))
    r.unambiguous shouldEqual None
  }

  // Capture case
  it should "return None if the regex is unambiguous 8" in {
    val r = Capture("Test", Concatenate(KleeneStar(Union(e, f)), b^5))
    r.unambiguous shouldEqual None
  }

  // Complex case
  it should "return None if the regex is unambiguous 9" in {
    val r = Capture("Test", Concatenate(
                      KleeneStar(Union(d, c)),
                      Concatenate(
                        Union(
                          Concatenate(
                             (Concatenate(ε, f^2)),
                             f.?),
                          Concatenate(
                            c,
                            (Union(b, d)))),
                        Union(
                          Concatenate(b, c),
                          Concatenate(
                            KleeneStar(c),
                            d ~ f ~ e)))))
    r.unambiguous shouldEqual None
  }

  it should "throw an assertion error when encountering Intersect" in {
    val r = Intersect(b, d)
    the [AssertionError] thrownBy r.unambiguous should have message "assertion failed: Do not handle Intersect"
  }

  it should "throw an assertion error when encountering Complement" in {
    val r = Complement(b)
    the [AssertionError] thrownBy r.unambiguous should have message "assertion failed: Do not handle Complement"
  }

}
