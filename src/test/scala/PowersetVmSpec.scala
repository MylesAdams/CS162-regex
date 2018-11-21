package edu.ucsb.cs.cs162.regex.vm

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class PowersetVmSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._
  import compiler.Compiler
  import parse_tree._

  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')
  val bSet = b.chars
  val cSet = c.chars
  val dSet = d.chars

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "eval"

  // Replace {language 1} with a more descriptive name for what you're testing.
  // Feel free to add more tests, or write many shorter ones.

  it should "parse strings in the language (ε)" in {
    val parseStr = ""

    val program = IndexedSeq(PushEmpty, Accept)

    val tree = (new PowersetVm(program)).eval(parseStr).get

    tree should equal (EmptyLeaf)
  }

  it should "parse strings in the language (b)" in {
    val parseStr = "b"

    val program = IndexedSeq(
      MatchSet(Chars('b').chars),
      PushChar,
      Accept)

    val tree = (new PowersetVm(program)).eval(parseStr).get

    tree should equal (CharLeaf('b'))
  }

  it should "parse strings in the language (b ~ c)" in {
    val parseStr = "bc"

    val program = IndexedSeq(
      MatchSet(bSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      PushConcat,
      Accept)

    val tree = (new PowersetVm(program)).eval(parseStr).get

    tree should equal (ConcatNode(CharLeaf('b'), CharLeaf('c')))
  }

  it should "parse strings in the language (Union(b, c))" in {
    val parseStr0 = "b"
    val parseStr1 = "c"

    val program = IndexedSeq(
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(cSet),
      PushChar,
      PushRight,
      Accept)

    val vm = new PowersetVm(program)

    val tree0 = vm.eval(parseStr0).get
    val tree1 = vm.eval(parseStr1).get

    tree0 should equal (LeftNode(CharLeaf('b')))
    tree1 should equal (RightNode(CharLeaf('c')))
  }

  it should "parse strings in the language (c.*)" in {
    val parseStr0 = ""
    val parseStr1 = "c"
    val parseStr2 = "ccccccccc"

    val program = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushStar,
      Jump(-4),
      Accept)

    val vm = new PowersetVm(program)

    val tree0 = vm.eval(parseStr0).get
    val tree1 = vm.eval(parseStr1).get
    val tree2 = vm.eval(parseStr2).get

    tree0 should equal(StarNode(Seq()))
    tree1 should equal(StarNode(Seq(CharLeaf('c'))))
    tree2 should equal(
      StarNode(
        Seq(
          CharLeaf('c'),
          CharLeaf('c'),
          CharLeaf('c'),
          CharLeaf('c'),
          CharLeaf('c'),
          CharLeaf('c'),
          CharLeaf('c'),
          CharLeaf('c'),
          CharLeaf('c'))))
  }

  it should "parse username, day, month, and year from piazza example (Capture)" in {

    val username = (Chars('a'->'z').+).capture("username")
    val date = ((Chars('0'->'9') <= 2).capture("day") ~ Chars('/', '-') ~
                  (Chars('0'->'9') <= 2).capture("month") ~ Chars('/', '-') ~
                  (Chars('0'->'9') ^ 4).capture("year")).capture("date")
    val row = username ~ Chars(',') ~ date

    val program = Compiler.compile(row)
    val tree = (new PowersetVm(program)).eval("tom,25/5/2002").get
    val extractor = (new Extractor(tree))

    extractor.extract("username") should equal (List("tom"))
    extractor.extract("date", "day") should equal (List("25"))
    extractor.extract("date", "month") should equal (List("5"))
    extractor.extract("date", "year") should equal (List("2002"))
  }


  it should "parse strings in the language ((b | c).* ~ (d^2))" in {
    val parseStr = "bbccbdd"

    val program = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(Chars('b','c').chars),
      PushChar,
      PushStar,
      Jump(-4),
      MatchSet(dSet),
      PushChar,
      MatchSet(dSet),
      PushChar,
      PushConcat,
      PushConcat,
      Accept)

    val tree = (new PowersetVm(program)).eval(parseStr).get

    tree should equal (ConcatNode(
                         StarNode(
                           Seq(
                             CharLeaf('b'),
                             CharLeaf('c'),
                             CharLeaf('c'),
                             CharLeaf('b'),
                             CharLeaf('b'))),
                         ConcatNode(
                           CharLeaf('d'),
                           CharLeaf('d'))))

  }

  it should "parse strings in the language (Union(ε, d.+)^3)" in {
    val parseStr0 = "d"
    val parseStr1 = ""
    val parseStr2 = "dddd"

    val program = IndexedSeq(
      Fork(1,4),
      PushEmpty,
      PushLeft,
      Jump(11),
      MatchSet(dSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushRight,
      Fork(1,4),
      PushEmpty,
      PushLeft,
      Jump(11),
      MatchSet(dSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushRight,
      Fork(1,4),
      PushEmpty,
      PushLeft,
      Jump(11),
      MatchSet(dSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushRight,
      PushConcat,
      PushConcat,
      Accept)

    val vm = new PowersetVm(program)

    val tree0 = vm.eval(parseStr0).get
    val tree1 = vm.eval(parseStr1).get
    val tree2 = vm.eval(parseStr2).get

    tree0 should equal(ConcatNode(
                         LeftNode(EmptyLeaf),
                         ConcatNode(
                           LeftNode(EmptyLeaf),
                           RightNode(
                             ConcatNode(
                               CharLeaf('d'),
                               StarNode(Seq()))))))

    tree1 should equal(ConcatNode(
                         LeftNode(EmptyLeaf),
                         ConcatNode(
                           LeftNode(EmptyLeaf),
                           LeftNode(EmptyLeaf))))

    tree2 should equal(ConcatNode(
                         LeftNode(EmptyLeaf),
                         ConcatNode(
                           LeftNode(EmptyLeaf),
                           RightNode(
                             ConcatNode(
                               CharLeaf('d'),
                               StarNode(
                                 Seq(
                                   CharLeaf('d'),
                                   CharLeaf('d'),
                                   CharLeaf('d'))))))))
  }

  it should "parse strings in the language ((Union((Union(b.* ~ c.*, b.+)).+, b.* ~ b.*)).*)" in {

    val parseStr0 = ""
    val parseStr1 = "cbcc"
    val parseStr2 = "bc"

    val program = IndexedSeq(
      InitStar,
      CheckProgress,
      Fork(1,78),
      Fork(1,61),
      Fork(1,16),
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      InitStar,
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushLeft,
      Jump(11),
      MatchSet(bSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushRight,
      InitStar,
      CheckProgress,
      Fork(1,29),
      Fork(1,16),
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      InitStar,
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushLeft,
      Jump(11),
      MatchSet(bSet),
      PushChar,
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushRight,
      PushStar,
      Jump(-29),
      PushConcat,
      PushLeft,
      Jump(15),
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      PushRight,
      PushStar,
      Jump(-78),
      Accept)

    val vm = new PowersetVm(program)

    val tree0 = vm.eval(parseStr0).get
    val tree1 = vm.eval(parseStr1).get
    val tree2 = vm.eval(parseStr2).get

    tree0 should equal (StarNode(Seq()))
    tree1 should equal (StarNode(
                          Seq(
                            LeftNode(
                              ConcatNode(
                                LeftNode(
                                  ConcatNode(
                                    StarNode(Seq()),
                                    StarNode(Seq(CharLeaf('c'))))),
                                StarNode(
                                  Seq(
                                    LeftNode(
                                      ConcatNode(
                                        StarNode(Seq(CharLeaf('b'))),
                                        StarNode(
                                          Seq(
                                            CharLeaf('c'),
                                            CharLeaf('c'))
                                        ))))))))))
    tree2 should equal (StarNode(
                          Seq(
                            LeftNode(
                              ConcatNode(
                                LeftNode(
                                  ConcatNode(
                                    StarNode(Seq(CharLeaf('b'))),
                                    StarNode(Seq(CharLeaf('c'))))),
                                StarNode(Seq()))))))
  }

  // more tests...

  it should "not parse strings not in the lanauge (∅)" in {
    val parseStr0 = "d"
    val parseStr1 = "cb"

    val program = IndexedSeq(Reject, Accept)

    val vm = new PowersetVm(program)

    val tree0 = vm.eval(parseStr0)
    val tree1 = vm.eval(parseStr1)

    tree0 should equal (None)
    tree1 should equal (None)
  }

  it should "not parse strings not in the language (ε)" in {
    val parseStr0 = "b"
    val parseStr1 = "cc"

    val program = IndexedSeq(PushEmpty, Accept)

    val vm = new PowersetVm(program)

    val tree0 = vm.eval(parseStr0)
    val tree1 = vm.eval(parseStr1)

    tree0 should equal (None)
    tree1 should equal (None)
  }

  it should "not parse strings not in the language (d)" in {
    val parseStr0 = "c"
    val parseStr1 = ""

    val program = IndexedSeq(
      MatchSet(dSet),
      PushChar,
      Accept)

    val vm = new PowersetVm(program)

    val tree0 = vm.eval(parseStr0)
    val tree1 = vm.eval(parseStr0)

    tree0 should equal (None)
    tree1 should equal (None)
  }

  it should "not parse strings not in the language (d ~ c)" in {
    val parseStr0 = "bb"
    val parseStr1 = "cd"

    val program = IndexedSeq(
      MatchSet(dSet),
      PushChar,
      MatchSet(cSet),
      PushChar,
      PushConcat,
      Accept)

    val vm = new PowersetVm(program)

    val tree0 = vm.eval(parseStr0)
    val tree1 = vm.eval(parseStr1)

    tree0 should equal (None)
    tree1 should equal (None)
  }

  it should "not parse strings not in the language (Union(d, c))" in {
    val parseStr1 = "db"
    val parseStr2 = "b"
    val program = IndexedSeq(
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(cSet),
      PushChar,
      PushRight,
      Accept
    )
    val vm = new PowersetVm(program)
    val tree1 = vm.eval(parseStr1)
    val tree2 = vm.eval(parseStr2)
    tree1 shouldEqual None
    tree2 shouldEqual None
  }

  it should "not parse strings not in the language b*" in {
    val parseStr1 = "a"
    val parseStr2 = "bc"
    val parseStr3 = "bbbbbbbbbd"
    val program = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      Accept
    )
    val vm = new PowersetVm(program)
    val tree1 = vm.eval(parseStr1)
    val tree2 = vm.eval(parseStr2)
    val tree3 = vm.eval(parseStr3)
    tree1 shouldEqual None
    tree2 shouldEqual None
    tree3 shouldEqual None
  }

  it should "not parse strings not in the language (d.*) | ((c | b)^2)" in {
    val parseStr0 = "b"
    val parseStr1 = "dbc"

    val program = IndexedSeq(
      Fork(1,8),
      MatchSet(Chars('b', 'c').chars),
      PushChar,
      MatchSet(Chars('b', 'c').chars),
      PushChar,
      PushConcat,
      PushLeft,
      Jump(8),
      InitStar,
      Fork(1,5),
      MatchSet(dSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushRight,
      Accept)

    val vm = new PowersetVm(program)

    val tree0 = vm.eval(parseStr0)
    val tree1 = vm.eval(parseStr1)

    tree0 shouldEqual None
    tree1 shouldEqual None
  }

  it should "not parse strings not in the language (Concatenate(b.*, ε) | (c | (d^2))).+" in {
    val parseStr0 = "bddd"
    val parseStr1 = "bdb"
    val parseStr2 = "cd"

    val program = IndexedSeq(
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushLeft,
      Jump(19),
      Fork(1,8),
      MatchSet(dSet),
      PushChar,
      MatchSet(dSet),
      PushChar,
      PushConcat,
      PushLeft,
      Jump(10),
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushEmpty,
      PushConcat,
      PushRight,
      PushRight,
      InitStar,
      CheckProgress,
      Fork(1,26),
      Fork(1,5),
      MatchSet(cSet),
      PushChar,
      PushLeft,
      Jump(19),
      Fork(1,8),
      MatchSet(dSet),
      PushChar,
      MatchSet(dSet),
      PushChar,
      PushConcat,
      PushLeft,
      Jump(10),
      InitStar,
      Fork(1,5),
      MatchSet(bSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushEmpty,
      PushConcat,
      PushRight,
      PushRight,
      PushStar,
      Jump(-26),
      PushConcat,
      Accept)

    val vm = new PowersetVm(program)

    val tree0 = vm.eval(parseStr0)
    val tree1 = vm.eval(parseStr1)
    val tree2 = vm.eval(parseStr2)

    tree0 shouldEqual None
    tree1 shouldEqual None
    tree2 shouldEqual None
  }
  // more tests...
}
