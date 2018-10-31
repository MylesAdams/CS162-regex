// A virtual machine implementation of derivative-based matching.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.regex._

object `package` {
  // Programs for the DerivativeMachine.
  type Program = Seq[Instruction]

  // Pretty-print derivative virtual machine programs.
  def programToString(prog: Program): String = {
    val strs = for (inst <- prog) yield inst match {
      case `PushDerive` => "derive"
      case `PushConcatenate` => "concatenate"
      case `PushUnion` => "union"
      case `PushComplement` => "complement"
      case `PushIntersect` => "intersect"
      case `PushNullable` => "nullable"
      case PushRe(re) => "push " + re.toString
    }

    strs.mkString("\n")
  }
}

// Instructions for the virtual machine.
//
// - Derive: pop the top of the operand stack, compute its derivative w.r.t. the
//   machine's given char, then push the result back on the operand stack.
// - PushConcatentate: pop the top two elements of the operand stack and push
//   their concatenation back on.
// - PushUnion: pop the top two elements of the operand stack and push their
//   union back on.
// - PushComplement: pop the top of the operand stack, take its complement, and
//   push the result back on.
// - PushIntersect: pop the top two elements of the operand stack and push
//   their intersection back on.
// - PushNullable: pop the top of the operand stack, compute its nullability,
//   and push the result back on the operand stack.
// - PushRe(re): push re onto the top of the operand stack.
sealed abstract class Instruction
case object PushDerive extends Instruction
case object PushConcatenate extends Instruction
case object PushUnion extends Instruction
case object PushComplement extends Instruction
case object PushIntersect extends Instruction
case object PushNullable extends Instruction
case class PushRe(re: Regex) extends Instruction

class DerivativeMachine(re: Regex) {
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Returns true iff 'str' is recognized by 're'.
  def eval(str: String): Boolean = str.foldLeft(re){(re, char) => run(Seq(re), Seq(PushDerive), char)}.nullable == `ε`

  // Returns the derivative of 're' w.r.t. 'char'.
  def derive(char: Char): Regex = run(Seq(re), Seq(PushDerive), char)

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Derives a regular expression from the top of 'operands' w.r.t. 'char'.
  // @annotation.tailrec
  private def run(operands: Seq[Regex], program: Program, char: Char): Regex = {
    if (program.isEmpty) {
      assert(operands.size == 1)
      operands.head
    }
    else {
      // println("program = " + program.toString())
      // println("operand head = " + operands.toString())
      program.head match {
        case PushDerive => {
          assert(operands.size > 0)
          operands.head match {
            case `∅` => run(
              `∅` +: operands.tail,
              program.tail,
              char)

            case `ε` => run(
              `∅` +: operands.tail,
              program.tail,
              char)

            case Chars(c) if c.contains(char) => run(
              `ε` +: operands.tail,
              program.tail,
              char)

            case Chars(c) => run(
              `∅` +: operands.tail,
              program.tail,
              char)

            case Concatenate(r1, r2) => run(
              operands.tail,
              Seq(
                PushRe(r1),
                PushNullable,
                PushRe(r2),
                PushDerive,
                PushConcatenate,
                PushRe(r1),
                PushDerive,
                PushRe(r2),
                PushConcatenate,
                PushUnion) ++ program.tail,
              char)

            case Union(r1, r2) => run(
              operands.tail,
              Seq(
                PushRe(r1),
                PushDerive,
                PushRe(r2),
                PushDerive,
                PushUnion) ++ program.tail,
              char)

            case KleeneStar(r) => run(
              operands.tail,
              Seq(
                PushRe(r),
                PushDerive,
                PushRe(KleeneStar(r)),
                PushConcatenate) ++ program.tail,
              char)

            case Complement(r) => run(
              operands.tail,
              Seq(
                PushRe(r),
                PushDerive,
                PushComplement
              ) ++ program.tail,
              char)

            case Intersect(r1, r2) => run(
              operands.tail,
              Seq(
                PushRe(r1),
                PushDerive,
                PushRe(r2),
                PushDerive,
                PushIntersect
              ) ++ program.tail,
              char)
          }
        }

        case PushConcatenate => {
          assert(operands.size > 1)
          run(
            operands.tail.tail,
            PushRe(operands.tail.head ~ operands.head) +: program.tail,
            char)
        }

        case PushUnion => {
          assert(operands.size > 1)
          run(
            operands.tail.tail,
            PushRe(operands.tail.head | operands.head) +: program.tail,
            char)
        }

        case PushComplement => {
          assert(operands.size > 0)
          run(
            operands.tail,
            PushRe(!operands.head) +: program.tail,
            char)
        }

        case PushIntersect => {
          assert(operands.size > 1)
          run(
            operands.tail.tail,
            PushRe(operands.tail.head & operands.head) +: program.tail,
            char)
        }

        case PushNullable => {
          assert(operands.size > 0)
          run(
            operands.tail,
            PushRe(operands.head.nullable) +: program.tail,
            char)
        }

        case PushRe(r) => run(
          r +: operands,
          program.tail,
          char)
      }
    }
  }
}

object DerivativeMachine {
  def apply(re : Regex) = new DerivativeMachine(re)
}
