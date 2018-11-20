package edu.ucsb.cs.cs162.regex.vm

import edu.ucsb.cs.cs162.regex.parse_tree._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

// A virtual machine that uses Thompson's powerset strategy to implement a
// non-backtracking algorithm for regular expression matching.
class PowersetVm(program: Program) extends VirtualMachine(program) {
  override def eval(str: String): Option[ParseTree] = {
    // Algorithm:
    // 1. compute initial set of threads (the ε-closure of the nfa start state)
    // 2. if the input string is empty go to step 7
    // 3. run the threads until they reach a match or accept instruction
    // 4. compact them to enforce at most one thread per program counter
    // 5. execute the surviving threads one step (i.e., the match or accept instruction)
    // 6. go to step 2
    // 7. compact the final set of threads
    // 8. if there is a surviving thread at an accept instruction, then that
    //    thread's 'parse' contains the final answer; otherwise there is no answer

    // Execute all given threads until they reach either a MatchSet or an Accept
    // instruction; returns the resulting set of Threads.
    @annotation.tailrec
    def runUntilMatchOrAccept(
      thread: Thread,
      todo: Set[Thread],
      result: Set[Thread]
    ): Set[Thread] =
    {
      // println(thread)
      program(thread.pc) match {
      case `Accept` => {
        if (!todo.isEmpty) {
          val nextThread = todo.head

          runUntilMatchOrAccept(nextThread, todo - nextThread, result + thread)
        }
        else {
          result + thread
        }
      }

      case `Reject` => {
        if (!todo.isEmpty) {
          val nextThread = todo.head

          runUntilMatchOrAccept(nextThread, todo - nextThread, result)
        }
        else {
          result
        }
      }

      case `CheckProgress` => {
        if (thread.progress contains(thread.pc)) {
          if (!todo.isEmpty) {
            val nextThread = todo.head

            runUntilMatchOrAccept(nextThread, todo - nextThread, result)
          }
          else {
            result
          }
        }
        else {
          runUntilMatchOrAccept(
            thread.update(shouldUpdateProgress = true),
            todo, result)
        }
      }

      case MatchSet(chars) => {
        if (!todo.isEmpty) {
          val nextThread = todo.head

          runUntilMatchOrAccept(nextThread, todo - nextThread, result + thread)
        }
        else {
          result + thread
        }
      }

      case Jump(offset) =>
        runUntilMatchOrAccept(thread.update(offset = offset), todo, result)

      case Fork(offset1, offset2) =>
        runUntilMatchOrAccept(
          thread.update(offset = offset1),
          todo + thread.update(offset = offset2),
          result)

      case `PushEmpty` =>
        runUntilMatchOrAccept(
          thread.update(newParseTree = Some(EmptyLeaf)),
          todo,
          result)

      case `PushChar` => {
        assert(false, "should be unreachable")
        Set()
      }

      case `PushConcat` =>
        runUntilMatchOrAccept(
          thread.update(newParseTree =
                          Some(ConcatNode(
                                 thread.parse.tail.head,
                                 thread.parse.head))),
          todo,
          result)

      case `PushLeft` =>
        runUntilMatchOrAccept(
          thread.update(
            newPriorityChar = Some('l'),
            newParseTree = Some(LeftNode(thread.parse.head))),
          todo,
          result)

      case `PushRight` =>
        runUntilMatchOrAccept(
          thread.update(
            newPriorityChar = Some('r'),
            newParseTree = Some(RightNode(thread.parse.head))),
          todo,
          result
        )

      case `InitStar` =>
        runUntilMatchOrAccept(
          thread.update(newParseTree = Some(StarNode(Seq()))),
          todo,
          result)

      case `PushStar` => {
        val body = thread.parse.head
        val star = thread.parse.tail.head

        star match {
          case StarNode(seq) =>
            runUntilMatchOrAccept(
              thread.update(newParseTree = Some(StarNode(body +: seq))),
              todo,
              result)
          case _ => {
            assert(false, "should be unreachable")
            Set()
          }
        }
      }

      case PushCapture(name) =>
        runUntilMatchOrAccept(
          thread.update(
            newParseTree = Some((CaptureNode(name, thread.parse.head)))),
          todo,
          result)

    }}

    // Remove any threads s.t. there exists another thread at the same program
    // point with a smaller Priority.
    def compact(threads: Set[Thread]): Set[Thread] = {
      threads.groupBy(thread => thread.pc).foldLeft(Set[Thread]()) {
        (newThreads, group) => {
          newThreads + group._2.minBy(thread => thread.priority)
}      }
    }

    // Return the result of matching the current string position on all the
    // given threads.
    val matchStringPosition: (Set[Thread], Char) => Set[Thread] =
      (threads, char) => {
        threads.foldLeft(Set[Thread]()) {
          (newThreads, thread) => {
            (program(thread.pc)) match {
               case MatchSet(chars) if chars contains char =>
                 program(thread.pc + 1) match {
                   case `PushChar` =>
                     newThreads + thread.update(
                       offset = 2,
                       newParseTree = Some(CharLeaf(char))
                     ).copy(progress = Set[Int]())

                   case _ => {
                     assert(false, "Should be unreachable")
                     newThreads
                   }
                 }

              case _ => newThreads
            }
          }
        }
      }

    val initialThread = Thread(0, Set[Int](), "", Seq[ParseTree]())

    val endThreads = str.foldLeft(Set[Thread](initialThread)) {
      (accThreads, char) => accThreads.isEmpty match {
        case false => {
          val firstThread = accThreads.head

          val matchOrAcceptThreads = runUntilMatchOrAccept(
            firstThread,
            accThreads - firstThread,
            Set[Thread]())

          matchStringPosition(compact(matchOrAcceptThreads), char)
        }
        case true => accThreads
      }
    }

    if (!endThreads.isEmpty) {
      val finalRunThreads = runUntilMatchOrAccept(
        endThreads.head,
        endThreads.tail,
        Set[Thread]())

      val compactedThreads = compact(finalRunThreads)

      val acceptingThreads =
        compactedThreads.filter(thread => program(thread.pc) == Accept)

      val instructs = acceptingThreads.map{(thread) => program(thread.pc)}

      if (!acceptingThreads.isEmpty) {
        Some(acceptingThreads.head.parse.head)
      }
      else {
        None
      }
    }
    else {
      None
    }



  }

  // A thread of execution for the VM, where 'pc' is the program counter,
  // 'progress' is the set of encountered CheckProgress instructions, 'priority'
  // is the thread priority (lower is better), 'parse' is the current parsing
  // stack. We don't need a separate string position per thread because all
  // executing threads will, by construction, always be at the same string
  // position.
  private case class Thread(pc: Int, progress: Set[Int], priority: String,
    parse: Seq[ParseTree])

  private implicit class UpdateThread(thread: Thread) {
    def update(
      offset: Int = 1,
      shouldUpdateProgress: Boolean = false,
      newPriorityChar: Option[Char] = None,
      newParseTree: Option[ParseTree] = None
    ): Thread = {
      val newPC = thread.pc + offset

      val newProgress = shouldUpdateProgress match {
        case false => thread.progress

        case true => thread.progress + thread.pc
      }

      val newPriority = newPriorityChar match {
        case None => thread.priority

        case Some(char) => thread.priority + char
      }

      val newParse = newParseTree match {
        case None => thread.parse

        case Some(parseTree) => parseTree match {
          case pt: EmptyLeaf.type => pt +: thread.parse

          case pt: CharLeaf => pt +: thread.parse

          case pt: ConcatNode => pt +: thread.parse.tail.tail

          case pt: LeftNode => pt +: thread.parse.tail

          case pt: RightNode => pt +: thread.parse.tail

          case pt: StarNode => {
            if (pt.children.isEmpty) pt +: thread.parse
            else pt+: thread.parse.tail.tail
          }

          case pt: CaptureNode => pt +: thread.parse.tail
        }
      }

      Thread(newPC, newProgress, newPriority, newParse)
    }
  }
}

