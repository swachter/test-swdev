package eu.swdev.parser.push

import java.util.regex.Pattern

trait Parsers2 {

  sealed trait ParserState[-I, +O] { self =>

    def ~[I1 <: I, O1 >: O](p: => ParserState[I1, O1]): ParserState[I1, O1] = this and p

    def and[I1 <: I, O1 >: O](p: => ParserState[I1, O1]): ParserState[I1, O1] = this match {
      case Await(push, flush) => Await(push andThen (_ ~ p), flush ~ p)
      case Emit(out, next) => Emit(out, next ~ p)
      case Halt() => p
      case s@Error() => s
      case Mark(next) => Mark(next ~ p)
      case Reset(next) => Reset(next ~ p)
      case Commit(next, handle) => Commit(next ~ p, handle)
    }

    def |[I1 <: I, O1 >: O](p: => ParserState[I1, O1]): ParserState[I1, O1] = this or p

    def or[I1 <: I, O1 >: O](p: => ParserState[I1, O1]): ParserState[I1, O1] = Mark(this ||| p)

    private def |||[I1 <: I, O1 >: O](p: => ParserState[I1, O1]): ParserState[I1, O1] = this match {
      case Await(push, flush) => Await(push andThen (_ ||| p), flush ||| p)
      case Emit(out, next) => Emit(out, next ||| p)
      case Halt() => {
        // auto-commit the first alternative
        // set the handle property to false because committing to the first alternative of this choice
        // does not mean committing to alternatives of superordinate choices.
        Commit(Halt(), false)
      }
      case Error() => {
        // Reset and continue with the second alternative.
        // Ignore any explicit commits that may be output by the second alternative because there is no more mark set.
        // In other words: the second alternative runs unconditionally
        Reset(ignoreCommit(p))
      }
      case Mark(next) => Mark(next ||| p)    // pass-through Mark of nested choice
      case Reset(next) => Reset(next ||| p)  // pass-through Reset of nested choice
      case Commit(next, handle) => {
        if (handle) {
          // there is an explicit commit for the first alternative
          // -> commit the alternative, but ignore any further commits of its continuation
          // -> set the handle property to false because committing to the first alternative of this choice
          //    does not mean committing to alternatives of superordinate choices.
          Commit(ignoreCommit(next), false)
        } else {
          // pass-through Commit of nested choice
          Commit(next ||| p, false)
        }
      }
    }

    def many: ParserState[I, O] = (this ~ commit ~ many) | Halt()

    def oneOrMore: ParserState[I, O] = this ~ many

    def >>=[I1 <: I, T](f: O => ParserState[I1, T]): ParserState[I1, T] = this flatMap f

    def flatMap[I1 <: I, T](f: O => ParserState[I1, T]): ParserState[I1, T] = this match {
      case Await(push, flush) => Await(push andThen (_ >>= f), flush >>= f)
      case Emit(out, next) => {
        // for each output o: apply the function f to get a ParserState[I1, T]
        // and join these parser states with the final (next >>= f)
        // f(on) ~ ... ~ f(o1) ~ (next >>= f)
        // NB: The oldest output is on and the latest output is o1.
        //     Therefore the concatenation starts with f(on).
        out.foldLeft(next >>= f)((ps, o) => f(o) ~ ps)
      }
      case s@Halt() => s
      case s@Error() => s
      case Mark(next) => Mark(next >>= f)
      case Reset(next) => Reset(next >>= f)
      case Commit(next, handle) => Commit(next >>= f, handle)
    }

    def map[O1](f: O => O1): ParserState[I, O1] = this match {
      case Await(push, flush) => Await(push andThen (_ map f), flush map f)
      case Emit(out, next) => Emit(out map f, next map f)
      case s@Halt() => s
      case s@Error() => s
      case Mark(next) => Mark(next map f)
      case Reset(next) => Reset(next map f)
      case Commit(next, handle) => Commit(next map f, handle)
    }

    def ? = optional

    def optional: ParserState[I, O] = this | Halt()

    def option: ParserState[I, Option[O]] = map(Some(_)) | unit(None)

    def |>[I1 >: O, O2](p2: ParserState[I1, O2]): ParserState[I, O2] = this pipe p2

    def pipe[I1 >: O, O2](p2: ParserState[I1, O2]): ParserState[I, O2] = {
      p2 match {
        case Await(push2, flush2) => this match {
          case Await(push, flush) => Await(push andThen (_ |> p2), flush  |> p2)
          case Emit(out, next) => {
            out match {
              case h :: t => (Emit(t, next): ParserState[I, O]) |> push2(h)
              case _ => next |> flush2
            }
          }
          case s@Halt() => s |> flush2
          case s@Error() => s
          case Mark(next) => Mark(next |> p2)
          case Reset(next) => Reset(next |> p2)
          case Commit(next, handle) => Commit(next |> p2, handle)
        }
        case Emit(out2, next2) => Emit(out2, this |> next2)
        case s@Halt() => s
        case s@Error() => s
        case Mark(next2) => markedPipe(this, next2, PipeRecorder(Nil, Nil, Nil))
        case Reset(next2) => throw new RuntimeException("pipe can not be reset - it has not been marked")
        case Commit(next2, handle2) => this |> next2 // ignore commit - pipe has not been marked
      }
    }

    def collapse: ParserState[I, Seq[O]] = {
      def doCollapse(p: ParserState[I, O], accu: Seq[O]): ParserState[I, Seq[O]] = {
        p match {
          case Await(push, flush) => Await(push andThen (doCollapse(_, accu)), doCollapse(flush, accu))
          case Emit(out, next) => doCollapse(next, out ++ accu)
          case s@Halt() => Emit(Seq(accu), Halt())
          case s@Error() => s
          case Mark(next) => Mark(doCollapse(next, accu))
          case Reset(next) => Reset(doCollapse(next, accu))
          case Commit(next, handle) => Commit(doCollapse(next, accu), handle)
        }
      }
      doCollapse(this, Seq())
    }

  }

  case class Await[I, O](push: I => ParserState[I, O], flush: ParserState[I, O]) extends ParserState[I, O]

  case class Emit[I, O](out: Seq[O], next: ParserState[I, O]) extends ParserState[I, O]

  case class Halt() extends ParserState[Any, Nothing]

  case class Error() extends ParserState[Any, Nothing]

  case class Mark[I, O](next: ParserState[I, O]) extends ParserState[I, O]

  case class Reset[I, O](next: ParserState[I, O]) extends ParserState[I, O]

  case class Commit[I, O](next: ParserState[I, O],
                       // the handle property indicates if a commit must be handled by its enclosing choice or not
                       handle: Boolean) extends ParserState[I, O]

  // Ignores all commits whose handle property is true. Commits whose handle property is false are forwarded.
  // This is used to filter all commits after the the first alternative of a choice was committed or
  // after a choice resumed with its second alternative.
  private def ignoreCommit[I, O](p: ParserState[I, O]): ParserState[I, O] = p match {
    case Await(push, flush) => Await(push andThen (ignoreCommit(_)), ignoreCommit(flush))
    case Emit(out, next) => Emit(out, ignoreCommit(next))
    case s@Halt() => s
    case s@Error() => s
    case Mark(next) => Mark(ignoreCommit(next))
    case Reset(next) => Reset(ignoreCommit(next))
    case s@Commit(next, handle) => if (handle) ignoreCommit(next) else Commit(ignoreCommit(next), handle)
  }

  private def ignoreAllCommits[I, O](p: ParserState[I, O]): ParserState[I, O] = p match {
    case Await(push, flush) => Await(push andThen (ignoreAllCommits(_)), ignoreAllCommits(flush))
    case Emit(out, next) => Emit(out, ignoreAllCommits(next))
    case s@Halt() => s
    case s@Error() => s
    case Mark(next) => Mark(ignoreAllCommits(next))
    case Reset(next) => Reset(ignoreAllCommits(next))
    case s@Commit(next, handle) => ignoreAllCommits(next)
  }

  //
  //
  //

  private case class PipeRecorder[I, O](recordedInput: Seq[I], recordedOutput: Seq[O], unconsumedInput: Seq[I]) {
    def recordInput(in: Seq[I]): PipeRecorder[I, O] = copy(recordedInput = in ++ recordedInput, unconsumedInput = in.reverse)
    def peek(): Option[I] = unconsumedInput.headOption
    def recordOutput(out: Seq[O]): PipeRecorder[I, O] = copy(recordedOutput = out ++ recordedOutput)
    def consumed(): PipeRecorder[I, O] = copy(unconsumedInput = unconsumedInput.tail)
  }

  private def markedPipe[I, M, O](p1: ParserState[I, M], p2: ParserState[M, O], rec: PipeRecorder[M, O]): ParserState[I, O] = {
    p2 match {
      case Await(push2, flush2) => {
        // the right parser needs input
        rec.peek() match {
          case Some(o) => {
            // there is some recorded input -> apply it to the right parser
            markedPipe(p1, push2(o), rec.consumed)
          }
          case None => {
            // there is no recorded input -> check the left parser
            p1 match {
              case Await(push, flush) => {
                // the left parser needs input
                // if there is input then recurse with the new state of the left parser
                // otherwise recurse with the flush state of the left parser
                Await(push andThen (markedPipe(_, p2, rec)), markedPipe(flush, p2, rec))
              }
              case Emit(out, next) => {
                // the left parser outputs some tokens -> record them and recurse
                markedPipe(next, p2, rec.recordInput(out))
              }
              case s@Halt() => {
                // the left parser is halted -> flush the right parser
                markedPipe(s, flush2, rec)
              }
              case s@Error() => s
              case Mark(next) => Mark(markedPipe(next, p2, rec)) // pass-through Mark of left parser
              case Reset(next) => Reset(markedPipe(next, p2, rec)) // pass-through Reset of left parser
              case Commit(next, handle) => Commit(markedPipe(next, p2, rec), handle)  // pass-through Commit of left parser
            }
          }
        }
      }
      case Emit(out2, next2) => {
        // the right parser outputs some tokens -> record them and recurse
        markedPipe(p1, next2, rec.recordOutput(out2))
      }
      case s@Halt() => {
        // emit the recorded output
        Emit(rec.recordedOutput, Halt())
      }
      case s@Error() => s
      case Mark(next2) => {
        markedPipe(p1, next2, PipeRecorder(Nil, Nil, Nil))
      }
      case Reset(next2) => {
        // Replay the recorded input by replacing the left parser with Emit(rec.recordedInput.reverse, p1)
        // and pipe this parser into the next right parser
        Emit(rec.recordedInput.reverse, p1) |> ignoreAllCommits(next2)
      }
      case Commit(next2, handle2) => {
        Emit(rec.recordedOutput, Emit(rec.recordedInput, p1) |> ignoreAllCommits(next2))
      }
    }
  }

  //
  //
  //

  def unit[I, O](o: O): ParserState[I, O] = Emit(Seq(o), Halt())

  def filter[I](f: I => Boolean): ParserState[I, I] = (Await(i => if (f(i)) Emit(Seq(i), Halt()) else Halt(), Halt()): ParserState[I, I]).many

  val commit: ParserState[Any, Nothing] = Commit(Halt(), true)

  // a process that skips a number of inputs
  def skip(cnt: Int): ParserState[Any, Nothing] = if (cnt > 0) Await((i => skip(cnt - 1)), Halt()) else Halt()

  def require[I](input: I): ParserState[I, Nothing] = Await(i => if (i == input) Halt() else Error(), Error())

  def require[I](input: Seq[I]): ParserState[I, Nothing] = if (input.isEmpty) {
    Halt()
  } else {
    Await(i => if (i == input.head) require(input.tail) else Error(), Error())
  }

  //
  //
  //

  /**
   * Runs the parser state until its end or until the run state has no more input.
   *
   * The encountered parser states are collected for debugging.
   *
   * @param ps
   * @param runState
   * @param log
   * @tparam O
   * @return
   */
  def run[I, O](ps: ParserState[I, O], runState: RunState[I, O], log: List[ParserState[I, O]]): (RunResult[I, O], List[ParserState[I, O]]) = {
    ps match {
      case Await(push, flush) => runState.input match {
        case Some(i) => run(push(i), runState.next, ps :: log)
        case None => run(flush, runState, ps :: log)
      }
      case Emit(out, next) => run(next, runState.output(out.to[List]), ps :: log)
      case Halt() => (Success(runState.result.reverse, runState.unconsumed), ps :: log)
      case Error() => (Failure(runState.result.reverse, runState.unconsumed), ps :: log)
      case Mark(next) => run(next, runState.mark, ps :: log)
      case Reset(next) => run(next, runState.reset, ps :: log)
      case Commit(next, handle) => run(next, runState.commit, ps :: log)
    }
  }

  trait RunState[I, O] {

    def mark: RunState[I, O]
    def commit: RunState[I, O]
    def reset: RunState[I, O]

    def next: RunState[I, O]

    def input: Option[I]
    def output(o: List[O]): RunState[I, O]

    def unconsumed: List[I]
    def result: List[O]
  }

  abstract class ListRunState[I, O](val unconsumed: List[I], val result: List[O]) extends RunState[I, O] {
    def mark: RunState[I, O] = new MarkedListRunState(unconsumed, Nil, unconsumed, this)
    def input: Option[I] = unconsumed.headOption
    def advance(newUnconsumed: List[I], out: List[O]): ListRunState[I, O]
  }

  class RootListRunState[I, O](unconsumed: List[I], result: List[O]) extends ListRunState[I, O](unconsumed, result) {
    def commit: RunState[I, O] = this // ignore commit - run state has not been marked
    def reset: RunState[I, O] = throw new RuntimeException("input can not be reset - it has not been marked")
    def next: RunState[I, O] = unconsumed match {
      case h :: t => new RootListRunState(t, result)
      case _ => new RootListRunState(unconsumed, result)
    }
    def output(o: List[O]): RunState[I, O] = new RootListRunState(unconsumed, o ++ result)
    def advance(newUnconsumed: List[I], out: List[O]): ListRunState[I, O] = new RootListRunState(newUnconsumed, out ++ result)
  }

  class MarkedListRunState[I, O](unconsumed: List[I], result: List[O], mark: List[I], parent: ListRunState[I, O]) extends ListRunState[I, O](unconsumed, result) {
    def commit: RunState[I, O] = parent.advance(unconsumed, result) // result.foldRight(parent)((o, d) => d.out(o))
    def reset: RunState[I, O] = parent
    def next: RunState[I, O] = unconsumed match {
      case h :: t => new MarkedListRunState(t, result, mark, parent)
      case _ => new MarkedListRunState(unconsumed, result, mark, parent)
    }
    def output(o: List[O]): RunState[I, O] = new MarkedListRunState(unconsumed, o ++ result, mark, parent)
    def advance(newUnconsumed: List[I], out: List[O]): ListRunState[I, O] = new MarkedListRunState(newUnconsumed, out ++ result, mark, parent)
  }

  sealed trait RunResult[I, O]
  case class Success[I, O](out: Seq[O], unconsumed: Seq[I]) extends RunResult[I, O]
  case class Failure[I, O](out: Seq[O], unconsumed: Seq[I]) extends RunResult[I, O]

  //
  //
  //


  trait MarkedParsingState[I, O] {

    def push(i: I): ParserState[I, O]

    def flush: ParserState[I, O]

    def await: ParserState[I, O] = Await(i => push(i), flush)

    def finishWithFailure: ParserState[I, O] = Reset(Error())

    def finishWithSuccess(out: Option[O], consumed: Option[Int]): ParserState[I, O] = {
      val result = out match {
        case Some(o) => Emit(Seq(o), Halt())
        case None => Halt()
      }
      consumed match {
        case Some(i) => Reset(skip(i) ~ result)
        case None => Commit(result, false)
      }
    }

  }

  /**
   * Returns a parser that sets a mark, repeatedly consumes input, and finally
   * commits or resets the mark.
   *
   * @param state Provides the initial parsing state. It is crucial that this is a by-name parameter because each
   *              instantiation needs a new state
   * @tparam I
   * @tparam O
   * @return
   */
  def markedParsing[I, O](state: => MarkedParsingState[I, O]): ParserState[I, O] =
    Mark(Await(i => state.push(i), state.flush))

}

trait CharParsers2 extends Parsers2 {

  /**
   * Creates a parser for a regular expression.
   *
   * This parser uses only a single mark for its input. The input is adjusted corresponding to the consumed characters
   * by either committing or reset and skips.
   *
   * @param pattern
   * @param greedy indicates if the parser should consume as much input as possible
   * @return the parser
   */
  def optimizedRegexParser(pattern: Pattern, greedy: Boolean): ParserState[Char, String] = markedParsing(new MarkedParsingState[Char, String] {

    // The regex parser marks its input then awaits characters until the first match is found (in case of a
    // non-greedy parser), until more input will not match, or until it is flushed.
    //
    // When the parser stops by either emitting a found match or by indicating an error it takes care that its
    // input is correctly positioned. If not all received characters are contained in the output then
    // the parser resets its input probably skips the number of characters it actually processed. Otherwise the
    // parser has successfully processed all received characters and it simply commits its input.


    val sb = new StringBuilder
    var foundMatch = if (pattern.matcher("").matches()) Some(0) else None

    def push(c: Char): ParserState[Char, String] = {
      sb.append(c)
      val matcher = pattern.matcher(sb)
      if (matcher.matches()) {
        foundMatch = Some(sb.length)
        if (greedy) {
          await
        } else {
          flush
        }
      } else {
        // it does not match
        if (matcher.hitEnd()) {
          // maybe after some more input it matches
          if (greedy || foundMatch.isEmpty) {
            await
          } else {
            flush
          }
        } else {
          // it will never match with more input
          flush
        }
      }
    }

    def flush: ParserState[Char, String] = {
      foundMatch match {
        case s@Some(l) => finishWithSuccess(Some(sb.substring(0, l)), if (l == sb.length) None else s)
        case None => finishWithFailure
      }
    }
  })

  /**
   * Simple RegEx parser that uses the choice combinator for backtracking.
   *
   * @param pattern
   * @param greedy
   * @return
   */
  def regexParser(pattern: Pattern, greedy: Boolean): ParserState[Char, String] = {
    def step(sb: String): ParserState[Char, String] = {
      val matcher = pattern.matcher(sb.mkString)
      if (matcher.matches()) {
        val emit = Emit(Seq(sb), Halt())
        if (sb.isEmpty || greedy) {
          // maybe there is a longer match -> try it using the first alternative of the choice.
          // if there is no longer match then emit the found match (second alternative of choice
          Await((c: Char) => step(sb + c), emit) | emit
        } else {
          emit
        }
      } else {
        if (matcher.hitEnd()) {
          // maybe there is a longer match
          Await((c: Char) => step(sb + c), Error()) | Error()
        } else {
          Error()
        }
      }
    }
    step("")
  }

}