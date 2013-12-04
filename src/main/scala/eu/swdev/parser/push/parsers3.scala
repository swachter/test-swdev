package eu.swdev.parser.push

import scala.annotation.tailrec

/**
 * Third trial implementation of a streaming push parser framework.
 *
 * The implementation is aligned with the streaming IO framework of the book "Functional Programming in Scala".
 * Backtracking is realized by a Replay state that informs a source that some input must be replayed.
 *
 * Summary: Shortest implementation variant. The implementation is rather obvious.
 */
trait Parsers3 {

  implicit def getParser[I, O](f: () => Parser[I, O]): Parser[I, O] = f()

  sealed trait Parser[I, +O] { self =>

    def ~[O1 >: O](p2: => Parser[I, O1]): Parser[I, O1] = this match {
      case Await(push, flush) => Await(push andThen (_ ~ p2), flush ~ p2)
      case Emit(out, next) => Emit(out, next ~ p2)
      case Halt() => p2 //.withRetry
      case Error(msg, recover) => {
        if (recover.isDefined) {
          Error(msg, Some(recover.get ~ p2))
        } else {
          Error(msg, Some(p2.sync))
        }
      }
      case Replay(input, next) => Replay(input, next ~ p2)
    }

    def |[O1 >: O](p2: => Parser[I, O1]): Parser[I, O1] = this or p2

    /**
     * A parser that first tries this parser and in case of failure tries the specified alternative parser.
     *
     * The "or" parser records its input until the first parser reaches an Emit state. In this case the "or" parser
     * forwards the Emit and commits to the first alternative. In other words no more backtracking is possible after
     * the first parser has emitted some output.
     *
     * If a parser does not generate Emits for some longer input and backtracking is no more required then an
     * Emit with an empty output sequence can be interspersed.
     *
     * @param p2
     * @tparam O1
     * @return
     */
    def or[O1 >: O](p2: => Parser[I, O1]): Parser[I, O1] = {
      def tryFirst(first: Parser[I, O], input: Seq[I], replay: Seq[I]): Parser[I, O1] = {
        first match {
          case Await(push, flush) => {
            if (replay.isEmpty) {
              // await a character
              // either record the received character and continue persuing the first alternative
              // or try to flush the first alternative
              Await(i => tryFirst(push(i), i +: input, replay), tryFirst(flush, input, replay))
            } else {
              tryFirst(push(replay.head), input, replay.tail)
            }
          }
          case Emit(out, next) => {
            // the Emit commits the first alternative
            Emit(out, if (replay.isEmpty) next else Replay(replay, next))
          }
          case s@Halt() => if (replay.isEmpty) s else Replay(replay, s)
          case s: Error[I, O] => {
            // restore the recorded input and continue with the second alternative
            Replay(input.reverse, p2)
          }
          case Replay(inp, next) => {
            tryFirst(next, input, inp ++ replay)
          }
        }
      }
      tryFirst(this, Seq(), Seq())
    }

//    def race[O1 >: O](p2: Parser[I, O1]): Parser[I, O1] = {
//      def doRace(p1: Parser[I, O], replay1: Seq[I], p2: Parser[I, O1], replay2: Seq[I]): Parser[I, O1] = {
//        (p1, p2) match {
//          case (Await(push1, flush1), Await(push2, flush2)) => Await(i => doRace(push1(i), replay1, push2(i), replay2), doRace(flush1, replay1, flush2, replay2))
//        }
//      }
//      doRace(this, Seq(), p2, Seq())
//    }

    /**
     * An "or" parser that attempts this parser and in case of failure tries the specified alternative parser.
     *
     * If a parser is attempted then its output is delayed until the parser has reached its Halt() state. By attempting
     * this parser backtracking is enabled even if this parser would emit some output before it halts.
     *
     * @param p2
     * @tparam O1
     * @return
     */
    def |||[O1 >: O](p2: => Parser[I, O1]): Parser[I, O1] = this.attempt | p2

    /**
     * Delay the output of this parser until it halts. Then emit its collected output and continue with the
     * specified parser.
     *
     * @param p2
     * @tparam O2
     * @return
     */
    def attemptAndThen[O2 >: O](p2: => Parser[I, O2]): Parser[I, O2] = {
      def doAttempt(q: Parser[I, O], accu: Seq[O]): Parser[I, O2] = q match {
        case Await(push, flush) => Await(push andThen (doAttempt(_, accu)), doAttempt(flush, accu))
        case Emit(out, next) => doAttempt(next, out ++ accu)
        case Halt() => Emit(accu, p2)
        case s: Error[I, O] => s
        case Replay(input, next) => Replay(input, doAttempt(next, accu))
      }
      doAttempt(this, Seq())
    }

    def attempt: Parser[I, O] = Parsers3.this.attempt(this)

    def many: Parser[I, O] = (this ~ many) | Halt()

    def oneOrMore: Parser[I, O] = this ~ many

    def >>=[O1](f: O => Parser[I, O1]): Parser[I, O1] = this flatMap f

    /**
     * Collects the complete output of this parser and then applies the flatMap function to each output, and finally
     * concatenates the resulting parsers.
     *
     * @param f
     * @tparam O1
     * @return
     */
    def flatMap[O1](f: O => Parser[I, O1]): Parser[I, O1] = {
      def doFlatMap(p: Parser[I, O], accu: Seq[O]): Parser[I, O1] = {
        p match {
          case Await(push, flush) => Await(push andThen (doFlatMap(_, accu)), doFlatMap(flush, accu))
          case Emit(out, next) => doFlatMap(next, out ++ accu)
          case Halt() => {
            // - for each output o: apply the function f to get a Parser[I, O1]
            // - join these parser with the "and" combinator
            // f(accu_n) ~ ... ~ f(accu_1) ~ Halt()
            // NB: The oldest output is at the end of the accu
            //     Therefore the concatenation starts with f(accu_n).
             accu.foldLeft(Halt(): Parser[I, O1])((ps, o: O) => f(o) ~ ps)
          }
          case Error(msg, recover) => {
            Error(msg, recover.map(doFlatMap(_, accu)))
          }
          case Replay(input, next) => Replay(input, doFlatMap(next, accu))
        }
      }
      doFlatMap(this, Seq())
    }

    def map[O1](f: O => O1): Parser[I, O1] = this match {
      case Await(push, flush) => Await(push andThen (_ map f), flush map f)
      case Emit(out, next) => Emit(out map f, next map f)
      case s@Halt() => Halt()
      case Error(msg, recover) => Error(msg, recover.map(_.map(f)))
      case Replay(input, next) => Replay(input, next map f)
    }

    def ? = optional

    def optional: Parser[I, O] = this | Halt()

    def option: Parser[I, Option[O]] = map(Some(_)) | unit(None)

    def |>[I2 >: O, O2](p2: Parser[I2, O2]): Parser[I, O2] = this pipe p2

    def pipe[I2 >: O, O2](p2: Parser[I2, O2]): Parser[I, O2] = {
      p2 match {
        case Await(push2, flush2) => this match {
          case Await(push, flush) => {
            Await(push andThen (_ |> p2), flush  |> p2)
          }
          case Emit(out, next) => {
            out match {
              case h :: t => (Emit(t, next): Parser[I, O]) |> push2(h)
              case _ => next |> p2
            }
          }
          case s@Halt() => s |> flush2
          case Error(msg, recover) => Error(msg, recover.map(_ |> p2))
          case Replay(input, next) => Replay(input, next |> p2)
        }
        case Emit(out2, next2) => Emit(out2, this |> next2)
        case s@Halt() => Halt()
        case Error(msg, recover) => Error(msg, recover.map(this |> _))
        case Replay(input2, next2) => {
          // pipe the input that is to be replayed into next2
          Emit(input2, this) |> next2
        }
      }
    }

    def collapse: Parser[I, Seq[O]] = {
      def doCollapse(p: Parser[I, O], accu: Seq[O]): Parser[I, Seq[O]] = {
        p match {
          case Await(push, flush) => Await(push andThen (doCollapse(_, accu)), doCollapse(flush, accu))
          case Emit(out, next) => doCollapse(next, out ++ accu)
          case s@Halt() => Emit(Seq(accu), Halt())
          case Error(msg, recover) => Error(msg, recover.map(doCollapse(_, accu)))
          case Replay(input, next) => Replay(input, doCollapse(next, accu))
        }
      }
      doCollapse(this, Seq())
    }

    def sync: Parser[I, O] = this | (skipOne("missing input") ~ this.sync)

    def sync(max: Int): Parser[I, O] = if (max > 0) this | (skipOne("missing input") ~ this.sync(max - 1)) else this

    def withRetry: Parser[I, O] = self trans {
      case s: Emit[I, O] => s
      case Error(msg, recover) => Error(msg, if (recover.isDefined) recover.map(_ | this.sync) else Some(this.sync))
    }

    def withRecovery: Parser[I, O] = self trans {
      case Await(push, flush) => Await(
        i => {
          val tp: Parser[I, O] = push(i)
          tp trans {
            case Error(msg, recover) => Error(msg, Some(self | Replay(Seq(i), Halt())))
            case p => p
          }
        },
        flush
      )
    }

    def withCheck(check: I => Boolean, unexpectedMsg: => I => String): Parser[I, O] = self trans {
      case Await(push, flush) => Await(i => {
        if (check(i)) push(i).withCheck(check, unexpectedMsg) else Error(unexpectedMsg(i), None)
      }, flush)
    }

  }

  implicit class InvariantParserOps[I, O](val parser: Parser[I, O]) { //extends AnyVal {

    def trans(pf: PartialFunction[Parser[I, O], Parser[I, O]]): Parser[I, O] = {
      if (pf.isDefinedAt(parser)) {
        pf(parser)
      } else {
        parser match {
//            case Await(push, flush) => Await(push andThen (_ trans pf), flush() trans pf)
          case Await(push, flush) => Await[I, O]((i: I) => {
            val p: Parser[I, O] = push(i)
            p trans pf
          }, {
            val p: Parser[I, O] = flush
            p trans pf
          })
          case Emit(out, next) => Emit(out, next trans pf)
          case s@Halt() => s
          case Error(msg, rec) => Error(msg, rec.map(_ trans pf))
          case Replay(input, next) => Replay(input, next trans pf)
        }
      }
    }

    def withErrorValue(errorValue: => Parser[I, O]): Parser[I, O] = parser trans {
      case Error(msg, recover) => Error(msg, if (recover.isDefined) recover.map(errorValue ~ _) else Some(errorValue))
    }

  }

  class Await[I, O](val push: I => Parser[I, O], flushParam: => Parser[I, O]) extends Parser[I, O] {
    lazy val flush: Parser[I, O] = flushParam
  }

  object Await {
    def apply[I, O](push: I => Parser[I, O], flush: => Parser[I, O]) = {
      new Await(push, flush)
    }
    def unapply[I, O](await: Await[I, O]): Option[(I => Parser[I, O], () => Parser[I, O])] = Some((await.push, () => await.flush))
  }

  case class Emit[I, O](out: Seq[O], next: Parser[I, O]) extends Parser[I, O]

  case class Halt[I, O]() extends Parser[I, O]

  case class Error[I, O](msg: String, recover: Option[Parser[I, O]]) extends Parser[I, O]

  /**
   * Signals that some input should be replayed.
   *
   * @param input The input that has to be replayed. The head of the sequence is the first input element
   *              that has to be replayed.
   * @param next
   * @tparam I
   * @tparam O
   */
  case class Replay[I, O](input: Seq[I], next: Parser[I, O]) extends Parser[I, O]

  //
  //
  //

  def unit[I, O](o: O): Parser[I, O] = Emit(Seq(o), Halt())

  def filter[I](f: I => Boolean): Parser[I, I] = (Await(i => if (f(i)) Emit(Seq(i), Halt()) else Halt(), Halt()): Parser[I, I]).many

  // a process that skips a number of input items
  def skip[I](cnt: Int, missingMsg: => String): Parser[I, Nothing] = if (cnt > 0) Await((i => skip(cnt - 1, missingMsg)), Error(missingMsg, None)) else Halt()

  def echoOne[I](missingMsg: => String): Await[I, I] = Await(i => Emit(Seq(i), Halt()), Error(missingMsg, None))
  def skipOne[I](missingMsg: => String): Await[I, Nothing] = Await(i => Halt(), Error(missingMsg, None))

  def consumeOneAndDrop[I](check: I => Boolean, unexpectedMsg: => I => String, missingMsg: => String): Parser[I, Nothing] = {
    skipOne(missingMsg).withCheck(check, unexpectedMsg).withRecovery
  }

  def consumeOneAndDrop[I](input: I, unexpectedMsg: => I => String, missingMsg: => String): Parser[I, Nothing] = {
    consumeOneAndDrop(_ == input, unexpectedMsg, missingMsg)
  }

  def consumeSeqAndDrop[I](input: Seq[I], unexpectedMsg: => Seq[I] => I => String, missingMsg: => Seq[I] => String): Parser[I, Nothing] = input match {
    case h :: t => consumeOneAndDrop(h, unexpectedMsg(input), missingMsg(input)) ~ consumeSeqAndDrop(t, unexpectedMsg, missingMsg)
    case _ => Halt()
  }

  def consumeOneAndEmit[I](check: I => Boolean, default: => Parser[I, I], unexpectedMsg: => I => String, missingMsg: => String): Parser[I, I] = {
    echoOne(missingMsg).withCheck(check, unexpectedMsg).withRecovery // .withErrorValue(default)
  }

  def consumeOneAndEmit[I](input: I, unexpectedMsg: => I => String, missingMsg: => String): Parser[I, I] = {
    consumeOneAndEmit(_ == input, Emit(Seq(input), Halt()), unexpectedMsg, missingMsg)
  }

  def consumeSeqAndEmit[I](input: Seq[I], unexpectedMsg: => Seq[I] => I => String, missingMsg: => Seq[I] => String): Parser[I, I] = input match {
    case h :: t => consumeOneAndEmit(h, unexpectedMsg(input), missingMsg(input)) ~ consumeSeqAndEmit(t, unexpectedMsg, missingMsg)
    case _ => Halt()
  }

  def attempt[I, O](p: Parser[I, O]): Parser[I, O] = p attemptAndThen Halt()

  /**
   * A parser that emits an empty sequence and then halts.
   *
   * If an "or" parser receives and emit while trying its first alternative then it commits to the first alternative.
   * Therefore this parser is called a commit parser.
   *
   * @tparam I
   * @tparam O
   * @return
   */
  def commit[I, O]: Parser[I, O] = Emit(Seq(), Halt())

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
  def run[I, O](ps: Parser[I, O], runState: RunState[I, O], log: List[Parser[I, O]]): (RunResult[I, O], List[Parser[I, O]]) = {
    ps match {
      case Await(push, flush) => runState.input match {
        case Some(i) => run(push(i), runState.next, ps :: log)
        case None => run(flush, runState, ps :: log)
      }
      case Emit(out, next) => run(next, runState.output(out.to[List]), ps :: log)
      case Halt() => (Success(runState.result.reverse, runState.unconsumed), ps :: log)
      case Error(msg, recover) => {
        if (recover.isDefined) {
          val (rr, l) = run(recover.get, runState, ps :: log)
          (rr match {
            case Success(out, unconsumed) => Failure(out, unconsumed)
            case _ => rr
          }, l)
        } else {
          (Failure(runState.result.reverse, runState.unconsumed), ps :: log)
        }
      }
      case Replay(input, next) => run(next, runState.restore(input), ps :: log)
    }
  }

  type ErrorMsg = String
  type RunResult2[I, O] = (Seq[O], Seq[I], List[ErrorMsg])

  def run2[I, O](ps: Parser[I, O], runState: RunState[I, O], errors: List[ErrorMsg], log: List[Parser[I, O]]): (RunResult2[I, O], List[Parser[I, O]]) = {
    ps match {
      case Await(push, flush) => runState.input match {
        case Some(i) => run2(push(i), runState.next, errors, ps :: log)
        case None => run2(flush, runState, errors, ps :: log)
      }
      case Emit(out, next) => run2(next, runState.output(out.to[List]), errors, ps :: log)
      case Halt() => ((runState.result.reverse, runState.unconsumed, errors), ps :: log)
      case Error(msg, recover) => {
        if (recover.isDefined) {
          run2(recover.get, runState, msg :: errors, ps :: log)
        } else {
          ((runState.result.reverse, runState.unconsumed, msg :: errors), ps :: log)
        }
      }
      case Replay(input, next) => run2(next, runState.restore(input), errors, ps :: log)
    }
  }

  trait RunState[I, O] {

    def restore(input: Seq[I]): RunState[I, O]

    def next: RunState[I, O]

    def input: Option[I]
    def output(o: Seq[O]): RunState[I, O]

    def unconsumed: Seq[I]
    def result: Seq[O]
  }

  class RunStateImpl[I, O](val unconsumed: Seq[I], val result: Seq[O]) extends RunState[I, O] {
    def restore(input: Seq[I]):RunState[I, O] = new RunStateImpl(input ++ unconsumed, result)
    def next: RunState[I, O] = new RunStateImpl(unconsumed.tail, result)
    def input: Option[I] = unconsumed.headOption
    def output(o: Seq[O]): RunState[I, O] = new RunStateImpl(unconsumed, o ++ result)
  }

  sealed trait RunResult[I, O]
  case class Success[I, O](out: Seq[O], unconsumed: Seq[I]) extends RunResult[I, O]
  case class Failure[I, O](out: Seq[O], unconsumed: Seq[I]) extends RunResult[I, O]

}

trait CharParsers3 extends Parsers3 {

  import java.util.regex.Pattern


  /**
   * Simple RegEx parser that uses the choice combinator for backtracking.
   *
   * @param pattern
   * @param greedy
   * @return
   */
  def regexParser(pattern: Pattern, greedy: Boolean): Parser[Char, String] = {
    def step(sb: String): Parser[Char, String] = {
      val matcher = pattern.matcher(sb.mkString)
      if (matcher.matches()) {
        val emit: Parser[Char, String] = Emit(Seq(sb), Halt())
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
          Await((c: Char) => step(sb + c), Error("regular expression did not match", Some(Replay(sb.toList, Halt()))))
        } else {
          Error("regular expression did not match", Some(Replay(sb.toList, Halt())))
        }
      }
    }
    step("")
  }

}
