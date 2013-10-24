package eu.swdev.parser.push

trait PParsers {

  sealed trait Parser[-I, +O] { //self =>

    def ~[I1 <: I, O1 >: O](p: => Parser[I1, O1]): Parser[I1, O1] = this and p

    def and[I1 <: I, O1 >: O](p: => Parser[I1, O1]): Parser[I1, O1] = this match {
      case Await(push, flush) => Await(push andThen (_ ~ p), flush ~ p)
      case Emit(out, next) => Emit(out, next ~ p)
      case Halt() => p
      case s@Error() => s
      case Restore(input, next) => Restore(input, next and p)
    }

    def |[I1 <: I, O1 >: O](p: => Parser[I1, O1]): Parser[I1, O1] = this or p

    def or[I1 <: I, O1 >: O](p: => Parser[I1, O1]): Parser[I1, O1] = {
      def tryFirst(first: Parser[I, O], input: Seq[I]): Parser[I1, O1] = {
        first match {
          case Await(push, flush) => Await(i => tryFirst(push(i), i +: input), tryFirst(flush, input))
          case Emit(out, next) => {
            // the Emit commits the first alternative
            Emit(out, next)
          }
          case s@Halt() => s
          case s@Error() => Restore(input, p)
          case Restore(inp, next) => {
            Restore(inp, tryFirst(next, input.drop(inp.length)))
          }
        }
      }
      tryFirst(this, Seq())
    }

    def many: Parser[I, O] = (this ~ many) | Halt()

    def oneOrMore: Parser[I, O] = this ~ many

    def >>=[I1 <: I, T](f: O => Parser[I1, T]): Parser[I1, T] = this flatMap f

    def flatMap[I1 <: I, T](f: O => Parser[I1, T]): Parser[I1, T] = this match {
      case Await(push, flush) => Await(push andThen (_ >>= f), flush >>= f)
      case Emit(out, next) => {
        // for each output o: apply the function f to get a ParserState[T]
        // joins these parser states with the final (next >>= f)
        // f(on) ~ ... ~ f(o1) ~ (next >>= f)
        // NB: The oldest output is on and the latest output is o1.
        //     Therefore the concatenation starts with f(on).
        out.foldLeft(next >>= f)((ps, o) => f(o) ~ ps)
      }
      case s@Halt() => s
      case s@Error() => s
      case Restore(input, next) => Restore(input, next >>= f)
    }

    def map[O1](f: O => O1): Parser[I, O1] = this match {
      case Await(push, flush) => Await(push andThen (_ map f), flush map f)
      case Emit(out, next) => Emit(out map f, next map f)
      case s@Halt() => s
      case s@Error() => s
      case Restore(input, next) => Restore(input, next map f)
    }

    def ? = optional

    def optional: Parser[I, O] = this | Halt()

    def option: Parser[I, Option[O]] = map(Some(_)) | unit(None)

    def |>[I1 >: O, O2](p2: Parser[I1, O2]): Parser[I, O2] = this pipe p2

    def pipe[O2](p2: Parser[O, O2]): Parser[I, O2] = {
      p2 match {
        case Await(push2, flush2) => this match {
          case Await(push, flush) => Await(push andThen (_ |> p2), flush  |> p2)
          case Emit(out, next) => {
            out match {
              case h :: t => (Emit(t, next): Parser[I, O]) |> push2(h)
              case _ => next |> flush2
            }
          }
          case s@Halt() => s |> flush2
          case s@Error() => s
          case Restore(input, next) => Restore(input, next |> p2)
        }
        case Emit(out2, next2) => Emit(out2, this |> next2)
        case s@Halt() => s
        case s@Error() => s
        case Restore(input2, next2) => Emit(input2, this) |> next2
      }
    }

    def collapse: Parser[I, Seq[O]] = {
      def doCollapse(p: Parser[I, O], accu: Seq[O]): Parser[I, Seq[O]] = {
        p match {
          case Await(push, flush) => Await(push andThen (doCollapse(_, accu)), doCollapse(flush, accu))
          case Emit(out, next) => doCollapse(next, out ++ accu)
          case s@Halt() => Emit(Seq(accu), Halt())
          case s@Error() => s
          case Restore(input, next) => Restore(input, doCollapse(next, accu))
        }
      }
      doCollapse(this, Seq())
    }

  }

  case class Await[I, O](push: I => Parser[I, O], flush: Parser[I, O]) extends Parser[I, O]

  case class Emit[I, O](out: Seq[O], next: Parser[I, O]) extends Parser[I, O]

  case class Halt() extends Parser[Any, Nothing]

  case class Error() extends Parser[Any, Nothing]

  case class Restore[I, O](input: Seq[I], next: Parser[I, O]) extends Parser[I, O]

  //
  //
  //

  def unit[I, O](o: O): Parser[I, O] = Emit(Seq(o), Halt())

  def filter[I](f: I => Boolean): Parser[I, I] = (Await(i => if (f(i)) Emit(Seq(i), Halt()) else Halt(), Halt()): Parser[I, I]).many

  // a process that skips a number of inputs
  def skip(cnt: Int): Parser[Any, Nothing] = if (cnt > 0) Await((i => skip(cnt - 1)), Halt()) else Halt()

  def require[I](input: I): Parser[I, Nothing] = Await(i => if (i == input) Halt() else Error(), Error())

  def require[I](input: Seq[I]): Parser[I, Nothing] = if (input.isEmpty) {
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
  def run[I, O](ps: Parser[I, O], runState: RunState[I, O], log: List[Parser[I, O]]): (RunResult[I, O], List[Parser[I, O]]) = {
    ps match {
      case Await(push, flush) => runState.input match {
        case Some(i) => run(push(i), runState.next, ps :: log)
        case None => run(flush, runState, ps :: log)
      }
      case Emit(out, next) => run(next, runState.output(out.to[List]), ps :: log)
      case Halt() => (Success(runState.result.reverse, runState.unconsumed), ps :: log)
      case Error() => (Failure(runState.result.reverse, runState.unconsumed), ps :: log)
      case Restore(input, next) => run(next, runState.restore(input), ps :: log)
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
    def restore(input: Seq[I]):RunState[I, O] = new RunStateImpl(input.reverse ++ unconsumed, result)
    def next: RunState[I, O] = new RunStateImpl(unconsumed.tail, result)
    def input: Option[I] = unconsumed.headOption
    def output(o: Seq[O]): PParsers.this.type#RunState[I, O] = new RunStateImpl(unconsumed, o ++ result)
  }

  sealed trait RunResult[I, O]
  case class Success[I, O](out: Seq[O], unconsumed: Seq[I]) extends RunResult[I, O]
  case class Failure[I, O](out: Seq[O], unconsumed: Seq[I]) extends RunResult[I, O]



}

trait CharPParsers extends PParsers {

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
