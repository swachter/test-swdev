package eu.swdev.parser.push

trait Parsers[I] {

  sealed trait ParserState[+O] { self =>

    def ~[O1 >: O](p: => ParserState[O1]): ParserState[O1] = this and p

    def and[O1 >: O](p: => ParserState[O1]): ParserState[O1] = this match {
      case Await(push, flush) => Await(push andThen (_ ~ p), flush ~ p)
      case Emit(out, next) => Emit(out, next ~ p)
      case Halt() => p
      case s@Error() => s
      case Mark(next) => Mark(next ~ p)
      case Reset(next) => Reset(next ~ p)
      case Commit(next, handle) => Commit(next ~ p, handle)
    }

    def |[O1 >: O](p: => ParserState[O1]): ParserState[O1] = this or p

    def or[O1 >: O](p: => ParserState[O1]): ParserState[O1] = Mark(this ||| p)

    private def |||[O1 >: O](p: => ParserState[O1]): ParserState[O1] = this match {
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

    def many: ParserState[O] = (this ~ many) | Halt()

    def oneOrMore: ParserState[O] = this ~ many

    def >>=[T](f: O => ParserState[T]): ParserState[T] = this flatMap f

    def flatMap[T](f: O => ParserState[T]): ParserState[T] = this match {
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
      case Mark(next) => Mark(next >>= f)
      case Reset(next) => Reset(next >>= f)
      case Commit(next, handle) => Commit(next >>= f, handle)
    }

    def map[O1](f: O => O1): ParserState[O1] = this match {
      case Await(push, flush) => Await(push andThen (_ map f), flush map f)
      case Emit(out, next) => Emit(out map f, next map f)
      case s@Halt() => s
      case s@Error() => s
      case Mark(next) => Mark(next map f)
      case Reset(next) => Reset(next map f)
      case Commit(next, handle) => Commit(next map f, handle)
    }

    def ? = optional

    def optional: ParserState[O] = this | Halt()

    def option: ParserState[Option[O]] = map(Some(_)) | unit(None)
  }

  case class Await[O](push: I => ParserState[O], flush: ParserState[O]) extends ParserState[O]

  case class Emit[O](out: Seq[O], next: ParserState[O]) extends ParserState[O]

  case class Halt[O]() extends ParserState[Nothing]

  case class Error[O]() extends ParserState[Nothing]

  case class Mark[O](next: ParserState[O]) extends ParserState[O]

  case class Reset[O](next: ParserState[O]) extends ParserState[O]

  case class Commit[O](next: ParserState[O],
                       // the handle property indicates if a commit must be handled by its enclosing choice or not
                       handle: Boolean) extends ParserState[O]

  private def ignoreCommit[O](p: => ParserState[O]): ParserState[O] = p match {
    case Await(push, flush) => Await(push andThen (ignoreCommit(_)), ignoreCommit(flush))
    case Emit(out, next) => Emit(out, ignoreCommit(next))
    case s@Halt() => s
    case s@Error() => s
    case Mark(next) => Mark(ignoreCommit(next))
    case Reset(next) => Reset(ignoreCommit(next))
    case s@Commit(next, handle) => if (handle) ignoreCommit(next) else s
  }

  //
  //
  //

  def unit[O](o: O): ParserState[O] = Emit(Seq(o), Halt())

  def commit[O]: ParserState[O] = Commit(Halt(), true)

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
  def run[O](ps: ParserState[O], runState: RunState[O], log: List[ParserState[O]]): (RunResult[O], List[ParserState[O]]) = {
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

  trait RunState[O] {

    def mark: RunState[O]
    def commit: RunState[O]
    def reset: RunState[O]

    def next: RunState[O]

    def input: Option[I]
    def output(o: List[O]): RunState[O]

    def unconsumed: List[I]
    def result: List[O]
  }

  abstract class ListRunState[O](val unconsumed: List[I], val result: List[O]) extends RunState[O] {
    def mark: RunState[O] = new MarkedListRunState(unconsumed, Nil, unconsumed, this)
    def input: Option[I] = unconsumed.headOption
    def advance(newUnconsumed: List[I], out: List[O]): ListRunState[O]
  }

  class RootListRunState[O](unconsumed: List[I], result: List[O]) extends ListRunState[O](unconsumed, result) {
    def commit: RunState[O] = throw new RuntimeException("input can not be committed - it has not been marked")
    def reset: RunState[O] = throw new RuntimeException("input can not be reset - it has not been marked")
    def next: RunState[O] = unconsumed match {
      case h :: t => new RootListRunState(t, result)
      case _ => new RootListRunState(unconsumed, result)
    }
    def output(o: List[O]): RunState[O] = new RootListRunState(unconsumed, o ++ result)
    def advance(newUnconsumed: List[I], out: List[O]): ListRunState[O] = new RootListRunState[O](newUnconsumed, out ++ result)
  }

  class MarkedListRunState[O](unconsumed: List[I], result: List[O], mark: List[I], parent: ListRunState[O]) extends ListRunState[O](unconsumed, result) {
    def commit: RunState[O] = parent.advance(unconsumed, result) // result.foldRight(parent)((o, d) => d.out(o))
    def reset: RunState[O] = parent
    def next: RunState[O] = unconsumed match {
      case h :: t => new MarkedListRunState(t, result, mark, parent)
      case _ => new MarkedListRunState(unconsumed, result, mark, parent)
    }
    def output(o: List[O]): RunState[O] = new MarkedListRunState(unconsumed, o ++ result, mark, parent)
    def advance(newUnconsumed: List[I], out: List[O]): ListRunState[O] = new MarkedListRunState[O](newUnconsumed, out ++ result, mark, parent)
  }

  sealed trait RunResult[O]
  case class Success[O](out: Seq[O], unconsumed: Seq[I]) extends RunResult[O]
  case class Failure[O](out: Seq[O], unconsumed: Seq[I]) extends RunResult[O]

  //
  //
  //


}

trait CharParsers extends Parsers[Char] {

}