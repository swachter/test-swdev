package eu.swdev.parser.push

trait Parsers[I] {

  sealed trait ParserState[+O] { self =>

    def ~[O1 >: O](p: => ParserState[O1]): ParserState[O1] = this match {
      case Await(push, flush) => Await(push andThen (_ ~ p), flush ~ p)
      case Emit(out, next) => Emit(out, next ~ p)
      case Halt() => p
      case s@Error() => s
      case Mark(next) => Mark(next ~ p)
      case Reset(next) => Reset(next ~ p)
      case Commit(next) => Commit(next ~ p)
    }

    def |[O1 >: O](p: => ParserState[O1]): ParserState[O1] = Mark(this ||| p)
    def or[O1 >: O](p: => ParserState[O1]): ParserState[O1] = this | p

    private def |||[O1 >: O](p: => ParserState[O1]): ParserState[O1] = this match {
      case Await(push, flush) => Await(push andThen (_ ||| p), flush ||| p)
      case Emit(out, next) => Emit(out, next ||| p)
      case Halt() => Commit(Halt())
      case Error() => Reset(p)
      case Mark(next) => Mark(next ||| p)
      case Reset(next) => Reset(next ||| p)
      case Commit(next) => Commit(next)
    }

    def many: ParserState[O] = (this ~ many) | Halt()

    def oneOrMore: ParserState[O] = this ~ many

  }

  case class Await[O](push: I => ParserState[O], flush: ParserState[O]) extends ParserState[O]

  case class Emit[O](out: Seq[O], next: ParserState[O]) extends ParserState[O]

  case class Halt[O]() extends ParserState[O]

  case class Error[O]() extends ParserState[O]

  case class Mark[O](next: ParserState[O]) extends ParserState[O]

  case class Reset[O](next: ParserState[O]) extends ParserState[O]

  case class Commit[O](next: ParserState[O]) extends ParserState[O]

  //
  //
  //

  def unit[O](o: O): ParserState[O] = Emit(Seq(o), Halt())

  //
  //
  //

  def run[O](ps: ParserState[O], runState: RunState[O], log: List[ParserState[O]]): (RunResult[O], List[ParserState[O]]) = {
    ps match {
      case Await(push, flush) => runState.in match {
        case Some(i) => run(push(i), runState.next, ps :: log)
        case None => run(flush, runState, ps :: log)
      }
      case Emit(out, next) => run(next, runState.output(out.to[List]), ps :: log)
      case Halt() => (Success(runState.result.reverse, runState.unconsumed), ps :: log)
      case Error() => (Failure(runState.result.reverse, runState.unconsumed), ps :: log)
      case Mark(next) => run(next, runState.mark, ps :: log)
      case Reset(next) => run(next, runState.reset, ps :: log)
      case Commit(next) => run(next, runState.commit, ps :: log)
    }
  }

  trait RunState[O] {

    def mark: RunState[O]
    def commit: RunState[O]
    def reset: RunState[O]

    def next: RunState[O]

    def in: Option[I]
    def output(o: List[O]): RunState[O]

    def unconsumed: List[I]
    def result: List[O]
  }

  abstract class ListRunState[O](val unconsumed: List[I], val result: List[O]) extends RunState[O] {
    def mark: RunState[O] = new MarkedListRunState(unconsumed, Nil, unconsumed, this)
    def in: Option[I] = unconsumed.headOption
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