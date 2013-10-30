package eu.swdev.parser.push

import scala.annotation.tailrec
import java.util.regex.Pattern

/**
 */
trait PushParsers[I] {

  trait PushParser[+O] { self =>

    def push(i: I): PushResult[O]
    def flush(): FlushResult[O]

    /**
     * Pushes as much input as possible into this parser. Remaining input is returned.
     *
     * @param is list of input to consume; latest input comes first
     * @return
     */
    def pushAll(is: List[I]): PushResult[O] = {
      if (is.isEmpty) {
        Continue(false, Seq(), this)
      } else {
        pushAll(is.tail) match {
          case Continue(committed1, out1, next1) => next1.push(is.head) match {
            case Continue(committed2, out2, next2) => Continue(committed1 || committed2, out2 ++ out1, next2)
            case Return(committed2, out2, unconsumed) => Return(committed1 || committed2, out2 ++ out1, unconsumed)
            case r@FailedPush(_) => r
          }
          case Return(committed1, out1, unconsumed1) => Return(committed1, out1, is.head :: unconsumed1)
          case r@FailedPush(_) => r
        }
      }
    }

    def map[T](f: O => T): PushParser[T] = new PushParser[T] {
      def push(i: I): PushResult[T] = self.push(i) match {
        case r@Continue(_, out, next) => r.copy(out = out.map(f), next = next.map(f))
        case r@Return(_, out, _) => r.copy(out = out.map(f))
        case r@FailedPush(_) => r
      }

      def flush(): FlushResult[T] = self.flush() match {
        case r@Flushed(_, out, _) => r.copy(out = out.map(f))
        case r@FailedFlush(_) => r
      }
    }

    def >>=[T](f: O => PushParser[T]): PushParser[T] = new FlatMapPushParser(this, f)

    def ~[O1 >: O](p: => PushParser[O1]) = new AndPushParser(this, p)

    def |[O1 >: O](p: => PushParser[O1]) = new OrPushParser(this, p)

    def oneOrMore: PushParser[O] = this ~ this.many
    
    def many: PushParser[O] = (this ~ many).attempt | EmptyPushParser

    def optional: PushParser[O] = this.attempt | EmptyPushParser

    def option: PushParser[Option[O]] = this.attempt.map(Some(_)) | UnitPushParser(None)

    def attempt: PushParser[O] = new PushParser[O] {
      def push(i: I): PushResult[O] = self.push(i) match {
        case r@Continue(committed, _, next) => if (committed) r.copy(committed = false, next = next.attempt) else r
        case r@Return(committed, _, _) => if (committed) r.copy(committed = false) else r
        case r@FailedPush(committed) => if (committed) r.copy(committed = false) else r
      }
      def flush(): FlushResult[O] = self.flush() match {
        case r@Flushed(committed, _, _) => if (committed) r.copy(committed = false) else r
        case r@FailedFlush(committed) => if (committed) r.copy(committed = false) else r
      }
    }

    def commit: PushParser[O] = new PushParser[O] {
      def push(i: I): PushResult[O] = self.push(i) match {
        case r@Continue(committed, _, next) => if (!committed) r.copy(committed = true, next = next.commit) else r
        case r@Return(committed, _, _) => if (!committed) r.copy(committed = true) else r
        case r@FailedPush(committed) => if (!committed) r.copy(committed = true) else r
      }
      def flush(): FlushResult[O] = self.flush() match {
        case r@Flushed(committed, _, _) => if (!committed) r.copy(committed = true) else r
        case r@FailedFlush(committed) => if (!committed) r.copy(committed = true) else r
      }
    }

    def collect: PushParser[Seq[O]] = new CollectPushParser(this, Seq(), false)

    def ignore: PushParser[Nothing] = new PushParser[Nothing] {
      def push(i: I): PushResult[Nothing] = self.push(i) match {
        case r@Continue(_, _, next) => r.copy(out = Seq(), next = next.ignore)
        case r@Return(_, _, _) => r.copy(out = Seq())
        case r@FailedPush(_) => r
      }

      def flush(): FlushResult[Nothing] = self.flush() match {
        case r@Flushed(_, _, _) => r.copy(out = Seq())
        case r@FailedFlush(_) => r
      }
    }

    //

    def run(input: Seq[I]): RunResult[O] = {
      @tailrec
      def doRun(in: Seq[I], pp: PushParser[O], outAccu: Seq[O]): RunResult[O] = {
        if (in.isEmpty) {
          pp.flush() match {
            case Flushed(_, out, unconsumed) => RunSuccess(out ++ outAccu, unconsumed)
            case FailedFlush(_) => RunFailure(outAccu)
          }
        } else {
          pp.push(in.head) match {
            case Continue(_, out, next) => doRun(in.tail, next, out ++ outAccu)
            case Return(_, out, unconsumed) => RunSuccess(out ++ outAccu, unconsumed ++ in.tail)
            case FailedPush(_) => RunFailure(outAccu)
          }
        }
      }
      doRun(input, this, Seq())
    }

  }

  sealed trait PushResult[+O]

  case class Continue[O](
    committed: Boolean,
    out: Seq[O],
    next: PushParser[O]
  ) extends PushResult[O]

  case class Return[O](
    committed: Boolean,
    out: Seq[O],
    unconsumed: List[I]
  ) extends PushResult[O]

  case class FailedPush(
    committed: Boolean
  ) extends PushResult[Nothing]


  sealed trait FlushResult[+O]

  case class Flushed[O](
    committed: Boolean,
    out: Seq[O],
    unconsumed: List[I]) extends FlushResult[O]

  case class FailedFlush(
    committed: Boolean) extends FlushResult[Nothing]

  sealed trait RunResult[+O]

  case class RunSuccess[O](out: Seq[O], unconsumed: Seq[I]) extends RunResult[O]

  case class RunFailure[O](out: Seq[O]) extends RunResult[O]


  //
  //
  //

  def unit[O](o: O) = UnitPushParser(o)

  def ignore(input: Seq[I])(isSame: (I, I) => Boolean): PushParser[Nothing] = new IgnorePushParser(input, false)(isSame)

  //
  //
  //

  object EmptyPushParser extends PushParser[Nothing] {
    def push(i: I): PushResult[Nothing] = Return(true, Seq(), i :: Nil)
    def flush(): FlushResult[Nothing] = Flushed(true, Seq(), Nil)
  }

  case class UnitPushParser[O](o: O) extends PushParser[O] {
    def push(i: I): PushResult[O] = Return(true, Seq(o), i :: Nil)
    def flush(): FlushResult[O] = Flushed(true, Seq(o), Nil)
  }

  class AndPushParser[O](p1: PushParser[O], p2: => PushParser[O]) extends PushParser[O] {
    def push(i: I): PushResult[O] = {
      p1.push(i) match {
        case Continue(committed1, out1, next1) => Continue(committed1, out1, new AndPushParser(next1, p2))
        case Return(committed1, out1, unconsumed1) => p2.pushAll(unconsumed1) match {
          case Continue(committed2, out2, next2) => Continue(committed1 || committed2, out1 ++ out2, next2)
          case Return(committed2, out2, unconsumed2) => Return(committed1 || committed2, out1 ++ out2, unconsumed2)
          case FailedPush(committed2) => FailedPush(committed1 || committed2)
        }
        case r@FailedPush(_) => r
      }
    }

    def flush(): FlushResult[O] = {
      p1.flush() match {
        case Flushed(committed1, out1, unconsumed1) => p2.pushAll(unconsumed1) match {
          case Continue(committed2, out2, next2) => next2.flush() match {
            case Flushed(committed3, out3, unconsumed3) => Flushed(committed1 || committed2 || committed3, out3 ++ out2 ++ out1, unconsumed3)
            case r@FailedFlush(_) => r
          }
          case Return(committed2, out2, unconsumed2) => Flushed(committed1 || committed2, out2 ++ out1, unconsumed2)
          case FailedPush(committed2) => FailedFlush(committed1 || committed2)
        }
        case r@FailedFlush(_) => r
      }
      
    }
  }

  class OrPushParser[O](p1: PushParser[O], p2: => PushParser[O]) extends PushParser[O] {
    def push(i: I): PushResult[O] = p1.push(i) match {
      case r@Continue(committed, _, _) => if (committed) r else Continue(false, Seq(), Recorder(List((i, r))))
      case r@Return(_, _, _) => r
      case r@FailedPush(committed) => if (committed) r else p2.push(i)
    }

    def flush(): FlushResult[O] = p1.flush() match {
      case r@Flushed(_, _, _) => r
      case r@FailedFlush(committed) => if (committed) r else p2.flush()
    }

    case class Recorder(rec: List[(I, Continue[O])]) extends PushParser[O] {
      def push(i: I): PushResult[O] = rec.head._2.next.push(i) match {
        case r@Continue(committed, out, next) => if (committed) {
          Continue(true, out ++ recordedOut, next)
        } else {
          Continue(false, Seq(), Recorder((i, r) :: rec))
        }
        case Return(committed, out, unconsumed) => Return(committed, out ++ recordedOut, unconsumed)
        case r@FailedPush(committed) => if (committed) r else p2.pushAll(i :: rec.map(_._1))
      }

      def flush(): FlushResult[O] = {
        rec.head._2.next.flush() match {
          case Flushed(committed, out, unconsumed) => Flushed(committed, out ++ recordedOut, unconsumed)
          case r@FailedFlush(_) => r
        }
      }

      private def recordedOut: Seq[O] = rec.flatMap(_._2.out) //rec.foldRight(Seq[O]())((c, s) => c._2.out ++ s)

    }
  }

  class CollectPushParser[O](p: PushParser[O], collected: Seq[O], committed: Boolean) extends PushParser[Seq[O]] {
    def push(i: I): PushResult[Seq[O]] = p.push(i) match {
      case r@Continue(com, out, next) => Continue(committed || com, Seq(), new CollectPushParser(next, out ++ collected, committed || com))
      case r@Return(com, out, unconsumed) => Return(committed || com, Seq(out ++ collected), unconsumed)
      case r@FailedPush(com) => r.copy(committed = committed || com)
    }
    def flush(): FlushResult[Seq[O]] = p.flush() match {
      case r@Flushed(com, out, unconsumed) => Flushed(committed || com, Seq(out ++ collected), unconsumed)
      case r@FailedFlush(com) => r.copy(committed = committed || com)
    }
  }

  class FlatMapPushParser[O, T](p: PushParser[O], f: O => PushParser[T]) extends PushParser[T] {
    def fMap(os: Seq[O]): PushParser[T] = os match {
      case h :: t => f(h) ~ fMap(t)
      case _ => EmptyPushParser
    }
    def push(i: I): PushResult[T] = p.push(i) match {
      case r@Continue(committed, out, next) => if (out.isEmpty) {
        Continue(committed, Seq(), new FlatMapPushParser(next, f))
      } else {
        Continue(committed, Seq(), fMap(out) ~ new FlatMapPushParser(next, f))
      }
      case r@Return(committed, out, unconsumed) => if (out.isEmpty) {
        Return(committed, Seq(), unconsumed)
      } else {
        val ppb = fMap(out)
        val ppb2 = if (committed) ppb.commit else ppb
        ppb2.pushAll(unconsumed)
      }
      case r@FailedPush(_) => r
    }

    def flush(): FlushResult[T] = p.flush() match {
      case r@Flushed(committed1, out1, unconsumed1) => if (out1.isEmpty) {
        r.copy(out = Seq())
      } else {
        val ppb = fMap(out1)
        val ppb2 = if (committed1) ppb.commit else ppb
        ppb2.pushAll(unconsumed1) match {
          case Continue(committed2, out2, next2) => next2.flush() match {
            case Flushed(committed3, out3, unconsumed3) => Flushed(committed1 || committed2 || committed3, out3 ++ out2, unconsumed3)
            case FailedFlush(committed3) => FailedFlush(committed1 || committed2 || committed3)
          }
          case Return(committed2, out2, unconsumed2) => Flushed(committed1 || committed2, out2, unconsumed2)
          case r@FailedPush(committed2) => FailedFlush(committed1 || committed2)
        }
      }
      case r@FailedFlush(_) => r
    }

  }

  /**
   * A parser that accepts a specified sequence of items and produces no output.
   *
   * When at least one input item was consumed then the parser returns results with the committed flag set to true.
   *
   * @param ignore
   * @param committed
   */
  class IgnorePushParser(ignore: Seq[I], committed: Boolean)(isSame: (I, I) => Boolean) extends PushParser[Nothing] {
    def push(i: I): PushResult[Nothing] = if (ignore.isEmpty) {
      Return(committed, Seq(), i :: Nil)
    } else if (isSame(i, ignore.head)) {
      if (ignore.tail.isEmpty) {
        Return(true, Seq(), Nil)
      } else {
        Continue(true, Seq(), new IgnorePushParser(ignore.tail, true)(isSame))
      }
    } else {
      FailedPush(committed)
    }

    def flush(): FlushResult[Nothing] = if (ignore.isEmpty) {
      Flushed(committed, Seq(), Nil)
    } else {
      FailedFlush(committed)
    }
  }

  class EchoPushParser(ignore: Seq[I], committed: Boolean) extends PushParser[I] {
    def push(i: I): PushResult[I] = if (ignore.isEmpty) {
      Return(committed, Seq(), i :: Nil)
    } else if (i == ignore.head) {
      if (ignore.tail.isEmpty) {
        Return(true, Seq(i), Nil)
      } else {
        Continue(true, Seq(i), new EchoPushParser(ignore.tail, true))
      }
    } else {
      FailedPush(committed)
    }

    def flush(): FlushResult[I] = if (ignore.isEmpty) {
      Flushed(committed, Seq(), Nil)
    } else {
      FailedFlush(committed)
    }
  }

  //
  //
  //

}

class CharPushParsers[C](charInput: C => Char) extends PushParsers[C] {

  import scala.util.matching.Regex

  class PatternPushParser(regex: String, greedy: Boolean) extends PushParser[String] {

    val pattern = Pattern.compile(regex)

    def push(i: C): PushResult[String] = {
      if (pattern.matcher("").matches()) {
        new Recorder(new StringBuilder, Some(0), Nil).push(i)
      } else {
        new Recorder(new StringBuilder, None, Nil).push(i)
      }
    }

    def flush(): FlushResult[String] = {
      if (pattern.matcher("").matches()) {
        Flushed(true, Seq(""), Nil)
      } else {
        FailedFlush(false)
      }

    }

    class Recorder(sb: StringBuilder, var foundMatch: Option[Int], var unconsumed: List[C]) extends PushParser[String] {
      
      def retReturn(idx: Int): Return[String] = Return(true, Seq(sb.substring(0, idx)), unconsumed)
      def retFlushed(idx: Int): Flushed[String] = Flushed(true, Seq(sb.substring(0, idx)), unconsumed)
      
      def push(i: C): PushResult[String] = {
        sb.append(charInput(i))
        val matcher = pattern.matcher(sb)
        if (matcher.matches()) {
          unconsumed = Nil
          if (greedy) {
            // greedy: continue, maybe the regex matches even more characters
            foundMatch = Some(sb.length)
            Continue(true, Seq(), this)
          } else {
            Return(true, Seq(sb.toString()), Nil)
          }
        } else {
          // it does not match
          unconsumed = i :: unconsumed
          if (matcher.hitEnd()) {
            // maybe after some more characters it matches
            if (greedy || foundMatch.isEmpty) {
              Continue(true, Seq(), this)
            } else {
              // return until the last found match
              retReturn(foundMatch.get)
            }
          } else {
            // it will never match
            foundMatch.map(retReturn(_)).getOrElse(FailedPush(true))
          }
        }
      }

      def flush(): FlushResult[String] = foundMatch.map(retFlushed(_)).getOrElse(FailedFlush(true))
    }
  }


}
