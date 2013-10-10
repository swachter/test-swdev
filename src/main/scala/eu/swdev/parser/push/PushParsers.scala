package main.scala.eu.swdev.parser.push

/**
 */
trait PushParsers[I] {

  trait PushParser[O] { self =>

    def push(i: I): PushResult[O]
    def flush(): FlushResult[O]

    /**
     * Pushes as much input as possible into this parser. Remaining input is returned in an Unconsumed result
     *
     * @param is list of input to consume; latest input comes first
     * @return
     */
    def pushAll(is: List[I]): PushResult[O] = {
      if (is.isEmpty) {
        Consumed(Seq(), false, this)
      } else {
        pushAll(is.tail) match {
          case Consumed(out1, committed1, next1) => next1.push(is.head) match {
            case Consumed(out2, committed2, next2) => Consumed(out1 ++ out2, committed1 || committed2, next2)
            case Unconsumed(out2, committed2, unconsumed) => Unconsumed(out1 ++ out2, committed1 || committed2, unconsumed)
            case r@FailedPush(_) => r
          }
          case Unconsumed(out1, committed1, unconsumed1) => Unconsumed(out1, committed1, is.head :: unconsumed1)
          case r@FailedPush(_) => r
        }
      }
    }

    def repeat: PushParser[O] = OrPushParser(AndPushParser(this, repeat), EmptyPushParser)

    def optional: PushParser[O] = OrPushParser(this, EmptyPushParser)

    def attempt: PushParser[O] = new PushParser[O] {
      def push(i: I): PushResult[O] = self.push(i) match {
        case r@Consumed(_, committed, _) => if (committed) r.copy(committed = false) else r
        case r@Unconsumed(_, committed, _) => if (committed) r.copy(committed = false) else r
        case r@FailedPush(committed) => if (committed) r.copy(committed = false) else r
      }
      def flush(): FlushResult[O] = self.flush()
    }

  }

  sealed trait PushResult[O]

  case class Consumed[O](
    out: Seq[O],
    committed: Boolean,
    next: PushParser[O]
  ) extends PushResult[O]

  case class Unconsumed[O](
    out: Seq[O],
    committed: Boolean,
    unconsumed: List[I]
  ) extends PushResult[O]

  case class FailedPush[O](
    committed: Boolean
  ) extends PushResult[O]


  sealed trait FlushResult[O]
  case class Flushed[O](committed: Boolean, out: Seq[O]) extends FlushResult[O]
  case class FailedFlush[O](committed: Boolean) extends FlushResult[O]

  object EmptyPushParser extends PushParser[Nothing] {
    def push(i: I): PushResult[Nothing] = Unconsumed(Seq(), false, Nil)
    def flush(): FlushResult[Nothing] = Flushed(true, Seq())
  }

  case class AndPushParser[O](p1: PushParser[O], p2: => PushParser[O]) extends PushParser[O] {
    def push(i: I): PushResult[O] = {
      p1.push(i) match {
        case Consumed(out1, committed1, next1) => Consumed(out1, committed1, AndPushParser(next1, p2))
        case Unconsumed(out1, committed1, unconsumed1) => p2.push(i) match {
          case Consumed(out2, committed2, next2) => Consumed(out1 ++ out2, committed1 || committed2, next2)
          case Unconsumed(out2, committed2, unconsumed2) => Unconsumed(out1 ++ out2, committed1 || committed2, unconsumed2)
          case FailedPush(committed2) => FailedPush(committed1 || committed2)
        }
        case r@FailedPush => r
      }
    }

    def flush(): FlushResult[O] = {
      p1.flush() match {
        case Flushed(committed1, out1) => p2.flush() match {
          case Flushed(committed2, out2) => Flushed(committed1 || committed2, out1 ++ out2)
          case r@FailedFlush => r
        }
        case r@FailedFlush => r
      }
      
    }
  }

  case class OrPushParser[O](p1: PushParser[O], p2: => PushParser[O]) extends PushParser[O] {
    def push(i: I): PushResult[O] = p1.push(i) match {
      case r@Consumed(_, committed, _) => if (committed) r else Consumed(Seq(), false, Recorder(List((i, r))))
      case r@Unconsumed => r
      case r@FailedPush(committed) => if (committed) r else p2.push(i)
    }

    def flush(): FlushResult[O] = p1.flush() match {
      case r@Flushed(_, _) => r
      case r@FailedFlush(committed) => if (committed) r else p2.flush()
    }

    case class Recorder(rec: List[(I, Consumed[O])]) extends PushParser[O] {
      def push(i: I): PushResult[O] = rec.head._2.next.push(i) match {
        case r@Consumed(out, committed, next) => if (committed) {
          Consumed(out ++ recordedOut, true, next)
        } else {
          Consumed(Seq(), false, Recorder((i, r) :: rec))
        }
        case Unconsumed(out, committed, unconsumed) => Unconsumed(out ++ recordedOut, committed, unconsumed)
        case r@FailedPush(committed) => if (committed) r else p2.pushAll(i :: rec.map(_._1))
      }

      def flush(): FlushResult[O] = {
        rec.head._2.next.flush() match {
          case Flushed(committed, out) => Flushed(committed, out ++ recordedOut)
          case r@FailedFlush(_) => r
        }
      }

      private def recordedOut: Seq[O] = rec.foldRight(Seq[O]())((c, s) => c._2.out ++ s)

    }
  }
}
