package main.scala.eu.swdev.parser.push

/**
 */
trait PushParsers[I] {

  trait PushParser[+O] { self =>

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
        Continue(Seq(), false, this)
      } else {
        pushAll(is.tail) match {
          case Continue(out1, committed1, next1) => next1.push(is.head) match {
            case Continue(out2, committed2, next2) => Continue(out1 ++ out2, committed1 || committed2, next2)
            case Return(out2, committed2, unconsumed) => Return(out1 ++ out2, committed1 || committed2, unconsumed)
            case r@FailedPush(_) => r
          }
          case Return(out1, committed1, unconsumed1) => Return(out1, committed1, is.head :: unconsumed1)
          case r@FailedPush(_) => r
        }
      }
    }

    def map[T](f: O => T) = new PushParser[T] {
      def push(i: I): PushResult[T] = self.push(i) match {
        case r@Continue(out, _, _) => r.copy(out = out.map(f))
        case r@Return(out, _, _) => r.copy(out = out.map(f))
        case r@FailedPush(_) => r
      }

      def flush(): FlushResult[T] = self.flush() match {
        case r@Flushed(_, out) => r.copy(out = out.map(f))
        case r@FailedFlush(_) => r
      }
    }

    def flatMap[T](f: O => PushParser[T]) = FlatMapPushParser(this, f)

    def ~[O1 >: O](p: => PushParser[O1]) = AndPushParser(this, p)

    def |[O1 >: O](p: => PushParser[O1]) = OrPushParser(this, p)

    def oneOrMore: PushParser[O] = this ~ this.many
    
    def many: PushParser[O] = (this ~ many) | EmptyPushParser

    def optional: PushParser[O] = OrPushParser(this, EmptyPushParser)

    def attempt: PushParser[O] = new PushParser[O] {
      def push(i: I): PushResult[O] = self.push(i) match {
        case r@Continue(_, committed, _) => if (committed) r.copy(committed = false) else r
        case r@Return(_, committed, _) => if (committed) r.copy(committed = false) else r
        case r@FailedPush(committed) => if (committed) r.copy(committed = false) else r
      }
      def flush(): FlushResult[O] = self.flush() match {
        case r@Flushed(committed, _) => if (committed) r.copy(committed = false) else r
        case r@FailedFlush(committed) => if (committed) r.copy(committed = false) else r
      }
    }

    def commit: PushParser[O] = new PushParser[O] {
      def push(i: I): PushResult[O] = self.push(i) match {
        case r@Continue(_, committed, _) => if (!committed) r.copy(committed = true) else r
        case r@Return(_, committed, _) => if (!committed) r.copy(committed = true) else r
        case r@FailedPush(committed) => if (!committed) r.copy(committed = true) else r
      }
      def flush(): FlushResult[O] = self.flush() match {
        case r@Flushed(committed, _) => if (!committed) r.copy(committed = true) else r
        case r@FailedFlush(committed) => if (!committed) r.copy(committed = true) else r
      }
    }

    def collect: PushParser[Seq[O]] = CollectPushParser(this, Seq(), false)

    def ignore: PushParser[Nothing] = new PushParser[Nothing] {
      def push(i: I): PushResult[Nothing] = self.push(i) match {
        case r@Continue(_,_,_) => r.copy(out = Seq())
        case r@Return(_, _, _) => r.copy(out = Seq())
        case r@FailedPush(_) => r
      }

      def flush(): FlushResult[Nothing] = self.flush() match {
        case r@Flushed(_, _) => r.copy(out = Seq())
        case r@FailedFlush(_) => r
      }
    }

  }

  sealed trait PushResult[+O]

  case class Continue[O](
    out: Seq[O],
    committed: Boolean,
    next: PushParser[O]
  ) extends PushResult[O]

  case class Return[O](
    out: Seq[O],
    committed: Boolean,
    unconsumed: List[I]
  ) extends PushResult[O]

  case class FailedPush(
    committed: Boolean
  ) extends PushResult[Nothing]


  sealed trait FlushResult[+O]
  case class Flushed[O](committed: Boolean, out: Seq[O]) extends FlushResult[O]
  case class FailedFlush(committed: Boolean) extends FlushResult[Nothing]

  object EmptyPushParser extends PushParser[Nothing] {
    def push(i: I): PushResult[Nothing] = Return(Seq(), false, Nil)
    def flush(): FlushResult[Nothing] = Flushed(true, Seq())
  }

  case class AndPushParser[O](p1: PushParser[O], p2: => PushParser[O]) extends PushParser[O] {
    def push(i: I): PushResult[O] = {
      p1.push(i) match {
        case Continue(out1, committed1, next1) => Continue(out1, committed1, AndPushParser(next1, p2))
        case Return(out1, committed1, unconsumed1) => p2.push(i) match {
          case Continue(out2, committed2, next2) => Continue(out1 ++ out2, committed1 || committed2, next2)
          case Return(out2, committed2, unconsumed2) => Return(out1 ++ out2, committed1 || committed2, unconsumed2)
          case FailedPush(committed2) => FailedPush(committed1 || committed2)
        }
        case r@FailedPush(_) => r
      }
    }

    def flush(): FlushResult[O] = {
      p1.flush() match {
        case Flushed(committed1, out1) => p2.flush() match {
          case Flushed(committed2, out2) => Flushed(committed1 || committed2, out1 ++ out2)
          case r@FailedFlush(_) => r
        }
        case r@FailedFlush(_) => r
      }
      
    }
  }

  case class OrPushParser[O](p1: PushParser[O], p2: => PushParser[O]) extends PushParser[O] {
    def push(i: I): PushResult[O] = p1.push(i) match {
      case r@Continue(_, committed, _) => if (committed) r else Continue(Seq(), false, Recorder(List((i, r))))
      case r@Return(_,_,_) => r
      case r@FailedPush(committed) => if (committed) r else p2.push(i)
    }

    def flush(): FlushResult[O] = p1.flush() match {
      case r@Flushed(_, _) => r
      case r@FailedFlush(committed) => if (committed) r else p2.flush()
    }

    case class Recorder(rec: List[(I, Continue[O])]) extends PushParser[O] {
      def push(i: I): PushResult[O] = rec.head._2.next.push(i) match {
        case r@Continue(out, committed, next) => if (committed) {
          Continue(out ++ recordedOut, true, next)
        } else {
          Continue(Seq(), false, Recorder((i, r) :: rec))
        }
        case Return(out, committed, unconsumed) => Return(out ++ recordedOut, committed, unconsumed)
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

  case class CollectPushParser[O](p: PushParser[O], collected: Seq[O], committed: Boolean) extends PushParser[Seq[O]] {
    def push(i: I): PushResult[Seq[O]] = p.push(i) match {
      case r@Continue(out, com, next) => Continue(Seq(), committed || com, CollectPushParser(next, out ++ collected, committed || com))
      case r@Return(out, com, unconsumed) => Return(Seq(out ++ collected), committed || com, unconsumed)
      case r@FailedPush(com) => r.copy(committed = committed || com)
    }
    def flush(): FlushResult[Seq[O]] = p.flush() match {
      case r@Flushed(com, out) => Flushed(committed || com, Seq(out ++ collected))
      case r@FailedFlush(com) => r.copy(committed = committed || com)
    }
  }

  case class FlatMapPushParser[O, T](p: PushParser[O], f: O => PushParser[T]) extends PushParser[T] {
    def push(i: I): PushResult[T] = p.push(i) match {
      case r@Continue(out, committed, next) => if (out.isEmpty) {
        Continue(Seq(), committed, FlatMapPushParser(next, f))
      } else {
        throw new RuntimeException("a parser that outputs more than one token can not be flatMapped")
      }
      case r@Return(out, committed, unconsumed) => if (out.isEmpty) {
        Return(Seq(), committed, unconsumed)
      } else {
        if (!out.tail.isEmpty) {
          throw new RuntimeException("a parser that outputs more than one token can not be flatMapped")
        }
        val o = out.head
        val ppb = f(o)
        val ppb2 = if (committed) ppb.commit else ppb
        ppb2.pushAll(unconsumed)
      }
      case r@FailedPush(_) => r
    }

    def flush(): FlushResult[T] = p.flush() match {
      case r@Flushed(committed, out) => if (out.isEmpty) {
        Flushed(committed, Seq())
      } else {
        if (!out.tail.isEmpty) {
          throw new RuntimeException("a parser that outputs more than one token can not be flatMapped")
        } else {
          val o = out.head
          val ppb = f(o)
          val ppb2 = if (committed) ppb.commit else ppb
          ppb2.pushAll()

        }
      }
    }

  }
}
