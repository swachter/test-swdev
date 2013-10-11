package test.scala.eu.swdev.parser.push

import main.scala.eu.swdev.parser.push.CharPushParsers
import org.scalatest.FunSuite
import scala.annotation.tailrec

class PushParsersTest extends FunSuite {

  val parsers = new CharPushParsers {

    val _unit = unit('a')

    val _aOrb = "a" | "b"

    def drive[O](input: Seq[Char], pp: PushParser[O]): RunResult[O] = {
      @tailrec
      def doRun(in: Seq[Char], pp: PushParser[O], outAccu: Seq[O]): RunResult[O] = {
        if (in.isEmpty) {
          pp.flush() match {
            case Flushed(_, out) => RunSuccess(out ++ outAccu, Seq())
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
      doRun(input, pp, Seq())
    }

    def drive[O](input: String, pp: PushParser[O]): RunResult[O] = {
      val in: Seq[Char] = input
      drive(in, pp)
    }

  }

  import parsers._

  test("unit") {
    assert(_unit.flush() === Flushed(true, List('a')))
  }

  test("aOrB") {
    assert(_aOrb.run("a") === RunSuccess(Seq(), Seq()))
    assert(_aOrb.run("b") === RunSuccess(Seq(), Seq()))
    assert(_aOrb.run("bbc") === RunSuccess(Seq(), Seq('b', 'c')))
  }

}

