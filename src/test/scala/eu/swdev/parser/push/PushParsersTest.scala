package test.scala.eu.swdev.parser.push

import main.scala.eu.swdev.parser.push.CharPushParsers
import org.scalatest.FunSuite
import scala.annotation.tailrec

class PushParsersTest extends FunSuite {

  val parsers = new CharPushParsers {

    val _unit = unit('a')

    val _aOrB = "a" | "b"
    
    val _aAndB = "a" ~ "b"

    val _manyA = "a".many

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
    assert(_aOrB.run("a") === RunSuccess(Seq(), Seq()))
    assert(_aOrB.run("b") === RunSuccess(Seq(), Seq()))
    assert(_aOrB.run("bbc") === RunSuccess(Seq(), Seq('b', 'c')))
    assert(_aOrB.run("x") === RunFailure(Seq()))
  }

  test("aAndB") {
    assert(_aAndB.run("ab") === RunSuccess(Seq(), Seq()))
    assert(_aAndB.run("abc") === RunSuccess(Seq(), Seq('c')))
    assert(_aAndB.run("ac") === RunFailure(Seq()))
    assert(_aAndB.run("b") === RunFailure(Seq()))
  }

  test("manyA") {
    assert(_manyA.run("") === RunSuccess(Seq(), Seq()))
    assert(_manyA.run("a") === RunSuccess(Seq(), Seq()))
    assert(_manyA.run("aa") === RunSuccess(Seq(), Seq()))
    assert(_manyA.run("b") === RunSuccess(Seq(), Seq('b')))
    assert(_manyA.run("ab") === RunSuccess(Seq(), Seq('b')))
    assert(_manyA.run("aab") === RunSuccess(Seq(), Seq('b')))
  }


}

