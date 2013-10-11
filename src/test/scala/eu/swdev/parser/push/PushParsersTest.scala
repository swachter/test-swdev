package test.scala.eu.swdev.parser.push

import main.scala.eu.swdev.parser.push.CharPushParsers
import org.scalatest.FunSuite
import scala.annotation.tailrec

class PushParsersTest extends FunSuite {

  val parsers = new CharPushParsers {

    implicit def pushParser(string: String): PushParser[Char] = new EchoPushParser(string, false)

    implicit class StrOps(string: String) {
      def p: PushParser[String] = new PatternPushParser(string, true)
      def q: PushParser[String] = new PatternPushParser(string, false)
    }

    val _unit = unit('a')

    val _aOrB = "a" | "b"

    val _aabOrAb = "aab".attempt | "ab"

    val _aAndB = "a" ~ "b"

    val _manyA = "a".many

    val _oneOrMoreA = "a".oneOrMore

    val _optionalA = "a".optional

    val _optionA = "a".option

    val _aBindB = "a" >>= ((a: Char) => "b" >>= ((b: Char) => unit((a,b))))

    val _greedyAs = "a*".p

    val _reluctantAs = "a*".q

    val _yesOrNo = "yes|no".p
  }

  import parsers._

  test("unit") {
    assert(_unit.flush() === Flushed(true, List('a'), Nil))
  }

  test("aOrB") {
    assert(_aOrB.run("a") === RunSuccess(Seq('a'), Seq()))
    assert(_aOrB.run("b") === RunSuccess(Seq('b'), Seq()))
    assert(_aOrB.run("bbc") === RunSuccess(Seq('b'), Seq('b', 'c')))
    assert(_aOrB.run("x") === RunFailure(Seq()))
  }

  test("aabOrAb") {
    assert(_aabOrAb.run("aab") === RunSuccess(Seq('b', 'a', 'a'), Seq()))
    assert(_aabOrAb.run("ab") === RunSuccess(Seq('b', 'a'), Seq()))
    assert(_aabOrAb.run("abc") === RunSuccess(Seq('b', 'a'), Seq('c')))
    assert(_aabOrAb.run("aabc") === RunSuccess(Seq('b', 'a', 'a'), Seq('c')))
  }

  test("aAndB") {
    assert(_aAndB.run("ab") === RunSuccess(Seq('b', 'a'), Seq()))
    assert(_aAndB.run("abc") === RunSuccess(Seq('b', 'a'), Seq('c')))
    assert(_aAndB.run("ac") === RunFailure(Seq('a')))
    assert(_aAndB.run("b") === RunFailure(Seq()))
  }

  test("manyA") {
    assert(_manyA.run("") === RunSuccess(Seq(), Seq()))
    assert(_manyA.run("a") === RunSuccess(Seq('a'), Seq()))
    assert(_manyA.run("aa") === RunSuccess(Seq('a', 'a'), Seq()))
    assert(_manyA.run("b") === RunSuccess(Seq(), Seq('b')))
    assert(_manyA.run("ab") === RunSuccess(Seq('a'), Seq('b')))
    assert(_manyA.run("aab") === RunSuccess(Seq('a', 'a'), Seq('b')))
  }

  test("oneOreMoreA") {
    assert(_oneOrMoreA.run("") === RunFailure(Seq()))
    assert(_oneOrMoreA.run("a") === RunSuccess(Seq('a'), Seq()))
    assert(_oneOrMoreA.run("aa") === RunSuccess(Seq('a', 'a'), Seq()))
    assert(_oneOrMoreA.run("b") === RunFailure(Seq()))
    assert(_oneOrMoreA.run("ab") === RunSuccess(Seq('a'), Seq('b')))
    assert(_oneOrMoreA.run("aab") === RunSuccess(Seq('a', 'a'), Seq('b')))
  }

  test("optionalA") {
    assert(_optionalA.run("") === RunSuccess(Seq(), Seq()))
    assert(_optionalA.run("a") === RunSuccess(Seq('a'), Seq()))
    assert(_optionalA.run("b") === RunSuccess(Seq(), Seq('b')))
  }

  test("optionA") {
    assert(_optionA.run("") === RunSuccess(Seq(None), Seq()))
    assert(_optionA.run("a") === RunSuccess(Seq(Some('a')), Seq()))
    assert(_optionA.run("b") === RunSuccess(Seq(None), Seq('b')))
  }

  test("aBindB") {
    assert(_aBindB.run("ab") === RunSuccess(Seq(('a', 'b')), Seq()))
    assert(_aBindB.run("abc") === RunSuccess(Seq(('a', 'b')), Seq('c')))
    assert(_aBindB.run("ac") === RunFailure(Seq()))
    assert(_aBindB.run("b") === RunFailure(Seq()))
  }

  test("greedyAs") {
    assert(_greedyAs.run("") === RunSuccess(Seq(""), Seq()))
    assert(_greedyAs.run("a") === RunSuccess(Seq("a"), Seq()))
    assert(_greedyAs.run("aa") === RunSuccess(Seq("aa"), Seq()))
    assert(_greedyAs.run("aab") === RunSuccess(Seq("aa"), Seq('b')))
    assert(_greedyAs.run("b") === RunSuccess(Seq(""), Seq('b')))
  }

  test("reluctantAs") {
    assert(_reluctantAs.run("") === RunSuccess(Seq(""), Seq()))
    assert(_reluctantAs.run("a") === RunSuccess(Seq("a"), Seq()))
    assert(_reluctantAs.run("aa") === RunSuccess(Seq("a"), Seq('a')))
    assert(_reluctantAs.run("aab") === RunSuccess(Seq("a"), Seq('a', 'b')))
    assert(_reluctantAs.run("b") === RunSuccess(Seq(""), Seq('b')))
  }

  test("yesOrNo") {
    assert(_yesOrNo.run("yes") === RunSuccess(Seq("yes"), Seq()))
    assert(_yesOrNo.run("no") === RunSuccess(Seq("no"), Seq()))
    assert(_yesOrNo.run("yesx") === RunSuccess(Seq("yes"), Seq('x')))
    assert(_yesOrNo.run("y") === RunFailure(Seq()))
  }

}

