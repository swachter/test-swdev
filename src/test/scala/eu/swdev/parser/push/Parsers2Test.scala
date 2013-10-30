package eu.swdev.parser.push

import org.scalatest.FunSuite
import java.util.regex.Pattern

/**
  */
class Parsers2Test extends FunSuite {

  val parsers = new CharParsers2 {

    type PS = ParserState[Char, Char]

    val _AandB = 'a' ~ 'b'

    val _AorB: PS = 'a' or 'b'

    val _AorBorC1: PS = ('a' or 'b') or 'c'
    val _AorBorC2: PS = 'a' or ('b' or 'c')

    val _ABorA = ('a' ~ 'b') or 'a'

    val _AcommitBorA = ('a' ~ commit ~ 'b') or 'a'

    val _AcommitBorAorA = (('a' ~ commit ~ 'b') or 'a') or 'a'

    val _AABorAB = ('a' ~ 'a' ~ 'b') or ('a' ~ 'b')

    val _Amany = 'a'.many

    val _AoneOrMore = 'a'.oneOrMore

    val _Aoptional = 'a'?

    val _Aoption = 'a'.option

    val _AbindB: ParserState[Char, (Char, Char)] = 'a' >>= (a => 'b' >>= (b => unit((a, b))))

    val _ApipeA = 'a' |> 'a'

    val _AorBpipeAorB = ('a' or 'b') |> ('a' or 'b')

    val _AorBpipeABorA = ('a' or 'b').many |> (('a' ~ 'b') or 'a')

    val _ANYpipeABorA = any |> (('a' ~ 'b') or 'a')

    val _ANYpipeABorAandC = any |> ((('a' ~ 'b') or 'a') ~ 'c')

    val _greedyAs = "a*".p

    val _nonGreedyAs = "a*".q

    val _yesOrNo = "yes|no".p

    implicit def parser(char: Char): PS = Await(c => if (c == char) Emit(Seq(c), Halt()) else Error(), Error())

    implicit class StrOps(string: String) {
      def p: ParserState[Char, String] = regexParser(Pattern.compile(string), true)
      def q: ParserState[Char, String] = regexParser(Pattern.compile(string), false)
    }

    def any: PS = Await((c: Char) => Emit(Seq(c), any), Halt())

    def drive[O](ps: ParserState[Char, O], string: String): RunResult[Char, O] = {
      val (rr, log) = run(ps, new RootListRunState[Char, O](string.to[List], Nil), Nil)
      println(log)
      rr
    }
  }

  import parsers._

  test("a~b") {
    assert(drive(_AandB, "ab") === Success(Seq('a', 'b'), Seq()))
    assert(drive(_AandB, "abc") === Success(Seq('a', 'b'), Seq('c')))
    assert(drive(_AandB, "ac") === Failure(Seq('a'), Seq()))
    assert(drive(_AandB, "b") === Failure(Seq(), Seq()))
  }

  test("a|b") {
    assert(drive(_AorB, "a") === Success(Seq('a'), Seq()))
    assert(drive(_AorB, "b") === Success(Seq('b'), Seq()))
    assert(drive(_AorB, "bbc") === Success(Seq('b'), Seq('b', 'c')))
    assert(drive(_AorB, "x") === Failure(Seq(), Seq()))
  }

  test("(a|b)|c") {
    AorBorC(parsers._AorBorC1.asInstanceOf[parsers.PS])
  }

  test("a|(b|c)") {
    AorBorC(parsers._AorBorC2.asInstanceOf[parsers.PS])
  }

  test("ab|a") {
    assert(drive(_ABorA, "ab") === Success(Seq('a', 'b'), Seq()))
    assert(drive(_ABorA, "a") === Success(Seq('a'), Seq()))
    assert(drive(_ABorA, "abc") === Success(Seq('a', 'b'), Seq('c')))
    assert(drive(_ABorA, "ac") === Success(Seq('a'), Seq('c')))
    assert(drive(_ABorA, "c") === Failure(Seq(), Seq()))
  }

  test("a{commit}b|a") {
    assert(drive(_AcommitBorA, "ab") === Success(Seq('a', 'b'), Seq()))
    assert(drive(_AcommitBorA, "a") === Failure(Seq('a'), Seq()))
    assert(drive(_AcommitBorA, "abc") === Success(Seq('a', 'b'), Seq('c')))
    assert(drive(_AcommitBorA, "ac") === Failure(Seq('a'), Seq()))
    assert(drive(_AcommitBorA, "c") === Failure(Seq(), Seq()))
  }

  test("(a{commit}b|a)|a") {
    // ensure that a commit in an inner choice does not prevent the second alternativ of an outer choice
    assert(drive(_AcommitBorAorA, "ab") === Success(Seq('a', 'b'), Seq()))
    assert(drive(_AcommitBorAorA, "a") === Success(Seq('a'), Seq()))
    assert(drive(_AcommitBorAorA, "abc") === Success(Seq('a', 'b'), Seq('c')))
    assert(drive(_AcommitBorAorA, "ac") === Success(Seq('a'), Seq('c')))
    assert(drive(_AcommitBorAorA, "c") === Failure(Seq(), Seq()))
  }

  test("aab|ab") {
    assert(drive(_AABorAB, "aab") === Success(Seq('a', 'a', 'b'), Seq()))
    assert(drive(_AABorAB, "ab") === Success(Seq('a', 'b'), Seq()))
    assert(drive(_AABorAB, "abc") === Success(Seq('a', 'b'), Seq('c')))
    assert(drive(_AABorAB, "aabc") === Success(Seq('a', 'a', 'b'), Seq('c')))
    assert(drive(_AABorAB, "ac") === Failure(Seq('a'), Seq()))
  }

  test("a.many") {
    assert(drive(_Amany, "") === Success(Seq(), Seq()))
    assert(drive(_Amany, "a") === Success(Seq('a'), Seq()))
    assert(drive(_Amany, "aa") === Success(Seq('a', 'a'), Seq()))
    assert(drive(_Amany, "b") === Success(Seq(), Seq('b')))
    assert(drive(_Amany, "bb") === Success(Seq(), Seq('b', 'b')))
  }

  test("a.oneOrMore") {
    assert(drive(_AoneOrMore, "") === Failure(Seq(), Seq()))
    assert(drive(_AoneOrMore, "a") === Success(Seq('a'), Seq()))
    assert(drive(_AoneOrMore, "aa") === Success(Seq('a', 'a'), Seq()))
    assert(drive(_AoneOrMore, "ab") === Success(Seq('a'), Seq('b')))
    assert(drive(_AoneOrMore, "aabb") === Success(Seq('a', 'a'), Seq('b', 'b')))
    assert(drive(_AoneOrMore, "b") === Failure(Seq(), Seq()))
  }

  test("a?") {
    assert(drive(_Aoptional, "") === Success(Seq(), Seq()))
    assert(drive(_Aoptional, "a") === Success(Seq(('a')), Seq()))
    assert(drive(_Aoptional, "ab") === Success(Seq('a'), Seq('b')))
  }

  test("a{option}") {
    assert(drive(_Aoption, "") === Success(Seq(None), Seq()))
    assert(drive(_Aoption, "a") === Success(Seq((Some('a'))), Seq()))
    assert(drive(_Aoption, "ab") === Success(Seq(Some('a')), Seq('b')))
  }

  test("a{bind}b") {
    assert(drive(_AbindB, "ab") === Success(Seq(('a', 'b')), Seq()))
    assert(drive(_AbindB, "abc") === Success(Seq(('a', 'b')), Seq('c')))
    assert(drive(_AbindB, "ac") === Failure(Seq(), Seq()))
    assert(drive(_AbindB, "b") === Failure(Seq(), Seq()))
  }

  test("a|>a") {
    assert(drive(_ApipeA, "a") === Success(Seq('a'), Seq()))
    assert(drive(_ApipeA, "ab") === Success(Seq('a'), Seq('b')))
    assert(drive(_ApipeA, "") === Failure(Seq(), Seq()))
    assert(drive(_ApipeA, "b") === Failure(Seq(), Seq()))
  }

  test("(a|b)|>(a|b)") {
    assert(drive(_AorBpipeAorB, "a") === Success(Seq('a'), Seq()))
    assert(drive(_AorBpipeAorB, "b") === Success(Seq('b'), Seq()))
    assert(drive(_AorBpipeAorB, "bbc") === Success(Seq('b'), Seq('b', 'c')))
    assert(drive(_AorBpipeAorB, "x") === Failure(Seq(), Seq()))
  }

  test("{any}|>(ab|a)") {
    assert(drive(_ANYpipeABorA, "ab") === Success(Seq('a', 'b'), Seq()))
    assert(drive(_ANYpipeABorA, "a") === Success(Seq('a'), Seq()))
    assert(drive(_ANYpipeABorA, "abc") === Success(Seq('a', 'b'), Seq('c')))
    // In the next test case the letter 'c' is consumed by the left side of the parse because it
    // was tentatively piped into the right side. When the right side resets the pipe then the letters 'a' and 'c'
    // are stored in an Emit that is piped into the right side again.
    // The next test case shows that the letter 'c' is available for further matches
    assert(drive(_ANYpipeABorA, "ac") === Success(Seq('a'), Seq()))
    assert(drive(_ANYpipeABorA, "c") === Failure(Seq(), Seq()))
  }

  test("{any}|>(ab|a)c") {
    assert(drive(_ANYpipeABorAandC, "ac") === Success(Seq('a', 'c'), Seq()))
  }

  test("(a|b)|>(ab|a)") {
    assert(drive(_AorBpipeABorA, "ab") === Success(Seq('a', 'b'), Seq()))
    assert(drive(_AorBpipeABorA, "a") === Success(Seq('a'), Seq()))
    assert(drive(_AorBpipeABorA, "abc") === Success(Seq('a', 'b'), Seq('c')))
    assert(drive(_AorBpipeABorA, "ac") === Success(Seq('a'), Seq('c')))
    assert(drive(_AorBpipeABorA, "b") === Failure(Seq(), Seq())) //
  }

  test("greedy pattern: a*") {
    assert(drive(_greedyAs, "") === Success(Seq(""), Seq()))
    assert(drive(_greedyAs, "a") === Success(Seq("a"), Seq()))
    assert(drive(_greedyAs, "aa") === Success(Seq("aa"), Seq()))
    assert(drive(_greedyAs, "aab") === Success(Seq("aa"), Seq('b')))
    assert(drive(_greedyAs, "b") === Success(Seq(""), Seq('b')))
  }

  test("non-greedy pattern: a*") {
    assert(drive(_nonGreedyAs, "") === Success(Seq(""), Seq()))
    assert(drive(_nonGreedyAs, "a") === Success(Seq("a"), Seq()))
    assert(drive(_nonGreedyAs, "aa") === Success(Seq("a"), Seq('a')))
    assert(drive(_nonGreedyAs, "aab") === Success(Seq("a"), Seq('a', 'b')))
    assert(drive(_nonGreedyAs, "b") === Success(Seq(""), Seq('b')))
  }

  test("pattern: yes|no") {
    assert(drive(_yesOrNo, "yes") === Success(Seq("yes"), Seq()))
    assert(drive(_yesOrNo, "no") === Success(Seq("no"), Seq()))
    assert(drive(_yesOrNo, "yesx") === Success(Seq("yes"), Seq('x')))
    assert(drive(_yesOrNo, "y") === Failure(Seq(), Seq('y')))
  }

  def AorBorC(ps: parsers.PS): Unit = {
    assert(drive(ps, "a") === Success(Seq('a'), Seq()))
    assert(drive(ps, "b") === Success(Seq('b'), Seq()))
    assert(drive(ps, "c") === Success(Seq('c'), Seq()))
    assert(drive(ps, "ax") === Success(Seq('a'), Seq('x')))
    assert(drive(ps, "bx") === Success(Seq('b'), Seq('x')))
    assert(drive(ps, "cx") === Success(Seq('c'), Seq('x')))
    assert(drive(ps, "x") === Failure(Seq(), Seq()))
  }

}
