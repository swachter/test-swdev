package eu.swdev.parser.push

import org.scalatest.FunSuite

/**
  */
class ParsersTest extends FunSuite {

  val parsers = new Parsers[Char] {

    val _AandB = 'a' ~ 'b'

    val _AorB: ParserState[Char] = 'a' or 'b'

    val _AorBorC1: ParserState[Char] = ('a' or 'b') or 'c'
    val _AorBorC2: ParserState[Char] = 'a' or ('b' or 'c')

    val _ABorA = ('a' ~ 'b') or 'a'

    val _AcommitBorA = ('a' ~ Commit(Halt()) ~ 'b') or 'a'

    val _AABorAB = ('a' ~ 'a' ~ 'b') or ('a' ~ 'b')

    val _Amany = 'a'.many

    val _AoneOrMore = 'a'.oneOrMore
    
    implicit def parser(char: Char): ParserState[Char] = Await(c => if (c == char) Emit(Seq(c), Halt()) else Error(), Error())

    def drive[O](ps: ParserState[O], string: String): RunResult[O] = {
      val (rr, log) = run(ps, new RootListRunState(string.to[List], Nil), Nil)
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
    AorBorC(parsers._AorBorC1.asInstanceOf[parsers.ParserState[Char]])
  }

  test("a|(b|c)") {
    AorBorC(parsers._AorBorC2.asInstanceOf[parsers.ParserState[Char]])
  }

  test("ab|a") {
    assert(drive(_ABorA, "ab") === Success(Seq('a', 'b'), Seq()))
    assert(drive(_ABorA, "a") === Success(Seq('a'), Seq()))
    assert(drive(_ABorA, "abc") === Success(Seq('a', 'b'), Seq('c')))
    assert(drive(_ABorA, "ac") === Success(Seq('a'), Seq('c')))
    assert(drive(_ABorA, "c") === Failure(Seq(), Seq()))
  }

  test("a{commit}b|a") {
    assert(drive(_ABorA, "ab") === Success(Seq('a', 'b'), Seq()))
    assert(drive(_ABorA, "a") === Failure(Seq('a'), Seq()))
    assert(drive(_ABorA, "abc") === Success(Seq('a', 'b'), Seq('c')))
    assert(drive(_ABorA, "ac") === Failure(Seq('a'), Seq('c')))
    assert(drive(_ABorA, "c") === Failure(Seq(), Seq()))
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

  def AorBorC(ps: parsers.ParserState[Char]): Unit = {
    assert(drive(ps, "a") === Success(Seq('a'), Seq()))
    assert(drive(ps, "b") === Success(Seq('b'), Seq()))
    assert(drive(ps, "c") === Success(Seq('c'), Seq()))
    assert(drive(ps, "ax") === Success(Seq('a'), Seq('x')))
    assert(drive(ps, "bx") === Success(Seq('b'), Seq('x')))
    assert(drive(ps, "cx") === Success(Seq('c'), Seq('x')))
    assert(drive(ps, "x") === Failure(Seq(), Seq()))
  }

}
