package eu.swdev.xml.token

import org.scalatest.FunSuite

/**
  */
class Lexer2Test extends FunSuite {

  val lexer = new Lexer2()

  import lexer._
  import XmlPushParsers._

  def drive[O](ps: Parser[O], string: String): RunResult[Char, O] = {
    val (rr, log) = lexer.XmlPushParsers.run(ps, new RootListRunState[Char, O](string.to[List], Nil), Nil)
    println(log)
    rr
  }

  test ("S") {
    assert(drive[Nothing](_S, " ") === Success(Nil, Nil))
    assert(drive[Nothing](_S, "  ") === Success(Nil, Nil))
    assert(drive[Nothing](_S, "  x") === Success(Nil, 'x' :: Nil))
    assert(drive[Nothing](_S, "x") === Failure(Nil, Nil))
  }

  test ("VersionInfo") {
    assert(drive[Nothing](_S ~ "version", " version") === Success(Nil, Nil))
    assert(drive[Nothing](_S ~ "version" ~_Eq, " version=") === Success(Nil, Nil))
    assert(drive[String](_VersionInfo, " version=\"1.0\"") === Success(Seq("1.0"), Nil))
    assert(drive[String](_VersionInfo, " version=\"1.0\" ") === Success(Seq("1.0"), Seq(' ')))
    assert(drive[String](_VersionInfo >>= (vi => unit(vi)), " version=\"1.0\"") === Success(Seq("1.0"), Seq()))
    assert(drive[String](_VersionInfo >>= (vi => unit(vi)), " version=\"1.0\" ") === Success(Seq("1.0"), Seq(' ')))
  }

  test("encodingDecl") {
    assert(drive[String](_EncodingDecl, " encoding=\"UTF-8\"") === Success(Seq("UTF-8"), Nil))
    assert(drive[Option[String]](_EncodingDecl.option, " encoding=\"UTF-8\"") === Success(Seq(Some("UTF-8")), Nil))
  }

  test("versionInfo and encodingDecl") {
    assert(drive[String](_VersionInfo ~ _EncodingDecl, " version=\"1.0\" encoding=\"UTF-8\"") === Success(Seq("1.0", "UTF-8"), Nil))
    assert(drive[(String, String)](_VersionInfo >>= ((vi: String) => _EncodingDecl.map(ed => (vi, ed))), " version=\"1.0\" encoding=\"UTF-8\"") === Success(Seq(("1.0", "UTF-8")), Nil))
  }

  test("xmlDecl") {
    assert(drive[XmlDecl](_XmlDecl, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>") === Success(Seq(XmlDecl("1.0", Some("UTF-8"), Some("yes"))), Nil))
    assert(drive[XmlDecl](_XmlDecl, "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>") === Success(Seq(XmlDecl("1.0", Some("UTF-8"), None)), Nil))
    assert(drive[XmlDecl](_XmlDecl, "<?xml version=\"1.0\" standalone=\"yes\" ?>") === Success(Seq(XmlDecl("1.0", None, Some("yes"))), Nil))
    assert(drive[XmlDecl](_XmlDecl, "<?xml version=\"1.0\" ?>") === Success(Seq(XmlDecl("1.0", None, None)), Nil))
    assert(drive[XmlDecl](_XmlDecl, "<?xml version=\"1.0\"?>") === Success(Seq(XmlDecl("1.0", None, None)), Nil))
  }

  test("Name") {
    assert(drive[String](_Name, "a") === Success(Seq("a"), Nil))
    assert(drive[String](_Name, "a:b") === Success(Seq("a:b"), Nil))
    assert(drive[String](_Name, "a:b?") === Success(Seq("a:b"), '?' :: Nil))
  }

  test("Attribute") {
    assert(drive[Token](_AttValue, "'abc'") === Success(Seq(AttrChars("abc")), Nil))
    assert(drive[Token](_AttValue, "'&#xa;'") === Success(Seq(CharRef("a", true)), Nil))
    assert(drive[Token](_EntityRef, "&apos;") === Success(Seq(EntityRef("apos")), Nil))
    assert(drive[Token](_Reference, "&apos;") === Success(Seq(EntityRef("apos")), Nil))
    assert(drive[Token](_AttValue, "'&apos;'") === Success(Seq(EntityRef("apos")), Nil))
    assert(drive[Token](_Attribute, "a='abc'") === Success(Seq(AttrChars("abc"), AttrName("a")), Seq()))
    assert(drive[Token](_Attribute, "a='abc&#xa;&apos;xyz'") === Success(Seq(AttrChars("xyz"), EntityRef("apos"), CharRef("a", true), AttrChars("abc"), AttrName("a")), Seq()))
  }

  test("Element") {
    assert(drive[Token](_element, "<x:e a='5' xmlns:x='x'/>") === Success(Seq(EndSTagEmpty, AttrChars("x"), AttrName("xmlns:x"), AttrChars("5"), AttrName("a"), BeginSTag("x:e")), Nil))
    assert(drive[Token](_element, "<x:e></x:e>") === Success(Seq(ETag("x:e"), EndSTag, BeginSTag("x:e")), Nil))
  }

}


