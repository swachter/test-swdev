package eu.swdev.xml.token

import org.scalatest.FunSuite

/**
  */
class TokenizerTest extends FunSuite {

  val parsers = new Tokenizer()

  import parsers._
  import XmlPushParsers._

  test ("S") {
    assert(_S.run(" ") === RunSuccess(Nil, Nil))
    assert(_S.run("  ") === RunSuccess(Nil, Nil))
    assert(_S.run("  x") === RunSuccess(Nil, 'x' :: Nil))
    assert(_S.run("x") === RunFailure(Nil))
  }

  test ("VersionInfo") {
    assert((_S ~ "version").run(" version") === RunSuccess(Nil, Nil))
    assert((_S ~ "version" ~_Eq).run(" version=") === RunSuccess(Nil, Nil))
    assert(_VersionInfo.run(" version=\"1.0\"") === RunSuccess(Seq("1.0"), Nil))
  }

  test("xmlDecl") {
    assert(_XmlDecl.run("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>") === RunSuccess(Seq(XmlDecl("1.0", Some("UTF-8"), Some("yes"))), Nil))
    assert(_XmlDecl.run("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>") === RunSuccess(Seq(XmlDecl("1.0", Some("UTF-8"), None)), Nil))
    assert(_XmlDecl.run("<?xml version=\"1.0\" standalone=\"yes\" ?>") === RunSuccess(Seq(XmlDecl("1.0", None, Some("yes"))), Nil))
    assert(_XmlDecl.run("<?xml version=\"1.0\" ?>") === RunSuccess(Seq(XmlDecl("1.0", None, None)), Nil))
    assert(_XmlDecl.run("<?xml version=\"1.0\"?>") === RunSuccess(Seq(XmlDecl("1.0", None, None)), Nil))
  }

  test("Name") {
    assert(_Name.run("a") === RunSuccess(Seq("a"), Nil))
    assert(_Name.run("a:b") === RunSuccess(Seq("a:b"), Nil))
    assert(_Name.run("a:b?") === RunSuccess(Seq("a:b"), '?' :: Nil))
  }

  test("Attribute") {
    assert(_AttValue.run("'abc'") === RunSuccess(Seq(AttrChars("abc")), Nil))
    assert(_AttValue.run("'&#xa;'") === RunSuccess(Seq(CharRef("a", true)), Nil))
    assert(_EntityRef.run("&apos;") === RunSuccess(Seq(EntityRef("apos")), Nil))
    assert(_Reference.run("&apos;") === RunSuccess(Seq(EntityRef("apos")), Nil))
    assert(_AttValue.run("'&apos;'") === RunSuccess(Seq(EntityRef("apos")), Nil))
    assert(_Attribute.run("a='abc'") === RunSuccess(Seq(AttrChars("abc"), AttrName("a")), Seq()))
    assert(_Attribute.run("a='abc&#xa;&apos;xyz'") === RunSuccess(Seq(AttrChars("xyz"), EntityRef("apos"), CharRef("a", true), AttrChars("abc"), AttrName("a")), Seq()))
  }

  test("Element") {
    assert(_element.run("<x:e a='5' xmlns:x='x'/>") === RunSuccess(Seq(EndSTagEmpty, AttrChars("x"), AttrName("xmlns:x"), AttrChars("5"), AttrName("a"), BeginSTag("x:e")), Nil))
    assert(_element.run("<x:e></x:e>") === RunSuccess(Seq(ETag("x:e"), EndSTag, BeginSTag("x:e")), Nil))
  }

}


