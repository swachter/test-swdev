package eu.swdev.xml.token

import main.scala.eu.swdev.xml.token.Tokenizer
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

}
