package main.scala.eu.swdev.xml.token

import main.scala.eu.swdev.parser.push.{CharPushParsers, PushParsers}
import scala.util.matching.Regex

/**
 */
class Tokenizer {

  val XmlPushParsers = new CharPushParsers[Char](c => c) {

    var line = 1
    var column = 1


    def push(char: Char): Unit = {
      if (char == '\n') {
        line = line + 1
        column = 1
      }
      // state.push(char)
    }

    // 1
    lazy val _document = _prolog ~ _element ~ _misc.many

    // 2
    lazy val _Char = new PushParser[Char] {
      def push(i: Char): PushResult[Char] = if (isChar(i)) Return(true, Seq(i), Nil) else FailedPush(true)
      def flush(): FlushResult[Char] = FailedFlush(true)
    }

    // 3
    lazy val _S = new PushParser[Nothing] {
      def push(i: Char): PushResult[Nothing] = if (isWhitespace(i)) {
        Return(true, Seq(), Nil)
      } else {
        FailedPush(true)
      }
      def flush(): FlushResult[Nothing] = FailedFlush(true)
    }.oneOrMore

    // 22
    lazy val _prolog: PushParser[Token] = _XmlDecl.optional
    
    // 23
    lazy val _XmlDecl = "<?xml" ~ (_VersionInfo >>= ((vi: String) => (_EncodingDecl.option >>= ((ed: Option[String]) => (_SDDecl.option >>= ((sd: Option[String]) => unit(XmlDecl(vi, ed, sd)))))))) ~ _S.optional ~ "?>"

    // 24
    lazy val _VersionInfo = _S ~ "version" ~ _Eq ~ quotes(_VersionNum)

    // 25
    lazy val _Eq = _S.optional ~ "=" ~ _S.optional

    // 26
    lazy val _VersionNum = "1\\.[0-9]+".p

    // 27
    lazy val _misc: PushParser[Token] = EmptyPushParser

    // 32
    lazy val _SDDecl = _S ~ "standalone" ~ _Eq ~ quotes("yes|no".p)

    // 39
    lazy val _element: PushParser[Token] = EmptyPushParser

    // 80
    lazy val _EncodingDecl = _S ~ "encoding" ~ _Eq ~ quotes(_EncName)

    // 81
    lazy val _EncName = "[A-Za-z]([A-Za-z0-9,_]|-)*".p

    def quotes[I](p: PushParser[I]): PushParser[I] = ("'" ~ p ~ "'") | (("\"" ~ p ~ "\""))

    //
    //
    //
    
    implicit def pushParser(string: String): PushParser[Nothing] = new IgnorePushParser(string, false)((c, d) => c == d)

    implicit class StrOps(string: String) {
      def p: PushParser[String] = new PatternPushParser(string, true)
    }

  }

  sealed trait Token

  case class XmlDecl(version: String, encoding: Option[String], standalone: Option[String]) extends Token

  private def isWhitespace(char: Char) = char == ' ' || char == '\n' || char == '\t' || char == '\r'
  
  private def isChar(char: Char) = char >= '\u0020' && char <= '\ud7ff' || char == '\u0009' || char == '\u000a' || char == '\u000d' || char >= '\ue000' && char <= '\ufffd'



}
