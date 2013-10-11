package main.scala.eu.swdev.xml.token

import main.scala.eu.swdev.parser.push.PushParsers
import scala.util.matching.Regex

/**
 */
class Tokenizer(val tokenHandler: TokenHandler) {

  val XmlPushParsers = new PushParsers[Char] {

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
    val _document = _prolog ~ _element ~ _misc.many

    // 2
    val _Char = new PushParser[Char] {
      def push(i: Char): PushResult[Char] = if (isChar(i)) Return(true, Seq(i), Nil) else FailedPush(true)
      def flush(): FlushResult[Char] = FailedFlush(true)
    }

    // 3
    val _S = new PushParser[Nothing] {
      def push(i: Char): PushResult[Nothing] = if (isWhitespace(i)) {
        Return(true, Seq(), i :: Nil)
      } else {
        FailedPush(true)
      }
      def flush(): FlushResult[Nothing] = FailedFlush(true)
    }.oneOrMore

    // 22
    val _prolog: PushParser[Token] = _XmlDecl.optional
    
    // 23
    val _XmlDecl = "<?xml" ~ (_VersionInfo >>= ((vi: String) => (_EncodingDecl.option >>= ((ed: Option[String]) => (_SDDecl.option >>= ((sd: Option[String]) => unit(XmlDecl(vi, ed, sd)))))))) ~ _S.optional ~ "?>"

    // 24
    val _VersionInfo = _S ~ "version" ~ _Eq

    // 25
    val _Eq = _S.optional ~ "=".ignore ~ _S.optional

    // 27
    val _misc: PushParser[Token] = EmptyPushParser

    // 32
    val _SDDecl = _S ~ "standalone" ~ _Eq ~ (("'" ~ "yes|no".r ~ "'") | (("\"" ~ "yes|no".r ~ "\"")))

    // 39
    val _element: PushParser[Token] = EmptyPushParser

    // 80
    val _EncodingDecl = _S ~ "encoding" ~ _Eq ~ (("'" ~ _EncName ~ "'") | (("\"" ~ _EncName ~ "\"")))

    // 81
    val _EncName = "[A-Za-z]([A-Za-z0-9,_]|-)*".r

    //
    //
    //
    
    implicit def pushParser(string: String): PushParser[Nothing] = new PushParser[Nothing] {
      var idx = 0
      def push(i: Char): PushResult[Nothing] = if (idx == string.length) {
        Return(true, Seq(), i :: Nil)
      } else if (string.charAt(idx) == i) {
        idx = idx + 1
        Continue(true, Seq(), this)
      } else {
        FailedPush(true)
      }

      def flush(): FlushResult[Nothing] = if (idx == string.length) {
        Flushed(true, Seq())
      } else {
        FailedFlush(true)
      }
    }

    implicit def pushParser(regex: Regex): PushParser[String] = new PushParser[String] {
      val sb = new StringBuilder
      def push(i: Char): PushResult[String] = {
        sb.append(i)
        if (regex.findFirstIn(sb).isDefined) {
          Continue(true, Seq(), this)
        } else {
          sb.setLength(sb.length - 1)
          if (regex.findFirstIn(sb).isDefined) {
            Return(true, Seq(sb.toString()), i :: Nil)
          } else {
            FailedPush(true)
          }
        }
      }

      def flush(): FlushResult[String] = if (regex.findFirstIn(sb).isDefined) {
        Flushed(true, Seq(sb.toString()))
      } else {
        FailedFlush(true)
      }
    }

  }

  sealed trait Token

  case class XmlDecl(version: String, encoding: Option[String], standalone: Option[String]) extends Token

  private def isWhitespace(char: Char) = char == ' ' || char == '\n' || char == '\t' || char == '\r'
  
  private def isChar(char: Char) = char >= '\u0020' && char <= '\ud7ff' || char == '\u0009' || char == '\u000a' || char == '\u000d' || char >= '\ue000' && char <= '\ufffd'



}
