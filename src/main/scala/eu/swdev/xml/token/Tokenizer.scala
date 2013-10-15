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
    lazy val _Char = checkChar(isChar)

    // 3
    lazy val _S = new PushParser[Nothing] {
      def push(i: Char): PushResult[Nothing] = if (isWhitespace(i)) {
        Return(true, Seq(), Nil)
      } else {
        FailedPush(true)
      }
      def flush(): FlushResult[Nothing] = FailedFlush(true)
    }.oneOrMore

    // 4
    lazy val _NameStartChar = checkChar(isNameStartChar)

    // 4a
    lazy val _NameChar = checkChar(isNameChar)

    // 5
    lazy val _Name = (_NameStartChar ~ _NameChar.many).collect.map(_.foldRight(new StringBuilder)((c, b) => b.append(c)).toString())

    // 10
    lazy val _AttValue: PushParser[Token] = ('"' ~ (new AttrCharsPushParser('"') | _Reference ).many ~ '"') | ('\'' ~ (new AttrCharsPushParser('\'') | _Reference ).many ~ '\'')


    // 14 (requires at least one character (the XML spec allows no characters)
    lazy val _CharData = (new CharDataPushParser).map(CharData(_))

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
    lazy val _element: PushParser[Token] = '<' ~ _Name.map(BeginSTag(_)) ~ (_S ~ _Attribute).many ~ _S.optional ~ ("/>" ~ unit(EndSTagEmpty) | '>' ~ unit(EndSTag) ~ _content ~ _ETag.map(ETag(_)) )

    // 40
    //lazy val _STag = "<" ~ _Name ~ (_S ~ _Attribute).many ~ _S.optional ~ '>'

    // 41
    lazy val _Attribute: PushParser[Token] = _Name.map(AttrName(_)) ~ _Eq ~ _AttValue

    // 42
    lazy val _ETag = "</" ~ _Name ~ _S.optional ~ '>'

    // 43
    lazy val _content: PushParser[Token] = _CharData.optional ~ (( _element | _Reference ) ~ _CharData.optional).many

    // 44
    //lazy val _EmptyElemTag = '<' ~ _Name ~ (_S ~ _Attribute).many ~ _S.optional ~ "/>"

    // 66
    lazy val _CharRef: PushParser[Token] = "&#" ~ ( ( 'x' ~ "[0-9a-fA-f]+".p ).map((s: String) => CharRef(s, true)) | ( "[0-9]+".p ~ ';' ).map((s: String) => CharRef(s, false)) ) ~ ';'

    // 67
    lazy val _Reference: PushParser[Token] = _EntityRef.attempt | _CharRef

    // 68
    lazy val _EntityRef: PushParser[Token] = '&' ~ _Name.map(EntityRef(_)) ~ ';'

    // 80
    lazy val _EncodingDecl = _S ~ "encoding" ~ _Eq ~ quotes(_EncName)

    // 81
    lazy val _EncName = "[A-Za-z]([A-Za-z0-9,_]|-)*".p

    def quotes[O](p: PushParser[O]): PushParser[O] = ("'" ~ p ~ "'") | (("\"" ~ p ~ "\""))

    def checkChar(check: Char => Boolean): PushParser[Char] = new PushParser[Char] {
      def push(i: Char): PushResult[Char] = if (check(i)) Return(true, Seq(i), Nil) else FailedPush(true)
      def flush(): FlushResult[Char] = FailedFlush(true)
    }

    //
    //
    //
    
    implicit def pushParser(string: String): PushParser[Nothing] = new IgnorePushParser(string, false)((c, d) => c == d)

    implicit def pushParser(char: Char): PushParser[Nothing] = new PushParser[Nothing] {
      def push(i: Char): PushResult[Nothing] = if (i == char) Return(true, Nil, Nil) else FailedPush(false)
      def flush(): FlushResult[Nothing] = FailedFlush(false)
    }

    implicit class StrOps(string: String) {
      def p: PushParser[String] = new PatternPushParser(string, true)
    }

    //
    //
    //

    // requires at least one character
    class CharDataPushParser extends PushParser[String] {
      private def check(char: Char) = char != '<' && char != '&'
      def push(i: Char): PushResult[String] = if (check(i)) Continue(true, Seq(), new Recorder(new StringBuilder(i))) else FailedPush(false)
      def flush(): FlushResult[String] = FailedFlush(false)
      class Recorder(sb: StringBuilder) extends PushParser[String] {
        var state = 0
        def push(i: Char): PushResult[String] = if (check(i)) {
          sb.append(i)
          state match {
            case 0 => {
              if (i == ']') state = 1
              Continue(true, Seq(), this)
            }
            case 1 => {
              if (i == ']') state = 2 else state = 0
              Continue(true, Seq(), this)
            }
            case 2 => {
              if (i == '>') {
                FailedPush(true) // the sequence "]]>" is forbidden inside CharData
              } else {
                if (i != ']') state = 0
                Continue(true, Seq(), this)
              }
            }
          }
        } else {
          Return(true, Seq(sb.toString()), i :: Nil)
        }
        def flush(): FlushResult[String] = Flushed(true, Seq(sb.toString()), Nil)
      }
    }

    // requires at least one char
    class AttrCharsPushParser(quote: Char) extends PushParser[Token] {
      private def check(char: Char) = char != '<' && char != '&' && char != quote
      def push(i: Char): PushResult[Token] = if (check(i)) Continue(true, Seq(), new Recorder(new StringBuilder().append(i))) else FailedPush(false)
      def flush(): FlushResult[Token] = FailedFlush(false)
      class Recorder(sb: StringBuilder) extends PushParser[Token] {
        def push(i: Char): PushResult[Token] = if (check(i)) {
          sb.append(i)
          Continue(true, Seq(), this)
        } else {
          Return(true, Seq(AttrChars(sb.toString())), i :: Nil)
        }
        def flush(): FlushResult[Token] = Flushed(true, Seq(AttrChars(sb.toString())), Nil)
      }
    }

  }

  sealed trait Token

  case class XmlDecl(version: String, encoding: Option[String], standalone: Option[String]) extends Token

  case class BeginSTag(name: String) extends Token
  case object EndSTag extends Token
  case object EndSTagEmpty extends Token
  case class ETag(name: String) extends Token

  case class CharData(string: String) extends Token
  case class AttrName(name: String) extends Token
  case class AttrChars(string: String) extends Token
  case class CharRef(code: String, isHex: Boolean) extends Token
  case class EntityRef(code: String) extends Token

  private def isWhitespace(char: Char) = char == ' ' || char == '\n' || char == '\t' || char == '\r'
  
  private def isChar(char: Char) = char >= '\u0020' && char <= '\ud7ff' || char == '\u0009' || char == '\u000a' || char == '\u000d' || char >= '\ue000' && char <= '\ufffd'

  private def isNameStartChar(char: Char) =
    char == ':' ||
    char >= 'A' && char <= 'Z' ||
    char == '_' ||
    char >= 'a' && char <= 'z' ||
    char >= '\u00c0' && char <= '\u00d6' ||
    char >= '\u00d8' && char <= '\u00f6' ||
    char >= '\u00f8' && char <= '\u02ff' ||
    char >= '\u0370' && char <= '\u037d' ||
    char >= '\u037f' && char <= '\u1fff' ||
    char >= '\u200d' && char <= '\u200c' ||
    char >= '\u2070' && char <= '\u218f' ||
    char >= '\u2c00' && char <= '\u2fef' ||
    char >= '\u3001' && char <= '\ud7ff' ||
    char >= '\uf900' && char <= '\ufdcf' ||
    char >= '\ufdf0' && char <= '\ufffd'

  private def isNameChar(char: Char) =
    isNameStartChar(char) ||
    char == '-' ||
    char == '.' ||
    char >= '0' && char <= '9' ||
    char == '\u00b7' ||
    char >= '\u0300' && char <= '\u036f' ||
    char >= '\u203f' && char <= '\u2040'

  private def notLtOrAmp(char: Char): Boolean = char != '<' && char != '&'


}
