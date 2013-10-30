package eu.swdev.xml.token

import eu.swdev.parser.push.CharParsers2
import java.util.regex.Pattern

/**
 */
class Lexer2 {

  val XmlPushParsers = new CharParsers2 {

    type Parser[O] = ParserState[Char, O]

    implicit def charParser(char: Char): ParserState[Char, Nothing] = require(char)

    implicit def stringParser(str: String): ParserState[Char, Nothing] = require(str.toList)

    // 1
    lazy val _document = _prolog ~ _element ~ _misc.many

    // 2
    lazy val _Char = checkChar(isChar)

    // 3
    lazy val _S: Parser[Nothing] = (Await(c => if (isWhitespace(c)) Halt() else Error(), Error()): Parser[Nothing]).oneOrMore

    // 4
    lazy val _NameStartChar = checkChar(isNameStartChar)

    // 4a
    lazy val _NameChar = checkChar(isNameChar)

    // 5
    lazy val _Name = (_NameStartChar ~ _NameChar.many).collapse.map(_.foldRight(new StringBuilder)((c, b) => b.append(c)).toString())

    // 10
    lazy val _AttValue: ParserState[Char, Token] = ('"' ~ (attrChars('"') | _Reference ).many ~ '"') | ('\'' ~ (attrChars('\'') | _Reference ).many ~ '\'')

    // 14 (requires at least one character (the XML spec allows no characters)
    lazy val _CharData = charData.map(CharData(_))

    // 22
    lazy val _prolog: Parser[Token] = _XmlDecl.optional

    // 23
    lazy val _XmlDecl = "<?xml" ~ (_VersionInfo >>= ((vi: String) => (_EncodingDecl.option >>= ((ed: Option[String]) => (_SDDecl.option >>= ((sd: Option[String]) => unit(XmlDecl(vi, ed, sd)))))))) ~ _S.optional ~ "?>"

    // 24
    lazy val _VersionInfo: Parser[String] = _S ~ "version" ~ _Eq ~ quotes(_VersionNum)

    // 25
    lazy val _Eq = _S.optional ~ '=' ~ _S.optional

    // 26
    lazy val _VersionNum = "1\\.[0-9]+".p

    // 27
    lazy val _misc: Parser[Token] = Halt()

    // 32
    lazy val _SDDecl = _S ~ "standalone" ~ _Eq ~ quotes("yes|no".p)

    // 39
    lazy val _element: Parser[Token] = '<' ~ _Name.map(BeginSTag(_)) ~ (_S ~ _Attribute).many ~ _S.optional ~ ("/>" ~ unit(EndSTagEmpty) | '>' ~ unit(EndSTag) ~ _content ~ _ETag.map(ETag(_)) )

    // 40
    //lazy val _STag = "<" ~ _Name ~ (_S ~ _Attribute).many ~ _S.optional ~ '>'

    // 41
    lazy val _Attribute: Parser[Token] = _Name.map(AttrName(_)) ~ _Eq ~ _AttValue

    // 42
    lazy val _ETag = "</" ~ _Name ~ _S.optional ~ '>'

    // 43
    lazy val _content: Parser[Token] = _CharData.optional ~ (( _element | _Reference ) ~ _CharData.optional).many

    // 44
    //lazy val _EmptyElemTag = '<' ~ _Name ~ (_S ~ _Attribute).many ~ _S.optional ~ "/>"

    // 66
    lazy val _CharRef: Parser[Token] = "&#" ~ ( ( 'x' ~ "[0-9a-fA-f]+".p ).map((s: String) => CharRef(s, true)) | ( "[0-9]+".p ~ ';' ).map((s: String) => CharRef(s, false)) ) ~ ';'

    // 67
    lazy val _Reference: Parser[Token] = _CharRef | _EntityRef

    // 68
    lazy val _EntityRef: Parser[Token] = '&' ~ _Name.map(EntityRef(_)) ~ ';'

    // 80
    lazy val _EncodingDecl = _S ~ "encoding" ~ _Eq ~ quotes(_EncName)

    // 81
    lazy val _EncName = "[A-Za-z]([A-Za-z0-9,_]|-)*".p

    def quotes[O](p: Parser[O]): Parser[O] = ("'" ~ p ~ "'") | (("\"" ~ p ~ "\""))

    def checkChar(check: Char => Boolean): Parser[Char] = Await(i => if (check(i)) Emit(Seq(i), Halt()) else Error(), Error())

    //
    //
    //

    implicit class StrOps(string: String) {
      def p: Parser[String] = regexParser(Pattern.compile(string), true)
    }

    //
    //
    //

    def charData: Parser[String] = {
      val charParser: Parser[Char] = Await(char => if (char != '<' && char != '&') Emit(Seq(char), Halt()) else Error(), Error())
      charParser.oneOrMore.collapse.map(seq => seq.foldLeft(new StringBuilder)((b, c) => b.append(c)).toString())
    }

    def attrChars(quote: Char): Parser[Token] = {
      val attrChar: Parser[Char] = Await(char => if (char != '<' && char != '&' && char != quote) Emit(Seq(char), Halt()) else Error(), Error())
      attrChar.oneOrMore.collapse.map(seq => AttrChars(seq.foldLeft(new StringBuilder)((b, c) => b.append(c)).toString()))
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
