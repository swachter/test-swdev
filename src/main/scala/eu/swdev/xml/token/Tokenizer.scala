package main.scala.eu.swdev.xml.token

/**
 */
class Tokenizer(val tokenHandler: TokenHandler) {

  var line = 1
  var column = 1

  var state: State = initial

  var consumeList: List[Char]
  var consumeReturn: State

  def push(char: Char): Unit = {
    if (char == '\n') {
      line = line + 1
      column = 1
    }
    state.push(char)
  }


  private def isWhitespace(char: Char) = char == ' ' || char == '\n' || char == '\t' || char == '\r'

  trait State {
    def push(char: Char): Unit
    def flush() = throw new RuntimeException("unexpected eof")
  }

}
