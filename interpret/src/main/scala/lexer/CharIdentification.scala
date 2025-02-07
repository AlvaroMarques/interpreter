package lexer

trait CharIdentification {
  def isLetter(ch: Byte): Boolean = {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
  }

  def isDigit(ch: Byte): Boolean = {
    '0' <= ch && ch <= '9'
  }

  def isWhitespace(ch: Byte): Boolean = {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
  }

}
