package token

import enumeratum.{Enum, EnumEntry}

sealed trait TokenType extends EnumEntry

object TokenType extends Enum[TokenType] {
  val values: IndexedSeq[TokenType] = findValues
  object ILLEGAL extends TokenType { override def toString = "ILLEGAL" }
  object EOF extends TokenType { override def toString = "EOF" }

  // Identifiers + literals
  object IDENT extends TokenType { override def toString = "IDENT" }
  object INT extends TokenType { override def toString = "INT" }
  object STR extends TokenType { override def toString = "STR" }

  // Operators
  object ASSIGN extends TokenType { override def toString = "=" }
  object PLUS extends TokenType { override def toString = "+" }
  object BANG extends TokenType { override def toString = "!" }
  object MINUS extends TokenType { override def toString = "-" }
  object SLASH extends TokenType { override def toString = "/" }
  object ASTERISK extends TokenType { override def toString = "*" }
  object EQ extends TokenType { override def toString = "==" }
  object GT extends TokenType { override def toString = ">" }
  object LT extends TokenType { override def toString = "<" }
  object NOT_EQ extends TokenType { override def toString = "!=" }

  // Delimiters
  object COMMA extends TokenType { override def toString = "," }
  object SEMICOLON extends TokenType { override def toString = ";" }

  object LPAREN extends TokenType { override def toString = "(" }
  object RPAREN extends TokenType { override def toString = ")" }
  object LBRACE extends TokenType { override def toString = "{" }
  object RBRACE extends TokenType { override def toString = "}" }
  object LBRACKET extends TokenType { override def toString = "[" }
  object RBRACKET extends TokenType { override def toString = "]" }

  // Keywords
  object FUNCTION extends TokenType { override def toString = "fn" }
  object LET extends TokenType { override def toString = "let" }
  object ELSE extends TokenType { override def toString = "else" }
  object IF extends TokenType { override def toString = "if" }
  object RETURN extends TokenType { override def toString = "return" }

  // Booleans
  object TRUE extends TokenType { override def toString = "true" }
  object FALSE extends TokenType { override def toString = "false" }

}
