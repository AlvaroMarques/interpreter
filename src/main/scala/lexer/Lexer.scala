package lexer

import lexer.Lexer.identifierLookupTable
import token.{Token, TokenType}

import scala.annotation.tailrec

case class Lexer(input: String, position: Int = -1, ch: Byte = 0)
    extends CharIdentification {

  def nextPosition: Int = position + 1
  def nextChar: Byte = {
    if (nextPosition >= input.length) 0 else input(nextPosition).toByte
  }
  final def next: Lexer = Lexer(input, nextPosition, nextChar)

  def readIdentifier: (String, Lexer) = {
    if (isLetter(ch)) {
      val (identifierTail: String, nextLexer: Lexer) = next.readIdentifier
      (ch.toChar.toString ++ identifierTail, nextLexer)
    } else {
      ("", this)
    }
  }

  def readString: (String, Lexer) = {
    if ((nextChar != '\"' || (nextChar == '\"' && ch.toChar == '\\')) && ch != 0) {
      (ch.toChar, nextChar) match {
        case ('\\', 'n') =>
          val (string, nextLexer) = next.readString
          ('\n'.toString ++ string, nextLexer)
        case _ =>
          val (string, nextLexer) = next.readString
          (ch.toChar.toString ++ string, nextLexer)
      }
    } else {
      (ch.toChar.toString ++ "\"", next)
    }
  }

  def readDigit: (String, Lexer) = {
    if (isDigit(ch)) {
      val (identifierTail: String, nextLexer: Lexer) = next.readDigit
      (ch.toChar.toString ++ identifierTail, nextLexer)
    } else {
      ("", this)
    }
  }

  def lookUpIdentifier(identifier: String): TokenType = {
    identifierLookupTable.getOrElse(identifier, TokenType.IDENT)
  }

  def getTokens: Iterator[Option[Token]] = {
    @tailrec def recursiveGetToken(
        lexer: Lexer,
        currentIterator: Iterator[Option[Token]] = Iterator.empty[Option[Token]]
    ): Iterator[Option[Token]] = {
      lexer.token match {
        case (Some(token), nextLexer: Lexer)
            if token.tokenType != TokenType.EOF =>
          recursiveGetToken(nextLexer, currentIterator ++ Iterator(Some(token))  )
        case (Some(Token(TokenType.EOF, literal)), _: Lexer) =>
          currentIterator ++ Iterator(Some(Token(TokenType.EOF, literal)))
        case _ =>
          throw new RuntimeException("Unexpected token values")
      }
    }
    recursiveGetToken(this)

  }

  def token: (Option[Token], Lexer) = {
    if (isWhitespace(ch)) {
      next.token
    } else {
      ch match {
        case '\"' =>
          val (identifier: String, nextLexer: Lexer) = readString
          (
            Some(
              Token(TokenType.STR, identifier.replace("\nn", "\n"))
            ),
            nextLexer.next
          )
        case '=' =>
          if (next.ch == '=') {
            (
              Some(
                Token(
                  TokenType.EQ,
                  ch.toChar.toString ++ next.ch.toChar.toString
                )
              ),
              next.next
            )
          } else
            (Some(Token(TokenType.ASSIGN, ch.toChar.toString)), next)
        case ';' =>
          (Some(Token(TokenType.SEMICOLON, ch.toChar.toString)), next)
        case '[' =>
          (Some(Token(TokenType.LBRACKET, ch.toChar.toString)), next)
        case ']' =>
          (Some(Token(TokenType.RBRACKET, ch.toChar.toString)), next)
        case '(' =>
          (Some(Token(TokenType.LPAREN, ch.toChar.toString)), next)
        case ')' =>
          (Some(Token(TokenType.RPAREN, ch.toChar.toString)), next)
        case ',' =>
          (Some(Token(TokenType.COMMA, ch.toChar.toString)), next)
        case '-' =>
          (Some(Token(TokenType.MINUS, ch.toChar.toString)), next)
        case '+' =>
          (Some(Token(TokenType.PLUS, ch.toChar.toString)), next)
        case '{' =>
          (Some(Token(TokenType.LBRACE, ch.toChar.toString)), next)
        case '}' =>
          (Some(Token(TokenType.RBRACE, ch.toChar.toString)), next)
        case '*' =>
          (Some(Token(TokenType.ASTERISK, ch.toChar.toString)), next)
        case '/' =>
          (Some(Token(TokenType.SLASH, ch.toChar.toString)), next)
        case '<' =>
          (Some(Token(TokenType.LT, ch.toChar.toString)), next)
        case '>' =>
          (Some(Token(TokenType.GT, ch.toChar.toString)), next)
        case '!' =>
          if (next.ch == '=') {
            (
              Some(
                Token(
                  TokenType.NOT_EQ,
                  ch.toChar.toString ++ next.ch.toChar.toString
                )
              ),
              next.next
            )
          } else {
            (Some(Token(TokenType.BANG, ch.toChar.toString)), next)
          }
        case 0 =>
          (Some(Token(TokenType.EOF, "")), next)
        case ch if isDigit(ch) =>
          val (identifier: String, nextLexer: Lexer) = readDigit
          (
            Some(
              Token(TokenType.INT, identifier)
            ),
            nextLexer
          )
        case ch if isLetter(ch) =>
          val (identifier: String, nextLexer: Lexer) = readIdentifier
          (
            Some(
              Token(lookUpIdentifier(identifier), identifier)
            ),
            nextLexer
          )
        case _ => (Some(Token(TokenType.ILLEGAL, "")), next)
      }
    }

  }

}

object Lexer {

  final val identifierLookupTable: Map[String, TokenType] = Map(
    "fn" -> TokenType.FUNCTION,
    "return" -> TokenType.RETURN,
    "true" -> TokenType.TRUE,
    "false" -> TokenType.FALSE,
    "let" -> TokenType.LET,
    "if" -> TokenType.IF,
    "else" -> TokenType.ELSE,
    "while" -> TokenType.WHILE
  )
}
