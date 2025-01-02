package parser

import errors.ParserError
import parser.ast.Expression
import parser.ast.expressions.ExpressionOrdering.{
  Equal,
  LessGreater,
  Lowest,
  Product,
  Sum
}
import parser.ast.expressions.{
  ExpressionOrdering,
  Identifier,
  InfixExpression,
  IntegerLiteral,
  PrefixExpression
}
import token.TokenType.{SEMICOLON, SLASH}
import token.{Token, TokenType}

object ParserFns {
  type ParsePrefixFn =
    (Token, Option[Token]) => (Option[Expression], Seq[ParserError])
  type ParseInfixFn =
    (Expression, Token, Option[Token]) => (Option[Expression], Seq[ParserError])

  final val precedences: Map[TokenType, ExpressionOrdering] = Map(
    TokenType.EQ -> Equal,
    TokenType.NOT_EQ -> Equal,
    TokenType.LT -> LessGreater,
    TokenType.GT -> LessGreater,
    TokenType.PLUS -> Sum,
    TokenType.MINUS -> Sum,
    TokenType.SLASH -> Product,
    TokenType.ASTERISK -> Product
  )

  def getPrecedence(t: Token): ExpressionOrdering =
    precedences.getOrElse(t.tokenType, Lowest)

  def infixParseFns(p: Parser): Map[TokenType, ParseInfixFn] = Map(
    TokenType.EQ -> parseInfixExpression(p),
    TokenType.NOT_EQ -> parseInfixExpression(p),
    TokenType.LT -> parseInfixExpression(p),
    TokenType.GT -> parseInfixExpression(p),
    TokenType.PLUS -> parseInfixExpression(p),
    TokenType.MINUS -> parseInfixExpression(p),
    TokenType.SLASH -> parseInfixExpression(p),
    TokenType.ASTERISK -> parseInfixExpression(p)
  )

  def prefixParseFns(p: Parser): Map[TokenType, ParsePrefixFn] =
    Map(
      TokenType.IDENT -> parseIdentifierExpression(p),
      TokenType.INT -> parseIntegerLiteralExpression(p),
      TokenType.BANG -> parsePrefixExpression(p),
      TokenType.MINUS -> parsePrefixExpression(p)
    )

  private def parseIdentifierExpression(p: Parser)(
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    optionP match {
      case Some(token) if token.tokenType == SEMICOLON => p.getTokenPointers
      case _                                           =>
    }
    (Some(Identifier(c, c.literal)), Seq.empty[ParserError])
  }

  private def parseIntegerLiteralExpression(p: Parser)(
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    optionP match {
      case Some(token) if token.tokenType == SEMICOLON => p.getTokenPointers
      case _                                           =>
    }
    c.literal.toIntOption match {
      case Some(integer) =>
        (Some(IntegerLiteral(c, integer)), Seq.empty[ParserError])
      case None =>
        (None, Seq(ParserError(s"Token $c cannot be parsed into integer")))
    }
  }

  private def parsePrefixExpression(p: Parser)(
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    p.callN += 1
    val (optionC, nextOptionP) = p.getTokenPointers
    println(
      s"${p.callTabs} parsePrefixExpression Input (c, op, nc, np) - ",
      c,
      optionP,
      optionC,
      nextOptionP
    )
    optionC match {
      case Some(token) =>
        p.parseExpression(ExpressionOrdering.Prefix, token, nextOptionP) match {
          case (Some(expression), errors) =>
            p.callN -= 1
            (Some(PrefixExpression(c, c.literal, expression)), errors)
          case (None, errors: Seq[ParserError]) =>
            p.callN -= 1
            (
              None,
              errors :+ ParserError(
                s"Prefix operator for token $c had the following expression failed"
              )
            )
        }
    }
  }

  private def parseInfixExpression(p: Parser)(
      left: Expression,
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {

    val precedence = getPrecedence(c)
    val (nextC, nextOptionP) = p.getTokenPointers
    (nextC, nextOptionP) match {
      case (Some(nextC), nextOptionP) =>
        p.parseExpression(precedence, nextC, nextOptionP) match {
          case (Some(right), errors) =>
            optionP match {
              case Some(operatorToken) =>
                val infixExp =
                  InfixExpression(
                    operatorToken,
                    operatorToken.literal,
                    left,
                    right
                  )

                (
                  Some(
                    infixExp
                  ),
                  Seq.empty[ParserError]
                )
              case None =>
                (
                  None,
                  errors :+ ParserError(
                    s"Operator token was null on expression with inputs $c, $optionP"
                  )
                )
            }
          case (None, errors) =>
            (
              None,
              errors :+ ParserError(
                s"Right expression of (${left.string}${c.literal}...) failed"
              )
            )
        }
      case (None, _) =>
        (
          None,
          Seq.empty[ParserError]
        )
    }

  }

}
