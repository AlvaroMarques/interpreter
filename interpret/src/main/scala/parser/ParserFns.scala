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
  BooleanLiteral,
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

  private def parseInfixExpression(p: Parser) = ???

  def prefixParseFns(p: Parser): Map[TokenType, ParsePrefixFn] =
    Map(
      TokenType.IDENT -> parseIdentifierExpression(p),
      TokenType.INT -> parseIntegerLiteralExpression(p),
      TokenType.BANG -> parsePrefixExpression(p),
      TokenType.MINUS -> parsePrefixExpression(p),
      TokenType.FALSE -> parseBooleanExpression(p),
      TokenType.TRUE -> parseBooleanExpression(p),
      TokenType.LPAREN -> parseGroupedExpression(p)
    )

  private def parseGroupedExpression(p: Parser) = ???

  private def parseBooleanExpression(p: Parser) = ???

  private def parsePrefixExpression(p: Parser) = ???

  private def parseIntegerLiteralExpression(p: Parser) = ???

  private def parseIdentifierExpression(p: Parser) = ???

}
