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

  private def parseIdentifierExpression(p: Parser)(
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    optionP match {
      case Some(token) if token.tokenType == SEMICOLON => p.nextTokenPointers
      case _                                           =>
    }
    (Some(Identifier(c, c.literal)), Seq.empty[ParserError])
  }

  private def parseGroupedExpression(p: Parser)(
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    p.nextTokenPointers
    val parseExpressionOutput = p.currentTokenPointer match {
      case Some(currentToken) =>
        p.parseExpression(Lowest, currentToken, p.peekTokenPointer)
      case None =>
        (None, Seq(ParserError("No tokens found after parenthesis")))
    }
    parseExpressionOutput match {
      case (Some(expression), errors) =>
        p.peekTokenPointer match {
          case Some(token) if token.tokenType == TokenType.RPAREN =>
            (Some(expression), errors)
          case _ => (None, errors :+ ParserError("Right parenthesis not found"))
        }
      case (None, errors) => (None, errors)
    }
  }

  private def parseBooleanExpression(p: Parser)(
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    optionP match {
      case Some(token) if token.tokenType == SEMICOLON => p.nextTokenPointers
      case _                                           =>
    }
    c.literal.toBooleanOption match {
      case Some(boolean) =>
        (Some(BooleanLiteral(c, boolean)), Seq.empty[ParserError])
      case None =>
        (None, Seq(ParserError(s"Token $c cannot be parsed into boolean")))
    }
  }

  private def parseIntegerLiteralExpression(p: Parser)(
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    optionP match {
      case Some(token) if token.tokenType == SEMICOLON => p.nextTokenPointers
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

    val (optionC, nextOptionP) = p.nextTokenPointers
    optionC match {
      case Some(token) =>
        p.parseExpression(ExpressionOrdering.Prefix, token, nextOptionP) match {
          case (Some(expression), errors) =>
            (Some(PrefixExpression(c, c.literal, expression)), errors)
          case (None, errors: Seq[ParserError]) =>
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

    optionP match {
      case Some(peek) =>
        p.nextTokenPointers
        p.nextTokenPointers match {
          case (Some(rightExpressionToken), rightExpressionPeekToken) =>
            val precedence =
              getPrecedence(peek)

            p.parseExpression(
              precedence,
              rightExpressionToken,
              rightExpressionPeekToken
            ) match {
              case (Some(rightExpression), errors) =>
                (
                  Some(
                    InfixExpression(peek, peek.literal, left, rightExpression)
                  ),
                  errors
                )
              case (None, errors) => (None, errors)
            }
        }
      case None =>
        (None, Seq.empty[ParserError])

    }

  }

}
