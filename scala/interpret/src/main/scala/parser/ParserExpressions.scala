package parser

import errors.ParserError
import parser.Parser.EOFToken
import parser.ast.Expression
import parser.ast.expressions.ExpressionOrdering._
import parser.ast.expressions._
import token.TokenType.{ELSE, EOF, IDENT, LPAREN, RBRACE, RPAREN, SEMICOLON}
import token.{Token, TokenType}

import scala.util.control.Breaks.break

trait ParserExpressions {
  parser: Parser with ParserErrors =>
  final type PrefixParserFn = () => Option[Expression]
  final type InfixParserFn = Expression => Option[Expression]
  final val prefixParserFns: Map[TokenType, PrefixParserFn] = Map(
    TokenType.BANG -> parsePrefixExpression,
    TokenType.FALSE -> parseBoolean,
    TokenType.IDENT -> parseIdentifier,
    TokenType.INT -> parseInteger,
    TokenType.LPAREN -> parseGroupedExpression,
    TokenType.MINUS -> parsePrefixExpression,
    TokenType.TRUE -> parseBoolean,
    TokenType.IF -> parseIfExpression,
    TokenType.FUNCTION -> parseFunctionExpression
  )
  final val infixParserFns: Map[TokenType, InfixParserFn] = Map(
    (TokenType.PLUS, parseInfixExpression),
    (TokenType.MINUS, parseInfixExpression),
    (TokenType.SLASH, parseInfixExpression),
    (TokenType.ASTERISK, parseInfixExpression),
    (TokenType.EQ, parseInfixExpression),
    (TokenType.NOT_EQ, parseInfixExpression),
    (TokenType.LT, parseInfixExpression),
    (TokenType.GT, parseInfixExpression)
  )
  final val precedence: Map[TokenType, ExpressionOrdering] = Map(
    TokenType.EQ -> Equal,
    TokenType.NOT_EQ -> Equal,
    TokenType.LT -> LessGreater,
    TokenType.GT -> LessGreater,
    TokenType.PLUS -> Sum,
    TokenType.MINUS -> Sum,
    TokenType.SLASH -> Product,
    TokenType.ASTERISK -> Product
  )

  def parseExpressionMock(): Some[Expression] = {
    while (
      cToken.getOrElse(EOFToken).tokenType match {
        case EOF       => false
        case SEMICOLON => false
        case _         => true
      }
    ) {
      nextTokens()
    }
    Some(Identifier(EOFToken, ""))
  }

  def parseFunctionParameters(): Option[Seq[Identifier]] = {
    logger.debug(s"Functions parameters -> token $cToken $pToken")
    cToken match {
      case Some(Token(TokenType.LPAREN, _)) =>
        logger.debug("here")
        var identifiers = Seq.empty[Identifier]
        while (
          pToken
            .getOrElse(EOFToken)
            .tokenType != TokenType.RPAREN && pToken.isDefined
        ) {
          logger.debug("New iteration: ")
          nextTokens()
          (cToken, pToken) match {
            case (Some(cToken), Some(Token(TokenType.RPAREN, _)))
                if cToken.tokenType == TokenType.IDENT =>
              identifiers :+= Identifier(cToken, cToken.literal)
            case (Some(cToken), Some(Token(TokenType.COMMA, _)))
                if cToken.tokenType == TokenType.IDENT =>
              identifiers :+= Identifier(cToken, cToken.literal)
              logger.debug("Adding normally identifiers")
              nextTokens()
            case _ =>
              logger.debug(
                s"Something weird is happening with $cToken and $pToken"
              )
          }
        }
        if (expectPeek(RPAREN)) {
          nextTokens()
          Some(identifiers)
        } else {
          None
        }
      case _ => None
    }
  }

  def parseFunctionExpression(): Option[FunctionLiteral] = {
    parser.cToken match {
      case Some(token) =>
        if (expectPeek(LPAREN)) {
          parseFunctionParameters() match {
            case Some(parameters) =>
              parser.parseBlockStatement() match {
                case Some(body) =>
                  Some(FunctionLiteral(token, parameters, body))
                case _ => None
              }
            case _ => None
          }
        } else {
          None
        }
      case _ => None
    }
  }

  def parseInfixExpression(
      leftExpression: Expression
  ): Option[InfixExpression] = {
    cToken match {
      case Some(token) =>
        logger.debug("On infix")
        val operator = token.literal
        val precedence = currPrecedence
        nextTokens()
        parser.parseExpression(precedence) match {
          case Some(rightExpression) =>
            Some(
              InfixExpression(token, operator, leftExpression, rightExpression)
            )
          case _ => None
        }
      case None => None
    }
  }

  def currPrecedence: ExpressionOrdering =
    precedence.getOrElse(cToken.getOrElse(EOFToken).tokenType, Lowest)

  def parseExpression(precedence: ExpressionOrdering): Option[Expression] = {
    var leftExp: Option[Expression] =
      prefixParserFns.get(cToken.getOrElse(EOFToken).tokenType) match {
        case Some(function: PrefixParserFn) => function()
        case None =>
          cToken match {
            case Some(Token(SEMICOLON, _)) =>
              None
            case _ =>
              withNoPrefixParserFoundFor(cToken)
              None
          }
      }
    while ({
      pToken.getOrElse(EOFToken).tokenType match {
        case SEMICOLON => false
        case EOF       => false
        case _         => true
      }
    } && (precedence < peekPrecedence)) {
      infixParserFns.get(pToken.getOrElse(EOFToken).tokenType) match {
        case Some(function: InfixParserFn) =>
          leftExp = leftExp match {
            case Some(leftExp) =>
              nextTokens()
              function(leftExp)
            case None =>
              nextTokens()
              None
          }
        case None =>
      }
    }
    leftExp
  }

  def peekPrecedence: ExpressionOrdering =
    precedence.getOrElse(pToken.getOrElse(EOFToken).tokenType, Lowest)

  def withNoPrefixParserFoundFor(token: Option[Token]): Unit = {
    parser.errors ++= Seq(
      ParserError(s"No prefix parser function found for $token")
    )
  }

  def parseGroupedExpression(): Option[Expression] = {
    logger.debug("Start of grouped expression")
    nextTokens()
    parseExpression(Lowest) match {
      case Some(expression) =>
        if (expectPeek(RPAREN)) {
          logger.debug("End of grouped expression")
          Some(expression)
        } else {
          logger.debug(
            s"Bad End of grouped expression, expression = ${expression.toString}"
          )
          None
        }
      case _ => None
    }
  }

  def parseIfExpression(): Option[IfExpression] = {
    val token = parser.cToken
    if (!expectPeek(LPAREN)) {
      None
    } else {
      val condition = parser.parseGroupedExpression()
      parser.nextTokens() // Jumping over RPAREN
      val consequence = parser.parseBlockStatement()
      (condition, consequence) match {
        case (Some(condition), Some(consequence)) =>
          (cToken, pToken) match {
            case (Some(Token(RBRACE, _)), Some(Token(ELSE, _))) =>
              parser.nextTokens()
              parser.nextTokens()
              val alternative = parser.parseBlockStatement()
              Some(
                IfExpression(
                  token.getOrElse(EOFToken),
                  condition,
                  consequence,
                  alternative
                )
              )
            case (Some(Token(RBRACE, _)), _) =>
              Some(
                IfExpression(token.getOrElse(EOFToken), condition, consequence)
              )
            case _ => None

          }
        case _ => None
      }
    }
  }

  def parsePrefixExpression(): Option[PrefixExpression] = {
    cToken match {
      case Some(token) =>
        val operator = token.literal
        nextTokens()
        parser.parseExpression(Prefix) match {
          case Some(rightExpression) =>
            Some(PrefixExpression(token, operator, rightExpression))
          case _ => None
        }
      case _ => None
    }

  }

  def parseInteger(): Option[IntegerLiteral] = {
    cToken match {
      case Some(token) if token.literal.toIntOption.isDefined =>
        Some(IntegerLiteral(token, token.literal.toInt))
      case _ => None
    }
  }

  def parseBoolean(): Option[BooleanLiteral] = {
    cToken match {
      case Some(token) if token.literal.toBooleanOption.isDefined =>
        Some(BooleanLiteral(token, token.literal.toBoolean))
      case _ => None
    }
  }

  def parseIdentifier(): Option[Identifier] = {
    cToken match {
      case Some(token) if token.tokenType == IDENT =>
        Some(Identifier(token, token.literal))
      case _ => None
    }
  }

  def parseIdentifierPeak(): Option[Identifier] = {
    if (expectPeek(IDENT)) {
      cToken match {
        case Some(token) => Some(Identifier(token, token.literal))
        case _           => None
      }
    } else {
      None
    }
  }

}
