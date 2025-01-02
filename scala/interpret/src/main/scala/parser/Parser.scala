package parser

import errors.ParserError
import lexer.Lexer
import parser.ast.expressions.{
  ExpressionOrdering,
  Identifier,
  InfixExpression,
  PrefixExpression
}
import parser.ast.statements.{
  ExpressionStatement,
  LetStatement,
  ReturnStatement
}
import parser.ast.{Expression, Program, Statement}
import token.{Token, TokenType}

import scala.annotation.tailrec

case class Parser(lexer: Lexer) {

  lazy val tokenIterator: Iterator[Seq[Option[Token]]] =
    lexer.getTokens.sliding(2)
  def getTokenPointers: (Option[Token], Option[Token]) = {
    callN += 1
    tokenIterator.nextOption() match {
      case Some(Seq(current, peak)) =>
        println(s"$callTabs getTokens call - $current - $peak")
        callN -= 1
        (current, peak)
      case None => (None, None)
    }
  }

  // Temporary function while no evaluation exists
  private def waitUntil(t: TokenType): Option[(Token, Token)] =
    getTokenPointers match {
      case (None, None) =>
        None
      case (Some(currentToken), Some(peakToken)) if peakToken.tokenType == t =>
        getTokenPointers
        Some((currentToken, peakToken))
      case _ => waitUntil(t)

    }

  def parseReturnStatement(
      c: Token,
      optionP: Option[Token]
  ): (Option[ReturnStatement], Seq[ParserError]) = {
    val token = c

    token match {
      case returnToken: Token if returnToken.tokenType == TokenType.RETURN =>
        waitUntil(TokenType.SEMICOLON)
        (
          Some(
            ReturnStatement(
              token,
              new Expression {
                override def tokenLiteral: String = ""

                override def expressionNode(): Unit = {}
                override def string = ""
              }
            )
          ),
          Seq.empty[ParserError]
        )
      case _ =>
        (
          None,
          Seq(ParserError(s"Expected a return statement, got ${c.tokenType}"))
        )
    }

  }

  def parseLetStatement(
      c: Token,
      optionP: Option[Token]
  ): (Option[LetStatement], Seq[ParserError]) = {
    val token = c
    val (identifier, errors) = optionP match {
      case Some(identifierToken)
          if identifierToken.tokenType == TokenType.IDENT =>
        (
          Some(Identifier(identifierToken, identifierToken.literal)),
          Seq.empty[ParserError]
        )
      case Some(otherToken) =>
        (
          None,
          Seq(
            ParserError(
              s"Token $otherToken should be from the identifier type, but got ${otherToken.tokenType}"
            )
          )
        )
    }

    getTokenPointers match {
      case (_, Some(assignToken))
          if assignToken.tokenType == TokenType.ASSIGN =>
        waitUntil(TokenType.SEMICOLON) match {
          case None => (None, errors)
          case Some((ct, pt)) =>
            (Some(LetStatement(token, identifier.get, identifier.get)), errors)
        }

      case (_, Some(otherToken)) =>
        waitUntil(TokenType.SEMICOLON)
        (
          None,
          errors ++ Seq(
            ParserError(s"Expected an ASSIGN token, but got $otherToken")
          )
        )

    }
  }

  var callN = 0
  def callTabs = "\t" * callN

  def parseExpression(
      precedence: ExpressionOrdering,
      c: Token,
      optionP: Option[Token]
  ): (Option[Expression], Seq[ParserError]) = {
    callN += 1
    println(
      s"$callTabs Parse Expression input $precedence - $c - $optionP"
    )
    val (leftExp, leftExpErrors) =
      ParserFns.prefixParseFns(this).get(c.tokenType) match {
        case Some(fn) =>
          fn(c, optionP)
        case None =>
          (
            None,
            Seq(
              ParserError(
                s"Token type ${c.tokenType} not mapped into expression parser"
              )
            )
          )
      }
    println(
      s"$callTabs Parse Expression output $leftExp - $leftExpErrors"
    )

    leftExp match {
      case Some(leftExp) =>
        callN -= 1
        (Some(leftExp), leftExpErrors)
      case None =>
        callN -= 1
        (None, leftExpErrors)
    }

  }

  def parseExpressionStatement(
      c: Token,
      optionP: Option[Token]
  ): (Option[ExpressionStatement], Seq[ParserError]) = {
    println(s"$callTabs parseExpressionStatement input $c $optionP")
    callN += 1
    parseExpression(ExpressionOrdering.Lowest, c, optionP) match {
      case (Some(expression: Expression), errors: Seq[ParserError]) =>
        optionP match {
          case Some(token) if token.tokenType == TokenType.SEMICOLON =>
            getTokenPointers

            callN -= 1
            (Some(ExpressionStatement(c, Some(expression))), errors)
          case Some(token) if expression.isInstanceOf[InfixExpression] =>
            callN -= 1
            (Some(ExpressionStatement(c, Some(expression))), errors)
          case Some(token) if expression.isInstanceOf[PrefixExpression] =>
            callN -= 1
            (Some(ExpressionStatement(c, Some(expression))), errors)
          case None =>
            callN -= 1
            (None, errors)
        }
      case (None, errors: Seq[ParserError]) =>
        callN -= 1
        (None, errors)
    }
  }

  def parseStatement: (Option[Statement], Seq[ParserError]) = {
    val (currentToken: Option[Token], peakToken: Option[Token]) =
      getTokenPointers
    (currentToken, peakToken) match {
      case (Some(c), _) if c.tokenType == TokenType.EOF =>
        (None, Seq.empty[ParserError])
      case (Some(c), optionP) =>
        c.tokenType match {
          case TokenType.LET    => parseLetStatement(c, optionP)
          case TokenType.RETURN => parseReturnStatement(c, optionP)
          case _                => parseExpressionStatement(c, optionP)
        }
      case (None, None) => (None, Seq.empty[ParserError])
    }
  }

  def parseProgram: (Program, Seq[ParserError]) = {
    @tailrec
    def parseProgramIteration(
        program: Program = Program(Seq.empty[Statement]),
        errors: Seq[ParserError] = Seq.empty[ParserError]
    ): (Program, Seq[ParserError]) = {
      if (tokenIterator.hasNext) {
        parseStatement match {
          case (None, statementErrors) =>
            parseProgramIteration(program, errors ++ statementErrors)
          case (Some(statement), statementErrors) =>
            parseProgramIteration(
              Program(program.statements :+ statement),
              errors ++ statementErrors
            )
        }
      } else {
        (program, errors)
      }
    }
    parseProgramIteration()
  }

}
