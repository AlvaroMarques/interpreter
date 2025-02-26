package parser

import lexer.Lexer
import parser.Parser.EOFToken
import parser.ast.expressions.ExpressionOrdering.Lowest
import parser.ast.statements.{
  BlockStatement,
  ExpressionStatement,
  LetStatement,
  ReturnStatement
}
import parser.ast.{Program, Statement}
import token.TokenType._
import token.{Token, TokenType}

case class Parser(lexer: Lexer)
    extends ParserDebugger
    with ParserErrors
    with ParserExpressions {
  val tokens: Seq[Seq[Token]] = {
    lexer.next.getTokens.collect { case Some(token) => token }.sliding(2).toSeq
  }

  var iteratorCounter: Int = 0

  def cToken: Option[Token] =
    if (iteratorCounter < tokens.length)
      tokens(iteratorCounter).headOption
    else None

  def pToken: Option[Token] =
    if (iteratorCounter < tokens.length)
      tokens(iteratorCounter).tail.headOption
    else None

  def expectPeek(tokenType: TokenType): Boolean = {
    pToken match {
      case Some(Token(t, _)) if t == tokenType =>
        nextTokens()
        true
      case _ =>
        peekError(tokenType)
        false
    }
  }

  def nextTokens(): Unit = {
    iteratorCounter += 1
    logger.whenDebugEnabled {
      debugTokens()
    }
  }

  def parseProgram(): Program = {
    var programStatements = Seq.empty[Statement]
    while (cToken.getOrElse(EOFToken).tokenType != EOF) {
      val statement = cToken match {
        case Some(Token(LET, _))    => parseLetStatement()
        case Some(Token(RETURN, _)) => parseReturnStatement()
        case _ =>
          parseExpressionsStatement()
      }
      programStatements ++= Seq(statement).flatten
      nextTokens()
    }
    Program(programStatements)
  }

  def parseStatement(): Option[Statement] = cToken match {
    case Some(Token(LET, _))    => parseLetStatement()
    case Some(Token(RETURN, _)) => parseReturnStatement()
    case _ =>
      parseExpressionsStatement()
  }

  def parseBlockStatement(): Option[BlockStatement] = {
    val token = cToken
    logger.debug(s"Parsing block statement, first token is $token")
    token match {
      case Some(token) if token.tokenType == LBRACE =>
        nextTokens()
        var statements = Seq.empty[Statement]
        while (cToken.getOrElse(EOFToken).tokenType match {
                 case TokenType.RBRACE => false
                 case TokenType.EOF    => false
                 case _                => true
               }) {
          statements ++= Seq(parseStatement()).flatten
          nextTokens()
        }
        Some(BlockStatement(token, statements))
      case _ => None
    }
  }

  def parseExpressionsStatement(): Option[ExpressionStatement] = {
    (cToken, parseExpression(Lowest)) match {
      case (Some(token), Some(expression)) =>
        pToken match {
          case Some(Token(SEMICOLON, _)) =>
            nextTokens()
            Some(ExpressionStatement(token, Some(expression)))
          case _ =>
            Some(ExpressionStatement(token, Some(expression)))
        }
      case _ => None
    }
  }

  def parseLetStatement(): Option[LetStatement] = {
    val token = cToken.get
    val identifier = parseIdentifierPeak()
    nextTokens()
    (cToken, identifier) match {
      case (Some(Token(ASSIGN, _)), Some(identifier)) =>
        nextTokens()
        parseExpression(Lowest) match {
          case Some(value) =>
            Some(LetStatement(token, identifier, value))
          case _ => None
        }
      case _ => None
    }
  }

  def parseReturnStatement(): Option[ReturnStatement] = {
    val token = cToken.get
    nextTokens()
    parseExpression(Lowest) match {
      case Some(value) =>
        Some(ReturnStatement(token = token, returnValue = value))
      case _ =>
        None
    }
  }

}

object Parser {
  final val EOFToken = Token(EOF, EOF.toString)
}
