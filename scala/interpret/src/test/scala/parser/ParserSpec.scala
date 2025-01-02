package parser

import errors.ParserError
import lexer.Lexer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.must.Matchers.have
import org.scalatest.matchers.should.Matchers.{a, convertToAnyShouldWrapper}
import parser.ast.expressions.{
  Identifier,
  InfixExpression,
  IntegerLiteral,
  PrefixExpression
}
import parser.ast.statements.ExpressionStatement
import parser.ast.{Program, Statement}
import token.Token
import token.TokenType._

class ParserSpec extends AnyFlatSpec with ParserTestUtils {
  "tokenReader" should "iterate the lexer if each call" in {
    def input = "let x = 5;"
    val lexer = Lexer(input).next
    val parser = Parser(lexer)
    val expectedIterations = Seq(
      (Some(Token(LET, "let")), Some(Token(IDENT, "x"))),
      (Some(Token(IDENT, "x")), Some(Token(ASSIGN, "="))),
      (Some(Token(ASSIGN, "=")), Some(Token(INT, "5"))),
      (Some(Token(INT, "5")), Some(Token(SEMICOLON, ";"))),
      (Some(Token(SEMICOLON, ";")), Some(Token(EOF, "")))
    )

    expectedIterations.map { expected =>
      parser.getTokenPointers match {
        case (current, peak) =>
          assert(expected._1 == current && expected._2 == peak)
      }
    }
  }

  "LetStatement" should "work" in {
    val input =
      """
        let x = 5;
        let y = 10;
        let foobar = 838383;""".stripMargin
    val lexer = Lexer(input).next
    val parser = Parser(lexer)
    val program = parser.parseProgram
    val expectedLength = 3
    val expectedIdentifiers = Seq("x", "y", "foobar")
    program match {
      case (program: Program, errors: Seq[ParserError]) =>
        if (errors.nonEmpty) {
          println("Got the following errors: ")
          println(errors.map(_.message).mkString("\n"))
        }
        if (program.statements.length != expectedLength) {
          fail(
            s"Program contained only ${program.statements.length} statements, should be $expectedLength"
          )
        }
        program.statements.zip(expectedIdentifiers).map {
          case (statement: Statement, identifier: String) =>
            testLetStatement(statement, identifier)
        }
      case _ => fail("parse program returned none")
    }
  }

  "ReturnStatement" should "work" in {
    val input =
      """
      return 5;
      return 10;
      return 993322;""".stripMargin
    val lexer = Lexer(input).next
    val parser = Parser(lexer)
    val program = parser.parseProgram
    val expectedLength = 3
    program match {
      case (program: Program, errors: Seq[ParserError]) =>
        if (errors.nonEmpty) {
          println("Got the following errors: ")
          println(errors.map(_.message).mkString("\n"))
        }
        if (program.statements.length != expectedLength) {
          fail(
            s"Program contained only ${program.statements.length} statements, should be $expectedLength"
          )
        }
        assert(!program.statements.exists(_.tokenLiteral != "return"))
      case _ => fail("parse program returned none")
    }
  }

  "ExpressionParser - identifiers" should "Be correctly parsed" in {
    val input = "foobar;"

    val l = Lexer(input).next
    val p = Parser(l)
    val (program: Program, errors: Seq[ParserError]) = p.parseProgram

    assert(program.statements.length == 1)
    program.statements.head match {
      case stmt: ExpressionStatement =>
        stmt.expression match {
          case Some(ident: Identifier) =>
            assert(ident.value == "foobar")
            assert(ident.tokenLiteral == "foobar")
          case _ => fail("Statement expression should be identifier")
        }

      case _ => fail("Statement is not an expression statement")
    }
  }

  "ExpressionParser - integer literals" should "Be correctly parsed" in {
    val input = "420;"
    testIntegerLiteral(input, 420)

  }

  "ExpressionParser - Prefix Operators" should "Be correctly parsed" in {

    val prefixTests: Seq[(String, String, BigInt)] = Seq(
      ("!5;", "!", 5),
      ("-15;", "-", 15)
    )

    prefixTests.foreach {
      case (input: String, operator: String, value: BigInt) =>
        val l = Lexer(input).next
        val p = Parser(l)
        val (program, errors) = p.parseProgram
        program.statements should have length 1
        program.statements.head match {
          case es: ExpressionStatement if es.expression.nonEmpty =>
            es.expression.get shouldBe a[PrefixExpression]
            val expression = es.expression.get.asInstanceOf[PrefixExpression]
            expression.operator shouldEqual operator
            testIntegerLiteral(expression.right.string + ";", value)

          case _ => fail("Statement is not a prefix expression")
        }
    }
  }

  "ExpressionParser - Infix Operator String Parser" should "Output the strings correctly" in {

    val prefixTests: Seq[(String, String)] = Seq(
      //("-a * b", "((-a) * b)"),
      ("!-a;", "(!(-a))"),
      ("a + b + c", "((a + b) + c)"),
      ("a + b - c", "((a + b) - c)"),
      ("a * b * c", "((a * b) * c)"),
      ("a * b / c", "((a * b) / c)"),
      ("a + b / c", "(a + (b / c))"),
      ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
      ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
      ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
      ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
      (
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
      )
    )

    prefixTests.foreach { case (input: String, output: String) =>
      val l = Lexer(input).next
      val p = Parser(l)
      println(s"evaluation of $input...")
      val (program, errors) = p.parseProgram
      errors shouldBe Matchers.empty
      program.string shouldEqual output
      println("ok")
    }
  }

  "ExpressionParser - Infix Operators" should "Be correctly parsed" in {

    val prefixTests: Seq[(String, BigInt, String, BigInt)] = Seq(
      ("5 + 5;", 5, "+", 5),
      ("5 - 5;", 5, "-", 5),
      ("5 * 5;", 5, "*", 5),
      ("5 / 5;", 5, "/", 5),
      ("5 > 5;", 5, ">", 5),
      ("5 < 5;", 5, "<", 5),
      ("5 == 5;", 5, "==", 5),
      ("5 != 5;", 5, "!=", 5)
    )

    prefixTests.foreach {
      case (
            input: String,
            leftValue: BigInt,
            operator: String,
            rightValue: BigInt
          ) =>
        val l = Lexer(input).next
        val p = Parser(l)
        val (program, errors) = p.parseProgram
        errors shouldBe Matchers.empty
        program.statements should have length 1
        program.statements.head match {
          case es: ExpressionStatement if es.expression.nonEmpty =>
            es.expression.get shouldBe a[InfixExpression]
            val expression = es.expression.get.asInstanceOf[InfixExpression]
            expression.operator shouldEqual operator
            testIntegerLiteral(expression.right.string + ";", rightValue)
            testIntegerLiteral(expression.left.string + ";", leftValue)

          case _ => fail("Statement is not a prefix expression")
        }
    }
  }

}
