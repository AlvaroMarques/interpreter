package parser

import lexer.Lexer
import org.scalatest.Assertions.fail
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parser.ast.{Expression, Statement}
import parser.ast.expressions.{
  ArrayLiteral,
  BooleanLiteral,
  Identifier,
  IntegerLiteral,
  StringLiteral
}
import parser.ast.statements.{ExpressionStatement, LetStatement}

trait ParserTestUtils extends ParserMatchers {
  def testLetStatement(s: Statement, expectedName: String): Unit = {
    assert(
      s.tokenLiteral == "let",
      s"Statement literal is not let!, got ${s.tokenLiteral}"
    )
    s match {
      case LetStatement(_, identifier, _) =>
        assert(
          identifier.tokenLiteral == expectedName,
          s"Identifier with unexpected token literal: $identifier"
        )
        assert(
          identifier.value == expectedName,
          s"Identifier with unexpected value: $identifier"
        )
      case _ => fail(s"S is not let statement!, got $s")
    }
  }

  def testBooleanLiteral(input: String, expectedValue: Boolean): Unit = {
    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()

    program.statements.length shouldBe 1
    p.errors shouldBe Matchers.empty
    program.statements.head match {
      case stmt: ExpressionStatement =>
        stmt.expression match {
          case Some(ident: BooleanLiteral) =>
            assert(ident.value == expectedValue)
            assert(ident.tokenLiteral == expectedValue.toString)
          case _ => fail("Statement expression should be IntegerLiteral")
        }

      case _ => fail("Statement is not an expression statement")
    }
  }

  def testIdentifier(input: String, expectedValue: String): Unit = {
    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()

    program.statements.length shouldBe 1
    p.errors shouldBe Matchers.empty
    program.statements.head match {
      case stmt: ExpressionStatement =>
        stmt.expression match {
          case Some(ident: Identifier) =>
            assert(ident.value == expectedValue)
            assert(ident.tokenLiteral == expectedValue)
          case _ => fail("Statement expression should be IntegerLiteral")
        }

      case _ => fail("Statement is not an expression statement")
    }
  }

  def testStringLiteral(input: String, expectedValue: String): Unit = {
    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()

    program.statements.length shouldBe 1
    p.errors shouldBe Matchers.empty
    program.statements.head match {
      case stmt: ExpressionStatement =>
        stmt.expression match {
          case Some(ident: StringLiteral) =>
            assert(
              ident.value == expectedValue,
              s"${ident.tokenLiteral} did not equal $expectedValue"
            )
          case _ => fail("Statement expression should be StringLiteral")
        }

      case _ => fail("Statement is not an expression statement")
    }
  }

  def testArrayLiteral(input: String, expectedValue: Seq[Expression]): Unit = {
    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()

    program.statements.length shouldBe 1
    p.errors shouldBe Matchers.empty
    program.statements.head match {
      case stmt: ExpressionStatement =>
        stmt.expression match {
          case Some(ident: ArrayLiteral) =>
            assert(
              ident.values == expectedValue,
              s"Values should be $expectedValue but are actually ${ident.string}"
            )
          case _ => fail("Statement expression should be IntegerLiteral")
        }

      case _ => fail("Statement is not an expression statement")
    }
  }

  def testIntegerLiteral(input: String, expectedValue: BigInt): Unit = {
    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()

    program.statements.length shouldBe 1
    p.errors shouldBe Matchers.empty
    program.statements.head match {
      case stmt: ExpressionStatement =>
        stmt.expression match {
          case Some(ident: IntegerLiteral) =>
            assert(ident.value == expectedValue)
            assert(ident.tokenLiteral == expectedValue.toString)
          case _ => fail("Statement expression should be IntegerLiteral")
        }

      case _ => fail("Statement is not an expression statement")
    }
  }
}
