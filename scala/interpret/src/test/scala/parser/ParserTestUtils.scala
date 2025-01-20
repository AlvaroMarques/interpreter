package parser

import errors.ParserError
import lexer.Lexer
import org.scalatest.Assertions.fail
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.must.Matchers.have
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parser.ast.expressions.{BooleanLiteral, Identifier, IntegerLiteral}
import parser.ast.{Program, Statement}
import parser.ast.statements.{ExpressionStatement, LetStatement}

trait ParserTestUtils {
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
    val l = Lexer(input).next
    val p = Parser(l)
    val (program: Program, errors: Seq[ParserError]) = p.parseProgram

    program.statements.length shouldBe 1
    errors shouldBe Matchers.empty
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
    val l = Lexer(input).next
    val p = Parser(l)
    val (program: Program, errors: Seq[ParserError]) = p.parseProgram

    assert(program.statements.length == 1)
    errors shouldBe Matchers.empty
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

  def testIntegerLiteral(input: String, expectedValue: BigInt): Unit = {
    val l = Lexer(input).next
    val p = Parser(l)
    val (program: Program, errors: Seq[ParserError]) = p.parseProgram

    assert(program.statements.length == 1)
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
