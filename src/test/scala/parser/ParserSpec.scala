package parser

import lexer.Lexer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.must.Matchers.{a, have}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parser.ast.Statement
import parser.ast.expressions.{
  Identifier,
  InfixExpression,
  IntegerLiteral,
  PrefixExpression,
  StringLiteral
}
import parser.ast.statements.ExpressionStatement
import token.{Token, TokenType}
import token.TokenType._

class ParserSpec extends AnyFlatSpec with ParserTestUtils {
  "tokenReader" should "iterate the lexer if each call" in {
    def input = "let x = 5;"

    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val expectedIterations = Seq(
      (Token(LET, "let"), Token(IDENT, "x")),
      (Token(IDENT, "x"), Token(ASSIGN, "=")),
      (Token(ASSIGN, "="), Token(INT, "5")),
      (Token(INT, "5"), Token(SEMICOLON, ";")),
      (Token(SEMICOLON, ";"), Token(EOF, ""))
    )
    expectedIterations.map {
      case (cExpected: Token, pExpected: Token) =>
        parser.cToken shouldEqual Some(cExpected)
        parser.pToken shouldEqual Some(pExpected)
        parser.nextTokens()
    }
  }

  "LetStatement" should "work" in {
    val input =
      """
        let x = 5;
        let y = 10;
        let foobar = 838383;""".stripMargin
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    val expectedLength = 3
    val expectedIdentifiers = Seq("x", "y", "foobar")
    parser.errors.length shouldEqual 0
    if (program.statements.length != expectedLength) {
      fail(
        s"Program contained only ${program.statements.length} statements, should be $expectedLength"
      )
    }
    program.statements.zip(expectedIdentifiers).map {
      case (statement: Statement, identifier: String) =>
        testLetStatement(statement, identifier)
    }
  }

  "ReturnStatement" should "work" in {
    val input =
      """
      return 5;
      return 10;
      return 993322;""".stripMargin
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    val expectedLength = 3
    if (parser.errors.nonEmpty) {
      println("Got the following errors: ")
      println(parser.errors.map(_.message).mkString("\n"))
    }
    if (program.statements.length != expectedLength) {
      fail(
        s"Program contained only ${program.statements.length} statements, should be $expectedLength"
      )
    }
    assert(!program.statements.exists(_.tokenLiteral != "return"))
  }

  "ExpressionParser - identifiers" should "Be correctly parsed" in {
    val input = "foobar;"

    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()

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

  "ExpressionParser - boolean literals" should "Be correctly parsed" in {
    val inputTrue = "true"
    testBooleanLiteral(inputTrue, expectedValue = true)
    val inputFalse = "false;"
    testBooleanLiteral(inputFalse, expectedValue = false)
  }

  "ExpressionParser - string literals" should "Be correctly parsed" in {
    val input = "\"everything is going to be alright\";"
    testStringLiteral(input, "everything is going to be alright")
  }

  "ExpressionParser - array literals" should "Be correctly parsed" in {
    val input = "[1, 2, 3];"
    testArrayLiteral(
      input,
      Seq(1, 2, 3)
        .map(value => Token(TokenType.INT, value.toString))
        .map(t => IntegerLiteral(t, t.literal.toInt))
    )
    testArrayLiteral(
      "[\"hey\", 12];",
      Seq(
        StringLiteral(Token(TokenType.STR, "\"hey\"")),
        IntegerLiteral(Token(TokenType.INT, "12"), 12)
      )
    )

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
        val l = Lexer(input)
        val p = Parser(l)
        val program = p.parseProgram()
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
        val l = Lexer(input)
        val p = Parser(l)
        val program = p.parseProgram()
        p.errors shouldBe Matchers.empty
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

  "ExpressionParser - Infix Operators - Grouped Expressions" should "Be correctly parsed" in {

    val prefixTests: Seq[(String, String)] = Seq(
      (
        "(5 + 5) * 2",
        "((5 + 5) * 2)"
      ),
      (
        "1 + (2 + 3) + 4",
        "((1 + (2 + 3)) + 4)"
      ),
      (
        "2 / (5 + 5)",
        "(2 / (5 + 5))"
      ),
      (
        "-(5 + 5)",
        "(-(5 + 5))"
      )
    )

    prefixTests.foreach {
      case (
          input: String,
          expected: String
          ) =>
        val l = Lexer(input)
        val p = Parser(l)
        val expression = p.parseProgram()
        println(s"Expression output: ${expression.string}")
        p.errors shouldBe Matchers.empty
        expression.string shouldEqual expected

      case _ => fail("Statement is not a prefix expression")
    }
  }
  "ExpressionParser - Infix Operators - testing really complex operators" should "Be correctly parsed" in {

    val prefixTests: Seq[(String, String)] = Seq(
      (
        "-a * b",
        "((-a) * b)"
      ),
      (
        "!-a",
        "(!(-a))"
      ),
      (
        "a + b + c",
        "((a + b) + c)"
      ),
      (
        "a + b - c",
        "((a + b) - c)"
      ),
      (
        "a * b * c",
        "((a * b) * c)"
      ),
      (
        "a * b / c",
        "((a * b) / c)"
      ),
      (
        "a + b / c",
        "(a + (b / c))"
      ),
      (
        "a + b * c + d / e - f",
        "(((a + (b * c)) + (d / e)) - f)"
      ),
      (
        "5 > 4 == 3 < 4",
        "((5 > 4) == (3 < 4))"
      ),
      (
        "5 < 4 != 3 > 4",
        "((5 < 4) != (3 > 4))"
      ),
      (
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
      ),
      (
        "3 > 5 == false",
        "((3 > 5) == false)"
      ),
      (
        "3 < 5 == true",
        "((3 < 5) == true)"
      ),
      (
        "3 < 5 == true;",
        "((3 < 5) == true)"
      ),
      ("true", "true"),
      ("false", "false"),
      ("foobar", "foobar")
    )

    prefixTests.foreach {
      case (
          input: String,
          expected: String
          ) =>
        val l = Lexer(input)
        val p = Parser(l)
        val expression = p.parseProgram()
        println(s"Expression output: ${expression.string}")
        expression.string shouldEqual expected
        p.errors shouldBe Matchers.empty

      case _ => fail("Statement is not a prefix expression")
    }
  }

  "If Expressions" should "be correctly parsed" in {
    val input = "if (x < y) { x }"
    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()
    p.errors shouldBe Matchers.empty
    program.string shouldEqual "if (x < y)  { x }"
  }

  "Baguga statements" should "be correctly parsed" in {
    val input = "{ x + 1; y + 1; } mas antes { x + 1; } BAGUGA"
    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()
    p.errors shouldBe Matchers.empty
    println(program.string)
    program.string shouldEqual "(x + 1);(y + 1), mas antes ☝\uFE0F(x + 1), mas antes ☝\uFE0F"
  }

  "IfElse Expressions" should "be correctly parsed" in {
    val input = "if (x < y) { let y = x + 1; return 5; } else { return y; }"
    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()
    p.errors shouldBe Matchers.empty
    program.string shouldEqual "if (x < y)  { let y = (x + 1);return 5; } else { return y; }"
  }

  "Function Expressions" should "be correctly parsed" in {
    val input = "fn(x, y) { x + y; x + 5; let y = x + 10; }"
    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()
    p.errors shouldBe Matchers.empty
  }

  "While expressions" should "work" in {
    "while (x < y) { let y = (x + 1);return 5; }" should beParsedCorrectly
  }

  "If expressions" should "work" in {
    "if (x < y)  { let y = (x + 1);return 5; } else { return y; }" should beParsedCorrectly
  }

  "Function Expressions" should "work, even if there are no arguments" in {
    val input = "fn() { x + y; x + 5; let y = x + 10; fn() { x } ; }"
    val l = Lexer(input)
    val p = Parser(l)
    val program = p.parseProgram()
    p.errors shouldBe Matchers.empty
  }

  "Call Expressions" should "be correctly parsed" in {
    "add(1, 5)" should beParsedCorrectly
    "add(1, (2 * 3), (4 + 5))" should beParsedCorrectly
    "add()" should beParsedCorrectly
    "add(fn () { (x + 5) } )" should beParsedCorrectly
    "((a + add((b * c))) + d)" should beParsedCorrectly
    "add((((a + b) + ((c * d) / f)) + g))" should beParsedCorrectly
  }

}
