package lexer

import org.scalatest.flatspec.AnyFlatSpec
import token.{Token, TokenType}

class LexerSpec extends AnyFlatSpec {
  "nextToken - simple case" should "pass without problems and match all its tokens" in {
    val input = "=+(){},;"
    val expected: Seq[Token] = Seq(
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.LBRACE, "{"),
      Token(TokenType.RBRACE, "}"),
      Token(TokenType.COMMA, ","),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.EOF, "")
    )

    LexerTestInstance.test(input, expected)

  }

  "while" should "be parsed" in {
    val input = "while (true) { 1 + 1 }"
    val (_, lexer: Lexer) = Lexer(input).token
    println(lexer.getTokens.mkString(","))
  }

  "arrays" should "be parsed" in {
    val input = "[1, 10, a, \"hm\"]"
    val (_, lexer: Lexer) = Lexer(input).token
    println(lexer.getTokens.mkString(","))
  }

  "strings" should "be parsed" in {
    val input = "\"hey\"; \"hey\\\"aaa\naaá upen☝\uFE0F\";"
    val (_, lexer: Lexer) = Lexer(input).token
    println(lexer.getTokens.mkString(","))
  }

  "readIdentifier" should "" in {
    val input = "input a a a a a"
    val (_, lexer: Lexer) = Lexer(input).token
    assert(lexer.token._1.contains(Token(TokenType.IDENT, "input")))
  }

  "nextToken - more complex case" should "pass without problems and match all its tokens" in {
    val input =
      """
      let five = 5;
      let ten = 10;

      let add = fn(x, y) {
        x + y;
      };

      let result = add(five, ten);
      !-/*5;
      5 < 10 > 5;

      if (5 < 10) {
        return true;
      } else {
        return false;
      }

      10 == 10;
      10 != 9;""".stripMargin

    val expected: Seq[Token] = Seq(
      Token(TokenType.LET, "let"),
      Token(TokenType.IDENT, "five"),
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.INT, "5"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.LET, "let"),
      Token(TokenType.IDENT, "ten"),
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.INT, "10"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.LET, "let"),
      Token(TokenType.IDENT, "add"),
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.FUNCTION, "fn"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.IDENT, "x"),
      Token(TokenType.COMMA, ","),
      Token(TokenType.IDENT, "y"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.LBRACE, "{"),
      Token(TokenType.IDENT, "x"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.IDENT, "y"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.RBRACE, "}"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.LET, "let"),
      Token(TokenType.IDENT, "result"),
      Token(TokenType.ASSIGN, "="),
      Token(TokenType.IDENT, "add"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.IDENT, "five"),
      Token(TokenType.COMMA, ","),
      Token(TokenType.IDENT, "ten"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.BANG, "!"),
      Token(TokenType.MINUS, "-"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.ASTERISK, "*"),
      Token(TokenType.INT, "5"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.INT, "5"),
      Token(TokenType.LT, "<"),
      Token(TokenType.INT, "10"),
      Token(TokenType.GT, ">"),
      Token(TokenType.INT, "5"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.IF, "if"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.INT, "5"),
      Token(TokenType.LT, "<"),
      Token(TokenType.INT, "10"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.LBRACE, "{"),
      Token(TokenType.RETURN, "return"),
      Token(TokenType.TRUE, "true"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.RBRACE, "}"),
      Token(TokenType.ELSE, "else"),
      Token(TokenType.LBRACE, "{"),
      Token(TokenType.RETURN, "return"),
      Token(TokenType.FALSE, "false"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.RBRACE, "}"),
      Token(TokenType.INT, "10"),
      Token(TokenType.EQ, "=="),
      Token(TokenType.INT, "10"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.INT, "10"),
      Token(TokenType.NOT_EQ, "!="),
      Token(TokenType.INT, "9"),
      Token(TokenType.SEMICOLON, ";"),
      Token(TokenType.EOF, "")
    )

    LexerTestInstance.test(
      "==",
      Seq(Token(TokenType.EQ, "=="), Token(TokenType.EOF, ""))
    )
    LexerTestInstance.test(input, expected)

  }

}
