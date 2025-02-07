package parser

import com.typesafe.scalalogging.Logger

trait ParserDebugger {
  parser: Parser =>
  val logger: Logger = Logger(parser.getClass)

  def debugTokens(): Unit = {
    logger.debug(s"Current [$cToken] - Peek [$pToken]")
  }
}
