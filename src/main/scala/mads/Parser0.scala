package mads

import scala.util.matching.Regex

/** Parsers constructed through these methods consume zero or more characters */
object Parser0 {
  import Parser.{
    CharactersWhile,
    CharactersUntilRegexOrEnd,
    CharactersUntilTerminator,
    CharactersUntilTerminatorOrEnd
  }

  /** Parses zero or more character while predicate succeeds.
    */
  def charsWhile(predicate: Char => Boolean): Parser[String] =
    Parser.CharactersWhile(predicate, true, false)

  /** Parse zero or more characters until the predicate is true. */
  def charsUntil(predicate: Char => Boolean): Parser[String] =
    charsWhile(ch => !predicate(ch))

  /** Parse zero or more characters until the first match of the regular
    * expression or the end of the input
    */
  def charsUntilRegexOrEnd(regex: Regex): Parser[String] =
    CharactersUntilRegexOrEnd(regex, true, false)

  /** Parse until the first example of one of the terminators */
  def charsUntilTerminator(terminators: String*): Parser[String] =
    CharactersUntilTerminator(terminators, true, false)

  /** Parse until the first example of one of the terminators or the end of the
    * input
    */
  def charsUntilTerminatorOrEnd(terminators: String*): Parser[String] =
    CharactersUntilTerminatorOrEnd(terminators, true, false)
}
