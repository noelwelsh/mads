package mads

/** A result type for a parser that cannot be resumed.
  */
enum Complete[A] {

  def isSuccess: Boolean =
    this match {
      case Success(_, _, _) => true
      case _                => false
    }

  def isFailure: Boolean =
    !isSuccess

  /** Parsed nothing up to one character before the given offset and failed */
  case Epsilon(input: String, offset: Int)

  /** Parsed up to and including one character before the given offset and failed */
  case Committed(input: String, offset: Int)

  /** Successfully parsed input up to and including one character before the given to offset as
    * result
    */
  case Success(result: A, input: String, offset: Int)
}
