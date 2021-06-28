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

  /** Success nothing up to one character before the given offset and failed */
  case Epsilon(input: String, offset: Int)

  /** Success up to one character before the given offset and failed */
  case Committed(input: String, offset: Int)

  /** Successfully parsed input up one character before the given to offset as
    * result
    */
  case Success(result: A, input: String, offset: Int)
}
