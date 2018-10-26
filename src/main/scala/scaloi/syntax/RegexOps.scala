package scaloi
package syntax

import scala.util.matching.Regex

import scala.language.implicitConversions

/** Enhancements on regex. */
final class RegexOps(val self: Regex) extends AnyVal {
  /** Test whether this regex entirely matches `s`. */
  def matches(s: String): Boolean   = self.pattern.matcher(s).matches

  /** Test whether this regex partially matches `s`. */
  def test(s: String): Boolean      = self.pattern.matcher(s).find

  /** Test whether this regex matches the start of `s`. */
  def lookingAt(s: String): Boolean = self.pattern.matcher(s).lookingAt
}

/**
  * Regex operations companion.
  */
object RegexOps extends ToRegexOps

/**
  * Implicit conversion for Regex operations.
  */
trait ToRegexOps extends Any {

  /**
    * Implicit conversion from [[Regex]] to its enhancements.
    *
    * @param r the regex
    */
  implicit def toRegexOps(r: Regex): RegexOps = new RegexOps(r)
}
