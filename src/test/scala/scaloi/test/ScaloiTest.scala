package scaloi.test

import org.scalatest.{FlatSpecLike, Matchers}

/** A base trait for scaloi tests. */
trait ScaloiTest extends Matchers { self: FlatSpecLike =>

  final val behaviour: self.behavior.type = self.behavior

}
