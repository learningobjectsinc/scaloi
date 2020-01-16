package scaloi.test

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

/** A base trait for scaloi tests. */
trait ScaloiTest extends Matchers { self: AnyFlatSpecLike =>

  final val behaviour: self.behavior.type = self.behavior

}
