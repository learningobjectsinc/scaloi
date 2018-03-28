package scaloi.test

import org.scalatest.FlatSpecLike

/** A base trait for scaloi tests. */
trait ScaloiTest { self: FlatSpecLike =>

  final val behaviour: self.behavior.type = self.behavior

}
