package scaloi
package test

import org.scalactic.source
import org.scalatest.FlatSpec

trait YtteSpec extends ScaloiTest { this: FlatSpec =>
  final val yt = it

  implicit final class Ytte(yt: ItWord) {
    def shouldde(whut: String) = new ItVerbString("shouldde", whut)
  }

  implicit final class Ynne(vs: ItVerbString) {
    def yn(testFun: => Any)(implicit pos: source.Position): Unit = vs in testFun
  }
}
