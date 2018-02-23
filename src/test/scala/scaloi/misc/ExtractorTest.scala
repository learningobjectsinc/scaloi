package scaloi.misc

import org.scalatest.{FlatSpec, Matchers}

class ExtractorTest extends FlatSpec with Matchers {

  behavior of "Extractor"

  it should "match and drop prefixen" in {
    val DropBear = Extractor.dropPrefix("bear")

    "bear hug"     should    matchPattern { case DropBear(" hug")     => }
    "bearly alive" should    matchPattern { case DropBear("ly alive") => }
    "dog tired"    shouldNot matchPattern { case DropBear(_)          => }
  }

  it should "match and drop suffixen" in {
    val DropBear = Extractor.dropSuffix("bear")

    "arms bear"       should    matchPattern { case DropBear("arms ")    => }
    "grizzly bear"    should    matchPattern { case DropBear("grizzly ") => }
    "sea bear circle" shouldNot matchPattern { case DropBear(_)          => }
  }

}
