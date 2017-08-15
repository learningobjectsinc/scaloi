package scaloi

import org.scalatest.{Matchers, WordSpec}
import java.{lang => jl}

class ClassMapTest extends WordSpec with Matchers {

  "ClassMap" should {

    "require coherent bounds" in {
      ClassMap.empty[Number, java.lang.Long]
      ClassMap.empty[AnyRef, Null]
      ClassMap.empty[Int, Int]

      """ClassMap.empty[String, Symbol]""" shouldNot compile
      """ClassMap.empty[AnyRef, Boolean]""" shouldNot compile
      """ClassMap.empty[Int, Null]""" shouldNot compile

    }

    "work as expected" in {
      val numbers =
        ClassMap.empty0[Number]
          .+(classOf[jl.Long]    -> Long.box(4L))
          .+(classOf[jl.Integer] -> Int.box(12))
          .+(classOf[BigInt]     -> BigInt(67))

      numbers.get(classOf[jl.Long]) should contain (Long.box(4L))
      numbers.get(classOf[jl.Integer]) should contain (Int.box(12))
      numbers.get(classOf[BigInt]) should contain (BigInt(67))

      numbers.get(classOf[jl.Short]) should be (None)

      val objects =
        ClassMap.empty[AnyRef, Null]
          .+(classOf[String] -> "foo")
          .+(classOf[Symbol] -> 'bar)
          .-(classOf[String])

      objects.get(classOf[String]) should be (None)
      objects.get(classOf[Symbol]) should contain ('bar)

    }

    "allow free access to the keys and values" in {
      val numbers =
        ClassMap.empty0[Number]
          .+(classOf[jl.Long]    -> Long.box(4L))
          .+(classOf[jl.Integer] -> Int.box(12))
          .+(classOf[BigInt]     -> BigInt(67))

      numbers.keys should contain theSameElementsAs (classOf[jl.Long] :: classOf[jl.Integer] :: classOf[BigInt] :: Nil)
      numbers.values should contain theSameElementsAs (Long.box(4L) :: Int.box(12) :: BigInt(67) :: Nil)
    }

  }

}
