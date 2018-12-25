package scaloi.syntax

import java.{lang => jl, util => ju}

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.Functor

class CollectionBoxOpsTest extends FlatSpec with OptionValues with Matchers {
  import boxes._

  behavior of "CollectionBoxOps"

  it should "box collectionly" in {
    val coll = List[Int](1, 2, 3)
    coll.boxInside() should have size 3
    coll.boxInside().head shouldBe a[jl.Integer]

    coll.boxInsideTo[ju.List]() shouldBe a[ju.List[_]]
    coll.boxInsideTo[ju.List]() should have size 3

    "coll.boxInside()" shouldNot compile
    "coll.boxInsideTo[scala.Function0]()" shouldNot compile
  }

  it should "unbox collectionly" in {
    val coll = List[jl.Long](1L, 2L, 3L)
    coll.unboxInside() should have size 3
    coll.unboxInside().head.getClass should be(classOf[Long])

    coll.unboxInsideTo[Vector]() shouldBe a[Vector[_]]
    coll.unboxInsideTo[Vector]() should have size 3

    "coll.unboxInside()" shouldNot compile
    "coll.unboxInsideTo[scala.reflect.ClassTag]()" shouldNot compile
  }

  it should "box functorly" in {

    val opt = Option[Byte](11.toByte)
    opt.boxInside() should be('defined)
    opt.boxInside().get shouldBe a[jl.Byte]

    "opt.unboxInside()" shouldNot compile
  }

  it should "unbox functorly" in {

    val opt = Option[jl.Short](jl.Short.valueOf(22.toShort))
    opt.unboxInside() should be('defined)
    opt.unboxInside().get.getClass should be(classOf[Short])

    "opt.boxInside()" shouldNot compile
  }

  it should "go between Option(al)?s" in {

    val sopt = Option[Short](12.toShort)
    sopt.boxInsideTo[ju.Optional]() should equal(ju.Optional.of(Short box 12))

    val jopt = ju.Optional.of[jl.Long](142L)
    jopt.unboxInsideTo[Option]() should equal(Some(142L))
  }

  it should "not do stupid things" in {
    val aimless: Function0[Long] = () => 8192.toLong
    "aimless.boxInside()" shouldNot compile

    val shameless: Function0[jl.Long] = () => 4096.toLong
    "shameless.unboxInside()" shouldNot compile

    val pointless: Functor[Option] = scalaz.Scalaz.optionInstance
    "pointless.boxInside()" shouldNot compile
    "pointless.unboxInside()" shouldNot compile
  }

}
