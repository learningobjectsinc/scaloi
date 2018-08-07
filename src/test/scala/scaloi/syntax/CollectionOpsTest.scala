package scaloi.syntax

import java.{io => jio}

import org.scalatest.{FlatSpec, OptionValues}
import scalaz.syntax.either._
import scaloi.test.ScaloiTest

class CollectionOpsTest
  extends FlatSpec
     with OptionValues
     with ScaloiTest
{

  behaviour of "CollectionOps"
  import CollectionOps._

  it should "criss crossingly" in {
    Seq(1, 2) × Seq('a', 'b') should contain allOf ((1,'a'), (2,'a'), (1,'b'), (2,'b'))
  }

  it should "ensure serializability" in {
    val oos = new jio.ObjectOutputStream(new jio.ByteArrayOutputStream())

    def verify(x: Any): Unit = noException shouldBe thrownBy {
      oos.writeObject(x)
    }

    val list = List(1, 2, 3): Seq[Int]
    verify(list.makeSerializable)
    (list.makeSerializable eq list) should be(true)

    val ws = "": Seq[Char]
    ws should not be a[Serializable]
    verify(ws.makeSerializable)
  }

  it should "partition and collect" in {
    List(1, 2, 3, 4).partitionCollect {
      case i if i % 2 == 0 => i.toString.left
      case i if i % 3 == 0 => i.right
    } should equal {
      (List("2", "4"), List(3))
    }
  }

  it should "group by key and map values" in {
    case class Foo(key: String, value: Int) { def tuple = (key, value) }
    val foos = Seq(
      Foo("First", 1),
      Foo("First", 2),
      Foo("Second", 3)
    )
    val groupedFoos = foos.groupMap(_.key)(_.value)
    val groupedFoos1: Map[String, Seq[Int]] = groupedFoos // test inference
    groupedFoos("First").size shouldBe 2
    groupedFoos("First") should contain allOf (1, 2)
    groupedFoos("Second").size shouldBe 1
    groupedFoos("Second") should contain(3)

    val groupedToFoos = foos.map(_.tuple).groupToMap
    val groupedToFoos1: Map[String, Seq[Int]] = groupedToFoos // test inference
    groupedToFoos("First").size shouldBe 2
    groupedToFoos("First") should contain allOf (1, 2)
    groupedToFoos("Second").size shouldBe 1
    groupedToFoos("Second") should contain(3)
  }

  it should "group, map, fold, blend, and purée" in {
    import scalaz.std.string._
    val data = List(1 -> "1", 2 -> "2", 5 -> "bar", 5 -> "foo")
    data.groupMapFold(_._1 + 1)(_._2) should equal (Map(
      2 -> "1", 3 -> "2", 6 -> "barfoo",
    ))
  }

  it should "findMap values that are findable" in {
    import scalaz.syntax.std.boolean._

    List(1, 2, 3).findMap(i => (i == 2).option(i)).value shouldEqual 2
  }

  it should "findMap nothing where nothing is found" in {
    List(1, 2, 3).findMap(i => None) shouldEqual None
  }

  it should "findMap the first match" in {
    import scalaz.syntax.std.boolean._

    var count = 0
    List(1, 2, 2, 3).findMap(i => { count = count + 1; (i == 2).option(i) }).value shouldEqual 2
    count shouldEqual 2
  }

  it should "findMap in infinity" in {
    Stream.continually(1).findMap(i => Some(i)) shouldEqual Some(1)
  }

  it should "group uniq by" in {
    val data = List(1 -> "1", 1 -> "2", 2 -> "3")
    data.groupUniqBy(_._1) shouldBe data.groupBy(_._1).mapValues(_.head)
  }
}
