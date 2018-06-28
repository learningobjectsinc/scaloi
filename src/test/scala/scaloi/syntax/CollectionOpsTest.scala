package scaloi.syntax

import org.scalatest.{FlatSpec, OptionValues}
import scaloi.test.ScaloiTest
import scalaz.syntax.either._
import java.{io => jio}

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
    def verify(x: Any): Unit = noException shouldBe thrownBy { oos.writeObject(x) }

    val list = List(1,2,3) : Seq[Int]
    verify(list.makeSerializable)
    (list.makeSerializable eq list) should be (true)

    val ws = "" : Seq[Char]
    ws should not be a [Serializable]
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
}
