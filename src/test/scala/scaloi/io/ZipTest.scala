package scaloi.io

import java.io.{File, InputStream}

import org.scalatest.FlatSpec
import scaloi.NatTrans._
import scaloi.io.zip.{Name, Zip, _}

import scalaz.Id.Id
import scalaz.stream.tee
import scalaz.{Id, ~>}

/**
 * Created by zpowers on 10/3/15.
 */
class ZipTest extends FlatSpec {

  case object NopZip extends Zip
  case class NopEntry(name: String) extends Entry

  val emptyStream = new InputStream {
    override def read(): Int = -1
  }


  def testZip(files: Seq[String]) = new (ZipOperation ~> Id) {
    val filerator = files.toIterator
    override def apply[A](fa: ZipOperation[A]): Id.Id[A] = fa match {
      case Open => NopZip
      case Next(_) => if(filerator.hasNext) Some(NopEntry(filerator.next)) else None
      case Read(_) => emptyStream
      case Extract(_,_) => File.createTempFile("dummy","bin")
      case Name(NopEntry(name)) => name
      case Close(_) => ()
      case _ => ???
    }
  }

  val readThreeEntries = withZip(zip =>
    for {
      entry1  <- next(zip)
      entry2 <- next(zip)
      entry3 <- next(zip)
      //Unsafe
      name1 <- name(entry1.get)
      name2 <- name(entry2.get)
      name3 <- name(entry3.get)
    } yield (name1,name2,name3)
  )

  "A ZipIO monad " should "perform no effect" in {
    val files = readThreeEntries foldMap printOp(testZip(Seq("dummy.bin","aDir/aFile.txt","bfile.bin")))
    assert(files == (("dummy.bin","aDir/aFile.txt","bfile.bin")))
    println(files)
  }

  it should "read files form a Zip" in {
    val testZip = getClass.getClassLoader.getResource("simpleZip.zip")
    println(testZip)
    val testZipFile = new File(testZip.toURI)

    val files = (readThreeEntries foldMap printOp(withJIO(testZipFile))).unsafePerformSync
    assert(files == (("aFile","aFolder/","aFolder/aNestedFile")))
    println(files)
  }

  it should "iterate as a process" in {
    import scalaz.stream.Process

    val entriesIO = stream.runLog

    val entries = entriesIO foldMap printOp(testZip(Seq("dummy.bin","aDir/aFile.txt","bfile.bin")))
    println(entries)
  }
}
