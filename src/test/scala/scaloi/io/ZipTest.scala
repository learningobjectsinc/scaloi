package scaloi.io

import java.io.{File, InputStream}

import org.scalatest.FlatSpec
import scaloi.io.zip._

import scalaz.Id.Id
import scalaz.{Id, ~>}

/**
  * Created by zpowers on 10/3/15.
  */
class ZipTest extends FlatSpec {

  case object NopZip
  case class NopEntry(name: String)

  object UnitArchive extends Archive[NopZip.type, NopEntry] {
    def testZip(files: Seq[String]) = new (ArchiveOp ~> Id) {
      val filerator = files.toIterator
      override def apply[A](fa: ArchiveOp[A]): Id.Id[A] = fa match {
        case Open                 => NopZip
        case Next(_)              => if (filerator.hasNext) Some(NopEntry(filerator.next)) else None
        case Read(_)              => emptyStream
        case Extract(_, _)        => File.createTempFile("dummy", "bin")
        case Name(NopEntry(name)) => name
        case Close(_)             => ()
        case _                    => ???
      }
    }
  }

  val emptyStream = new InputStream {
    override def read(): Int = -1
  }

  def readThreeEntries[F, E](archive: Archive[F, E]) = {
    import archive._

    withFile(
        zip =>
          for {
        entry1 <- next(zip)
        entry2 <- next(zip)
        entry3 <- next(zip)
        //Unsafe
        name1 <- name(entry1.get)
        name2 <- name(entry2.get)
        name3 <- name(entry3.get)
      } yield (name1, name2, name3))
  }

  "A ZipIO monad " should "perform no effect" in {
    import UnitArchive._
    val files = readThreeEntries(UnitArchive) foldMap testZip(Seq("dummy.bin", "aDir/aFile.txt", "bfile.bin"))
    assert(files == (("dummy.bin", "aDir/aFile.txt", "bfile.bin")))
  }

  it should "read files form a Zip" in {
    import JIOZip._
    val testZip     = this.getClass.getClassLoader.getResource("simpleZip.zip")
    val testZipFile = new File(testZip.toURI)

    val files = (readThreeEntries(JIOZip) foldMap withJIO(testZipFile)).unsafePerformSync
    assert(files == (("aFile", "aFolder/", "aFolder/aNestedFile")))
  }

  it should "iterate as a process" in {
    import UnitArchive._

    val entriesIO = stream.runLog

    entriesIO foldMap testZip(Seq("dummy.bin", "aDir/aFile.txt", "bfile.bin"))
  }
}
