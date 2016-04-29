package scaloi
package io

import java.io.{BufferedInputStream, InputStream, File}

import scala.language.higherKinds
import scalaz._
import scalaz.Free.liftF
import scalaz.concurrent.Task
import scalaz.stream._

object zip extends FreeHacks {
  import java.util.zip.{ZipEntry, ZipFile}

  /**
   * A Free monad on JarOperations
   */
  type ZipIO[A] = Free[ZipOperation,A]

  /**
   * A sum type representing operations that can performed on a jar.
   */
  sealed trait ZipOperation[A]
  case object Open extends ZipOperation[Zip]
  case class  Next(jar: Zip) extends ZipOperation[Option[Entry]]
  case class  Read(entry: Entry) extends ZipOperation[InputStream]
  case class  Extract(entry: Entry, destination: File) extends ZipOperation[File]
  case class  Name(entry: Entry) extends ZipOperation[String]
  case class  Close(jarFile: Zip) extends ZipOperation[Unit]

  /**
   * A handle to a open ZipFile with a ready stateful iterator over files in the archive.
   */
  trait Zip

  /** A handle to a file inside a zip archive.
   */
  trait Entry

  //IO constructors
  val open: ZipIO[Zip] = liftF(Open)
  def next(jar: Zip) : ZipIO[Option[Entry]] = liftF(Next(jar))
  def read(entry: Entry): ZipIO[InputStream] = liftF(Read(entry))
  def extract(entry: Entry, destination: File): ZipIO[File] = liftF(Extract(entry,destination))
  def name(entry: Entry) = liftF(Name(entry))
  def close(jarFile: Zip) = liftF(Close(jarFile))

  //IO utility functions
  def withZip[T](f : Zip => ZipIO[T]): ZipIO[T] = for {
    zip <- open
    op <- f(zip)
    _ <- close(zip)
  } yield op

  //close a open Zip File
  def close[A](zio: ZipIO[A])(file: Zip): ZipIO[A] = for {
    z <- zio
    _ <- close(file)
  } yield z

  //Operation utility functions
  def isOpen[T](jarOp: ZipOperation[T]): Boolean = jarOp match {
    case Close(_) => false
    case _ => true
  }

  /**
   * A Process of Zip entries.
   */
  val stream = Process.eval[ZipIO,Zip](open)
      .evalMap[ZipIO,Option[Entry]](next)
      .repeat
      .takeWhile(_.isDefined)
      .map(_.get)

  /** Stateful Zip and Entry Values for working with zip files via java.util.zip.
   */
  case class JavaZip(private[zip] val zip: ZipFile, private[zip] val entries: Iterator[ZipEntry]) extends Zip
  case class JavaEntry(private[zip] val zip: JavaZip, private[zip] val entry: ZipEntry) extends Entry

  /* Returns a Task which executes the given zip program using java.util.zip.
   */
  def withJIO(file: File): ZipOperation ~> Task = new (ZipOperation ~> Task) {
    import Task.delay
    import collection.JavaConverters._

    override def apply[A](given: ZipOperation[A]): Task[A] = given match {
      case Open => delay {
        val jar = new ZipFile(file)
        val enum = jar.entries().asScala
        JavaZip(jar,enum)
      }
      case Next(zip @ JavaZip(_,entries)) => delay(if(entries.hasNext) Some(JavaEntry(zip,entries.next())) else None)
      case Read(JavaEntry(JavaZip(zip,_),entry)) => delay(zip.getInputStream(entry))
      case Extract(entry @ JavaEntry(zip,e),destination) => extractFile(destination,4096)(entry) map { _ => destination }
      case Name(JavaEntry(_,entry)) => delay(entry.getName())
      case Close(JavaZip(jar,_)) => delay(jar.close())
      case op => sys.error(s"$op is not a JIO based operation")
    }
  }

  def extractFile(destination: File, bufferSize: Int)(javaEntry: JavaEntry) = {
    val zip = javaEntry.zip.zip
    val entry = javaEntry.entry
    val dest = new File(destination, entry.getName)
    dest.getParentFile.mkdirs()
    io.chunkR(new BufferedInputStream(zip.getInputStream(entry), bufferSize))
      .map(read => read(bufferSize).unsafePerformSync)
      .to(io.fileChunkW(dest.getAbsolutePath, bufferSize)).run
  }

  implicit val zipCatchable = new Catchable[ZipIO] {
    //TODO Better implement error handling.
    override def attempt[A](f: ZipIO[A]): ZipIO[Throwable \/ A] = f map { x => \/-(x) }

    override def fail[A](err: Throwable): ZipIO[A] = liftF(throw err)
  }
}
