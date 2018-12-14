/*
 * Copyright 2007 Cengage Learning, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scaloi.free.io

import java.io.{BufferedInputStream, InputStream, File => JFile}

import scalaz.Free.liftF
import scalaz._
import scalaz.concurrent.Task
import scalaz.stream._

trait Archive[File, Entry] {

  /**
    * A Free monad on JarOperations
    */
  type ArchiveIO[A] = Free[ArchiveOp, A]

  /**
    * A sum type representing operations that can performed on a archive.
    */
  sealed trait ArchiveOp[A]
  case object Open extends ArchiveOp[File]
  case class Next(jar: File)                           extends ArchiveOp[Option[Entry]]
  case class Read(entry: Entry)                        extends ArchiveOp[InputStream]
  case class Extract(entry: Entry, destination: JFile) extends ArchiveOp[JFile]
  case class Name(entry: Entry)                        extends ArchiveOp[String]
  case class Close(jarFile: File)                      extends ArchiveOp[Unit]

  //IO constructors
  val open: ArchiveIO[File] = liftF(Open)
  def next(jar: File): ArchiveIO[Option[Entry]]                   = liftF(Next(jar))
  def read(entry: Entry): ArchiveIO[InputStream]                  = liftF(Read(entry))
  def extract(entry: Entry, destination: JFile): ArchiveIO[JFile] = liftF(Extract(entry, destination))
  def name(entry: Entry)                                          = liftF(Name(entry))
  def close(jarFile: File)                                        = liftF(Close(jarFile))

  //IO utility functions
  def withFile[T](f: File => ArchiveIO[T]): ArchiveIO[T] =
    for {
      zip <- open
      op  <- f(zip)
      _   <- close(zip)
    } yield op

  //close a open File File
  def close[A](zio: ArchiveIO[A])(file: File): ArchiveIO[A] =
    for {
      z <- zio
      _ <- close(file)
    } yield z

  //Operation utility functions
  def isOpen[T](jarOp: ArchiveOp[T]): Boolean = jarOp match {
    case Close(_) => false
    case _        => true
  }

  /**
    * A Process of Zip entries.
    */
  val stream = Process
    .eval[ArchiveIO, File](open)
    .evalMap[ArchiveIO, Option[Entry]](next)
    .repeat
    .takeWhile(_.isDefined)
    .map(_.get)

  //TODO Better implement error handling.
  implicit val archiveCatchable = new Catchable[ArchiveIO] {
    override def attempt[A](f: ArchiveIO[A]): ArchiveIO[Throwable \/ A] =
      f map { x =>
        try \/-(x)
        catch { case th: Throwable => -\/(th) }
      }
    override def fail[A](err: Throwable): ArchiveIO[A] = throw err // todo: wut
  }
}

object zip {
  import java.util.zip.{ZipEntry, ZipFile}

  /** Stateful Zip and Entry Values for working with zip files via java.util.zip.
    */
  case class JavaZip(private[zip] val zip: ZipFile, private[zip] val entries: Iterator[ZipEntry])
  case class JavaEntry(private[zip] val zip: JavaZip, private[zip] val entry: ZipEntry)

  object JIOZip extends Archive[JavaZip, JavaEntry] {
    /* Returns a Task which executes the given zip program using java.util.zip.
     */
    def withJIO(file: JFile): ArchiveOp ~> Task = new (ArchiveOp ~> Task) {
      import Task.delay
      import collection.JavaConverters._

      override def apply[A](given: ArchiveOp[A]): Task[A] = given match {
        case Open =>
          delay {
            val jar  = new ZipFile(file)
            val enum = jar.entries().asScala
            JavaZip(jar, enum)
          }
        case Next(zip @ JavaZip(_, entries)) =>
          delay(if (entries.hasNext) Some(JavaEntry(zip, entries.next())) else None)
        case Read(JavaEntry(JavaZip(zip, _), entry)) => delay(zip.getInputStream(entry))
        case Extract(entry @ JavaEntry(_, _), destination) =>
          extractFile(destination, 4096)(entry) map { _ =>
            destination
          }
        case Name(JavaEntry(_, entry)) => delay(entry.getName())
        case Close(JavaZip(jar, _))    => delay(jar.close())
      }
    }
  }

  def extractFile(destination: JFile, bufferSize: Int)(javaEntry: JavaEntry) = {
    val zip   = javaEntry.zip.zip
    val entry = javaEntry.entry
    val dest  = new JFile(destination, entry.getName)
    dest.getParentFile.mkdirs()
    io.chunkR(new BufferedInputStream(zip.getInputStream(entry), bufferSize))
      .map(read => read(bufferSize).unsafePerformSync)
      .to(io.fileChunkW(dest.getAbsolutePath, bufferSize))
      .run
  }
}
