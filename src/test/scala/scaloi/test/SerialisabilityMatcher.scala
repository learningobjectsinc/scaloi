package scaloi
package test

import org.scalatest._
import org.scalatest.matchers.{BeMatcher, MatchResult}

trait SerialisabilityMatcher {

  object serialisable extends BeMatcher[AnyRef] {
    def apply(o: AnyRef): MatchResult = {
      import java.io._

      import words.MatcherWords._
      try {
        val baos = new ByteArrayOutputStream()
        new ObjectOutputStream(baos).writeObject(o)
        val bais = new ByteArrayInputStream(baos.toByteArray)
        equal(new ObjectInputStream(bais).readObject())
          .matcher[AnyRef]
          .apply(o)
      } catch {
        case problem: NotSerializableException =>
          MatchResult(false,
            s"should have been serializable, but ${problem.getMessage} was not",
            "should not have been serializable", // why?
          )
      }
    }
  }

}
