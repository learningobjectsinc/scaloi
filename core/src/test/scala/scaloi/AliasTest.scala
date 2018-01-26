package scaloi

import org.scalatest.{FlatSpec, Matchers}

class AliasTest extends FlatSpec with Matchers {
  "alias" should "alias" in {
    val FirstName = alias.Opaque[String]
    type FirstName = FirstName.T

    val LastName = alias.Opaque[String]
    type LastName = LastName.T

    val fn: FirstName = FirstName inj "Bob"
    val ln: LastName = LastName inj "Dobbs"

    "(fn : String)" shouldNot compile

    "(ln : FirstName)" shouldNot compile

    def fullName(fn: FirstName, ln: LastName): String = s"$fn $ln"

    def userName(fn: FirstName, ln: LastName): String = s"${(FirstName prj fn take 1).toLowerCase}${(LastName prj ln).toLowerCase}"

    fullName(fn, ln) should equal ("Bob Dobbs")
    userName(fn, ln) should equal ("bdobbs")

    val fns: List[FirstName] = FirstName subst List("King", "Lord", "", "Ragged", "Jack")
    val lns: List[LastName] = LastName subst List("Mob", "Fanny", "Boy", "Robin", "Frost")

    ((fns zip lns) map (fullName _).tupled) should equal (List("King Mob", "Lord Fanny", " Boy", "Ragged Robin", "Jack Frost"))
    ((fns zip lns) map (userName _).tupled) should equal (List("kmob", "lfanny", "boy", "rrobin", "jfrost"))
  }
}
