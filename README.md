# Scaloi

Fyne Thyngges provided by Learning Objects, for Use with ScalaZ and Libraries of a similar Nature and Quality.

## Monad conversion syntax

Convenient syntax for converting to and among monads.

### Disjunction

Syntaces, including the flappy birds, to operate within `\/` disjunctions.

```scala
for {
  _ <- 0.right[Err] // ScalaZ
  _ <- Option(1)  \/>  Err.IsNone // ScalaZ
  _ <- true       \/>  Err.IsFalse
  _ <- false      \/>! Err.IsTrue
  _ <- Try(2)     \/>  Err.forThrowable
  _ <- Try(3)     \/>| Err.IsFailure
  _ <- \@~*/(op) -\/>  Err.forThrowable
} yield ???
```

### Try

Syntaces to operate within the `Try` monad.

```scala
for {
  _ <- 0.success
  _ <- Ex("Bah").failure
  _ <- Option(1) <@~* Ex("None")
  _ <- true      <@~* Ex("false")
  _ <- false     *~@> Ex("true")
  _ <- true      <@~* Ex("false") *> 3 // add a starry bird
  _ <- 2.right[Ex].toTry
  _ <- this.asInstanceOf_![That]
  _ <- true either 3      orFailure         Ex("false")
  _ <- Option(Try(4))     flatToTry         Ex("None")
  _ <- Option(5)          thenFailure      (Ex("None"), 6)
  _ <- Option("Some")     thenHollowFailure Ex.apply
  _ <- Option("Some")     elseHollowVictory Ex.apply
  _ <- Option(Ex("Some")) toFailure         8
} yield ???
```

### Task

Syntaces to operate within the `Task` monad.

```scala
for {
  i <- 0.now
  j <- Try(0).toTask
  k <- Option(0) *#@% Ex("None")
} yield ???
```

### Validation

Syntaces to operate within the `Validation` monad.

```scala
for {
  _ <- 0.success[Err] // ScalaZ
  _ <- Option(1) elseInvalid Err.IsNone
  _ <- Option("bah") thenInvalid Err.forString
  _ <- Option("bah") thenInvalid 2
  _ <- true elseFailure (Err.IsFalse, 3)
  _ <- false thenFailure (Error.IsTrue, 4)
} yield ???
```

Similarly for `ValidationNel`.

## Hypermonad

The `Hypermonad` typeclass describes a monad that supports mapping flatter
than flat. For example, given a list of ids and a
function from an id to perhaps a list of entities, it can flatter map to
a single list of all the associated entities.

```scala
val entityMap: Map[Long, List[Entity]] = ???
val ids: List[Long] = ???
val entities: List[Entity] = ids.flatterMap(entityMap.get)
```

Hypermonads can also flatten flatter.

```scala
val list3: List[List[List[Int]]] = ???
val ints: List[Int] = list3.hyperFlatten
```

## List tree

A strict tree, much as `StrictTree`, but based on `List` and thus avoidant
of the moral perils of `Vector`.

```scala
val tree = 0.listNode(1.listLeaf, 2.listLeaf)
tree.flatten must_=== List(0, 1, 2)
```

## Zero

Evidence for a type that has a zero but is not necessarily semigroupal.

```scala
trait Zero[A] {
  def zero: A
  def isZero(a: A): Boolean
}
```

All `Monoid`s with `Equal`, `IsEmpty`s and `Numeric`s have a `Zero`.

### Syntax

A minimal syntax is provided. Other zeroic syntaces are provided in
different contexts.

```scala
Nil.isZero must_=== true
1.nonZero must_=== true
```

## New Zealand Option

`OptionNZ` wraps a value in an `Option`, filtering out any zeroes.
Use this, for example, to filter out empty strings.

```scala
OptionNZ("") must_=== None
OptionNZ("Bob") must_=== Some("Bob")
```

## New Brunswick Option

Related, `OptionNB` wraps a `String` in an `Option`, filtering out any
blanks.

```scala
OptionNB(" ") must_=== None
OptionNZ("Old Brunswick") must_=== Some("Old Brunswick")
```

## South Carolina Fold

The `foldSC` operation, aka ``??>``, short-circuits a function to the resulting
zero if the parameter to the function is an empty collection. Use this, for
example, to avoid performing unnecessary I/O operations or calling database
libraries that fail on empty lists.

```scala
val ids: List[Long] = ???
val loadUsers: List[Long] => List[User] = ???
val users: List[User] = ids ??> loadUsers
```

## Bucket generational data structures

Used to efficiently track recent data.

### Bucket generational dedup

Tracks which data have been recently added, for the purposes of
deduplication.

```scala
val dedup = BucketGenerationalDedup.empty[Symbol](3.minutes, 3)
dedup += 'a
dedup contains 'a must_=== true
sleep(4.minutes)
dedup contains 'a must_=== false
```

### Bucket generational bag

Tracks how many of each datum have been recently added to the bag.

```scala
val bag = BucketGenerationalBag.empty[Symbol](3.minutes, 3)
bag += 'a
bag count 'a must_=== 1
sleep(1.minutes)
bag += 'a
bag count 'a must_=== 2
sleep(4.minutes)
bag count 'a must_=== 0
```

## Unbounded blocking fair keyed queue

A queue of key-value pairs that releases values fairly across the keyspace.

```scala
val queue = UnboundedBlockingFairKeyedQueue.empty[Symbol, Int]
queue.offer('a -> 1)
queue.offer('a -> 2)
queue.offer('b -> 3)
queue.take() must_=== 1
queue.take() must_=== 3
queue.take() must_=== 2
```

## Set delta

Tracks the delta between two sets.

```scala
val set0: Set[Int] = ???
val set1: Set[Int] = ???
val delta = SetDelta from set0 to set1
set0 |: delta must_=== set1
```

## Many fine option enhancements

With added support for `util.Optional`. Including, but not limited
to:

```scala
Option(a) <|? println     // tap on the contents
Option(a) -<| sys.exit()  // tap on absence
Option(a) flatOpt fa      // flat map to nullable
Option(a) orZ             // get or zero
Option(a) foldZ fa        // map or zero
Option(a) filterNZ        // filter non-zero
Option(a) mapNZ fa        // map then filter non-zero
Option(a0) - a1           // filter out
Option(a).isTrue          // container of truth
Option(a0) =&= Option(a1) // both present and equal
Option(a0) max a1         // maximality
Option(a0) max Option(a1) // co-maximality
Option(a0) min a1         // minimality
Option(a0) min Option(a1) // co-minimality
Option(a) when b          // filter by boolean
Option(a) unless b        // filter by invert boolean
Option(a).accept[Dog]     // filter by type
Option(a) \&/ Option(b)   // to these
```

## Many fine boolean enhancements

With added support for `lang.Boolean`. Including, but not limited to:

```scala
bool noption a               // some if false, else none
bool flatOption optA         // none if false, or else optA
bool flatNoption optA        // none if true, or else optA
false ??? Some(1) must_=== 0 // zero if false or absent, else a
true  <|? println("true")    // tap on true
false <|! println("false")   // tap on false
bool either a orElse optA    // flat either or
```

## Either neither both

The `\|/` bifunctor admits either the left, the right, both or neither.
Contrast with `\&/` which refuses to admit neither and `\/` which
admits neither neither nor both.

```scala
val optA: Option[A] = ???
val optB: Option[B] = ???
val ab = optA \|/ optB
val cd = enb bimap (fac, fbd)
```

## Font

Consider the [blessed](https://github.com/merlinorg/FiraLorde) font.
