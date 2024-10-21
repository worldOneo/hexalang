```
test :: fn() return 1

Option :: generic[T] enum {
  Some: tuple { T }
  None: tuple {}
}

Map :: generic[A, B] struct {
  iter: Iterator[A, B]
  t: fn(val: A): B

  next :: fn(this) {
    match this.iter.next()
      Option.Some{ v } this.t(v),
      Option.None{} Option.None{}
  }
}

Iterator :: generic[A] interface {
  next: fn(this): Option[A]

  skip :: fn(this, num: int): Iterator[A] {
    i := 0
    for i < num {
      this.next()
      i += 1
    }
    return this
  }

  map :: generic[B] fn(this, t: fn(A) B): Iterator[B] Map.{iter: this, t: t}
}

Range :: struct {
  start: int
  stop: int

  next :: fn(this) Option[int] {
    if this.start < this.stop {
      this.start += 1
      return Option[int].Some.{this.start - 1}
    }
    return Option[int].None.{}
  }

  new :: fn(start: int, stop: int) Range {
    return Range.{
      start: start
      stop: stop
    }
  }
}

genericAdd :: generic[A] fn(a: A, b: A): A return a + b

thisIsAStruct :: generic[A] struct {
  a: A
  b: A

  add :: fn() {
    return genericAdd[A](this.a, this.b)
  }
}

thisIsATuple :: tuple { int, int }

thisIsAEnum :: enum {
  VariantA: thisIsAStruct[int]
  VariantB: struct {
    c: int
    d: int
  }
  VariantC: tuple { int, float }
}

main :: fn() {
  thisIsAInstance := thisIsAStruct.{ a: 1 b: 2 }
  sum := thisIsAStruct.add()
  variant := thisIsAEnum.VariantC.{ 1, 3.14 }

  num := match variant
            thisIsAEnum.VariantA { a, b } 1,
            thisIsAEnum.VariantB { c: e, d: _ } 2,
            thisIsAEnum.VariantC { x, y } {
              yield x + y // yield is return but for blocks
            }

  arrau := Range.new(0, 100)
            .[Iterator[int]]
            .map(fn(x) x * 2)
}
```
