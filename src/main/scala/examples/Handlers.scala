package examples

import effekt._

object Handlers extends App {

  trait Eff { type effect }
  trait Exc extends Eff { def raise(msg: String): Nothing / effect }
  trait Amb extends Eff { def flip(): Boolean / effect }

  def drunkFlip(exc: Exc, amb: Amb): String / (exc.effect & amb.effect) = for {
    caught <- amb.flip()
    heads  <- if (caught) amb.flip() else exc.raise("Too drunk")
  } yield if (heads) "Heads" else "Tails"

  def maybe[R, E](prog: (exc: Exc) => R / (exc.effect & E)): Option[R] / E =
    handle {
      val exc = new Exc with Handler() {
        def raise(msg: String) = use { resume => pure(None) }
      }
      prog(exc) map { x => Some(x) }
    }

  def collect[R, E](prog: (amb: Amb) => R / (amb.effect & E)): List[R] / E =
    handle {
      val amb = new Amb with Handler() {
        def flip() = use { resume => for {
          xs <- resume(true)
          ys <- resume(false)
        } yield xs ++ ys }
      }
      prog(amb) map { x => List(x) }
    }

  println { run {
    collect { amb =>
      maybe [String, amb.effect] { exc =>
        drunkFlip(exc, amb)
      }
    }
  }}
  //> List(Some(Heads), Some(Tails), None)

  println { run {
    maybe { exc =>
      collect [String, exc.effect] { amb =>
        drunkFlip(exc, amb)
      }
    }
  }}
  //> None

}

object EffectEP extends App {

  trait Eff { type effect }
  trait Exc extends Eff { def raise(msg: String): Nothing / effect }
  trait Amb extends Eff { def flip(): Boolean / effect }

  def drunkFlip(exc: Exc, amb: Amb): String / (exc.effect & amb.effect) = for {
    caught <- amb.flip()
    heads  <- if (caught) amb.flip() else exc.raise("Too drunk")
  } yield if (heads) "Heads" else "Tails"

  trait Maybe[R, E] extends Exc with Use[Option[R], E] {
    def raise(msg: String) = use { resume => pure(None) }
  }

  trait Collect[R, E] extends Amb with Use[List[R], E] {
    def flip() = use { resume => for {
      xs <- resume(true)
      ys <- resume(false)
    } yield xs ++ ys }
  }

  def maybe[R, E](prog: (exc: Exc) => R / (exc.effect & E)): Option[R] / E =
    handle {
      val exc = new Maybe[R, E] with Handler()
      prog(exc) map { x => Some(x) }
    }

  def collect[R, E](prog: (amb: Amb) => R / (amb.effect & E)): List[R] / E =
    handle {
      val amb = new Collect[R, E] with Handler()
      prog(amb) map { x => List(x) }
    }

  println { run {
    collect { amb =>
      maybe [String, amb.effect] { exc =>
        drunkFlip(exc, amb)
      }
    }
  }}
  //> List(Some(Heads), Some(Tails), None)

  println { run {
    maybe { exc =>
      collect [String, exc.effect] { amb =>
        drunkFlip(exc, amb)
      }
    }
  }}
  //> None


  trait Choose extends Eff { def choose[A](first: A, second: A): A / effect }
  trait AmbChoose extends Amb with Choose

  trait CollectChoose[R, E] extends AmbChoose with Collect[R, E] {
    def choose[A](first: A, second: A): A / effect = for {
      b <- flip()
    } yield if (b) first else second
  }

  trait Backtrack[R, E] extends Amb with Use[Option[R], E] {
    def flip() = use { resume => for {
      fst <- resume(true)
      res <- if (fst.isDefined) pure(fst) else resume(false)
    } yield res }
  }
  trait Both[R, E] extends Backtrack[R, E] with Maybe[R, E]
  def both[R, E](prog: (b: Amb & Exc) => R / (b.effect & E)): Option[R] / E =
    handle {
      val b = new Both[R, E] with Handler()
      prog(b) map { r => Some(r) }
    }

  println { run { both { b => drunkFlip(b, b) } } }
  //> Some(Heads)


  trait Parser extends Eff {
    def alternative[A, E](fst: A / E, snd: A / E): A / (effect & E)
    def accept(token: Char): Unit / effect
  }

  def AB(p: Parser): Int / p.effect = p.alternative(
    for { _ <- p.accept('a'); rest <- AB(p) } yield rest + 1,
    for { _ <- p.accept('b') } yield 0)

  trait Input extends Eff { def read(): Char / effect }
  def reader[R, E](s: String)(prog: (in: Input) => R / (in.effect & E)): R / E =
    // effect domain: Int => R / E
    handle [Int => R / E, E] {
      val in = new Input with Handler() {
        def read(): Char / effect = use { resume =>
          pure { pos => resume(s(pos)) flatMap { _ apply (pos + 1) } }
        }
      }
      prog(in) map { r => pos => pure(r) }
    } flatMap { _ apply 0 }

  trait ParserForward extends Parser {
    val exc: Exc; val amb: Amb; val in: Input

    type effect = exc.effect & amb.effect & in.effect

    def accept(expected: Char): Unit / effect = for {
      next <- in.read()
      res <- if (next == expected)
        pure(())
      else
        exc.raise("Expected " + expected + " but got " + next)
    } yield res

    def alternative[A, E](fst: A / E, snd: A / E) =
      amb.flip() flatMap { b => if (b) fst else snd }
  }

  def parse[R, E](lang: (p: Parser) => R / (p.effect & E))(s: String): Option[R] / E =
    both { b =>
      reader[R, b.effect & E](s) { i =>
        val parser = new ParserForward {
          val amb: b.type = b; val exc: b.type = b; val in: i.type = i
        }
        lang(parser)
      }
    }

  println{ run { parse(AB)("b") } }
  println{ run { parse(AB)("aab") } }
  println{ run { parse(AB)("xab") } }
}
