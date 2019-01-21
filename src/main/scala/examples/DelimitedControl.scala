package examples

import effekt._

object DelimitedControl extends App {

  // Example 1
  // ---------
  val ex: Int / Pure = reset [Int, Pure] { p =>
    shift0[Int](p) { k => k(100) flatMap k } map { 10 + _ }
  } map { 1 + _ }

  println { run { ex } }
  //> 121


  // Example 2
  // ---------
  val ex2: Int / Pure = reset [Int, Pure] { p1 =>
    reset [Boolean, p1.type] { p2 =>
      shift0(p1) { k => pure(21) }
    } map { if(_) 1 else 2 }
  } map { 2 * _ }

  println { run { ex2 } }
  //> 42


  // Example 3
  // ---------
  trait Eff { type effect }
  trait Exc extends Eff { def raise(msg: String): Nothing / effect }
  trait Amb extends Eff { def flip(): Boolean / effect }

  def drunkFlip(exc: Exc, amb: Amb): String / (exc.effect & amb.effect) = for {
    caught <- amb.flip()
    heads  <- if (caught) amb.flip() else exc.raise("Too drunk")
  } yield if (heads) "Heads" else "Tails"

  def maybe[R, E](prog: (exc: Exc) => R / (exc.effect & E)): Option[R] / E =
    reset { p =>
      val exc = new Exc {
        type effect = p.type // type refinement
        def raise(msg: String) = shift0(p) { resume => pure(None) }
      }
      prog(exc) map { x => Some(x) }
    }

  def collect[R, E](prog: (amb: Amb) => R / (amb.effect & E)): List[R] / E =
    reset { p =>
      val amb = new Amb {
        type effect = p.type // type refinement
        def flip() = shift0(p) { resume => for {
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
