package examples

import effekt._

// All examples in this file are commented out, since they
// don't type check. And rightfully so: they are not effect safe.
object EffectSafety {

  // var prompt: Prompt[Unit, Pure] = null
  // val problem1 = run {
  //   for {
  //     _ <- reset [Unit, Pure] { p => prompt = p; pure(()) }
  //     _ <- shift0[Unit](prompt) { resume => pure(()) }
  //   } yield ()
  // }

  // val problem2 = run {
  //   for {
  //     p <- reset [Marker, Pure] { p => pure(p) }
  //     _ <- shift0(p) { resume => ??? }
  //   } yield ()
  // }

  // val problem3 = run {
  //   reset [Unit, Pure] { p =>
  //     shift0[Unit](p) { resume =>
  //       shift0[Unit](p) { resume => pure(()) }
  //     }
  //   }
  // }

  // val problem4 = run {
  //   reset [Unit, Pure] { p1 =>
  //     reset [Unit, p1.type] { p2 =>
  //       shift0[Unit](p1) { resume =>
  //         shift0[Unit](p2) { resume => pure(()) }
  //       }
  //     }
  //   }
  // }


  object subtyping {
    val p1, p2, p3: Prompt[Unit, Pure] = ???
    val prog1: Control[Int, p1.type & p2.type] = ???
    val prog2: Control[Int, p1.type & p2.type & p3.type] = prog1
  }
}
