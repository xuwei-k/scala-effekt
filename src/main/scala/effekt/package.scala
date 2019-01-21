package object effekt {

  // Type Aliases
  // ===

  // we use intersection types to track effects, so Pure = Top
  type Pure = Any

  // A type DSL to construct implicit, dependent function types
  // Currently effectively blocked by https://github.com/lampepfl/dotty/issues/5288
  type /[+A, -E] = Control[A, E]

  type CPS[A, R] = (A => R) => R


  // Effect Safe Delimited Control
  // ===
  trait Eff { type effect }

  // Just a marker trait used by the delimcc implementation
  // We store types Result and Effects in abstract type members for
  // better type inference and better interaction with partial
  // type annotations (e.g. on shift0)
  sealed trait Marker private[effekt]() { type Result; type Effects }
  type Prompt[R, E] = Marker { type Result = R; type Effects = E }

  object Prompt {
    private[effekt] def fresh[R, E]: Prompt[R, E] =
      new Marker { type Result = R; type Effects = E }
  }

  final def run[A](c: A / Pure): A = Result.trampoline(c(ReturnCont()))

  final def pure[A](a: => A): A / Pure = Control.pure(a)

  // to allow supplying only type parameter `A`, here we use existentials on R and E instead
  // of the universally quantified signature:
  // final def shift0[A, R, E](p: Prompt[R, E])(body: CPS[A, R / E]): A / p.type
  final def shift0[A](p: Marker)(body: CPS[A, p.Result / p.Effects]): A / p.type = Control.shift0(p)(body)

  final def reset[R, E](prog: (p: Prompt[R, E]) => R / (p.type & E)): R / E = Control.reset(prog)


  // Effect Handlers
  // ===
  def handle[R, E](prog: implicit (p: Prompt[R, E]) => R / (p.type & E)): R / E = reset(p => prog(p))

  trait Use[R, E] extends Eff {
    def use[A](body: CPS[A, R / E]): A / effect
  }

  // the indirection with type parameter P is necessary to improve
  // type inference for singleton types.
  trait Handler[R, E, P <: Prompt[R, E] & Singleton](implicit val prompt: P) extends Use[R, E] {
    type effect = prompt.type
    def use[A](body: CPS[A, R / E]): A / effect = shift0(prompt)(body)
  }


  // Implementation Details
  // ===
  // internally we ignore the effect types
  private[effekt] type Frame[-A, +R] = A => Control[R, _]

  // Trick described in https://github.com/lampepfl/dotty/issues/3920#issuecomment-360772033 to
  // help infer singleton types. Also works on trait parameters.
  type The[A] = A & Singleton

}
