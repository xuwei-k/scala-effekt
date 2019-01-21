package effekt

private[effekt]
sealed trait MetaCont[-A, +B] extends Serializable {
  def apply(a: A): Result[B]

  def append[C](s: MetaCont[B, C]): MetaCont[A, C]

  def splitAt(p: Marker): (MetaCont[A, p.Result], MetaCont[p.Result, B])

  def map[C](f: C => A): MetaCont[C, B] = flatMap(x => pure(f(x)))

  def flatMap[C](f: Frame[C, A]): MetaCont[C, B] = FramesCont(List(f), this)
}

private[effekt]
case class ReturnCont[A]() extends MetaCont[A, A] {
  final def apply(a: A): Result[A] = Value(a)
  final def append[B](s: MetaCont[A, B]): MetaCont[A, B] = s
  final def splitAt(p: Marker) = sys error s"Prompt $p not found on the stack."
}

private[effekt]
case class FramesCont[-A, B, +C](frames: List[Frame[Nothing, Any]], tail: MetaCont[B, C]) extends MetaCont[A, C] {

  final def apply(a: A): Result[C] = {
    val first :: rest = frames
    val result = first.asInstanceOf[Frame[A, B]](a)
    if (rest.isEmpty) {
      Computation(result, tail)
    } else {
      Computation(result, FramesCont(rest, tail))
    }
  }

  final def append[D](s: MetaCont[C, D]): MetaCont[A, D] = FramesCont(frames, tail append s)

  final def splitAt(p: Marker) = tail.splitAt(p) match {
    case (head, tail) => (FramesCont(frames, head), tail)
  }

  override def flatMap[D](f: Frame[D, A]): MetaCont[D, C] = FramesCont(f :: frames, tail)
}

private[effekt]
case class PromptCont[R, +A](p: Marker { type Result = R })(tail: MetaCont[R, A]) extends MetaCont[R, A] {
  final def apply(r: p.Result): Result[A] = tail(r)

  final def append[C](s: MetaCont[A, C]): MetaCont[R, C] = PromptCont(p)(tail append s)

  // Here we can see that our semantics is closer to spawn/controller than delimCC
  final def splitAt(other: Marker) = other match {
    // Here we deduce type equality p.Result =:= other.Result from referential equality
    case _: p.type =>
      val head = PromptCont(p)(ReturnCont[p.Result]()).asInstanceOf[MetaCont[p.Result, other.Result]]
      val rest = tail.asInstanceOf[MetaCont[other.Result, A]]
      (head, rest)

    case _ => tail.splitAt(other) match {
      case (head, tail) =>
        (PromptCont(p)(head), tail)
    }
  }
}
