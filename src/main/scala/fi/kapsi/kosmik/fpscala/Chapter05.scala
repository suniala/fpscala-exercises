package fi.kapsi.kosmik.fpscala

object Chapter05 {

  sealed trait Stream[+A]

  case object Empty extends Stream[Nothing] {
    override def toString: String = "Empty"
  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    override def toString: String = "Cons(" + h() + ", " + t() + ")"
  }

  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def toList[A](s: Stream[A], num: Int): List[A] = {
    def rec(rem: Stream[A], count: Int): List[A] =
      if (count == 0) Nil
      else rem match {
        case Empty => Nil
        case Cons(h, t) => h() :: rec(t(), count - 1)
      }

    rec(s, num)
  }

  def toList[A](s: Stream[A]): List[A] = toList(s, Int.MaxValue)

  object Ex04 {
    def forAll[A](s: Stream[A])(p: A => Boolean): Boolean = {
      s match {
        case Empty => true
        case Cons(h, t) => if (!p(h())) false else forAll(t())(p)
      }
    }
  }

  object Ex08 {
    def constant[A](a: A): Stream[A] = cons(a, constant(a))
  }

  object Ex09 {
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  }

  object Ex10 {
    def fibs: Stream[Int] = {
      def rec(n2: Int, n1: Int): Stream[Int] = cons(n2, rec(n1, n2 + n1))

      rec(0, 1)
    }
  }

  object Ex11 {
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => Empty
        case Some((value, nextState)) => cons(value, unfold(nextState)(f))
      }
  }

  object Ex12 {

    import Ex11.unfold

    def ones: Stream[Int] = unfold(None)(_ => Some(1, None))

    def constant[A](a: A): Stream[A] = unfold(None)(_ => Some(a, None))

    def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

    def fibs: Stream[Int] = unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))
  }

  object Ex13 {

    import Ex11.unfold

    def map[A, B](as: Stream[A])(f: A => B): Stream[B] = unfold(as) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

    def take[A](as: Stream[A], n: Int): Stream[A] = unfold((as, n))(s => {
      if (s._2 == 0) None
      else s._1 match {
        case Empty => None
        case Cons(h, t) => Some(h(), (t(), s._2 - 1))
      }
    })

    def takeWhile[A](as: Stream[A])(p: A => Boolean): Stream[A] =
      unfold(() => as)(stateThunk => {
        stateThunk() match {
          case Empty => None
          case Cons(h, t) =>
            if (p(h())) Some((h(), () => takeWhile(t())(p)))
            else None
        }
      })

    def zipWith[A, B](as: Stream[A], b: B): Stream[(A, B)] = unfold(as) {
      case Empty => None
      case Cons(h, t) => Some(((h(), b), t()))
    }

    def zipAll[A, B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((as, bs))(s =>
        s._1 match {
          case Empty => s._2 match {
            case Empty => None
            case Cons(bh, bt) => Some((None, Some(bh())), (Empty, bt()))
          }
          case Cons(ah, at) => s._2 match {
            case Empty => Some((Some(ah()), None), (at(), Empty))
            case Cons(bh, bt) => Some((Some(ah()), Some(bh())), (at(), bt()))
          }
        }
      )
  }

  object Ex14 {

    import Ex04._
    import Ex13._

    def startsWith[A](stream: Stream[A])(prefix: Stream[A]): Boolean = {
      val zippedUntilPrefixEnds =
        takeWhile(zipAll(stream, prefix))({ case (so, po) => (so.isDefined && po.isDefined) || (so.isEmpty && po.isDefined) })

      forAll(zippedUntilPrefixEnds)(
        { case (so, po) =>
          so match {
            case None => false
            case Some(s) => po match {
              case None => true
              case Some(p) => s == p
            }
          }
        })
    }
  }

}