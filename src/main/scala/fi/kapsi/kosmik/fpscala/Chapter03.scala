package fi.kapsi.kosmik.fpscala

import scala.annotation.tailrec

object Chapter03 {

  object Ex13 {
    /**
      * foldLeft in terms of foldRight
      */
    def foldLeftFR[A, B](xs: List[A], z: B)(f: (B, A) => B): B =
      xs.reverse.foldRight(z)((a, b) => f(b, a))

    /**
      * foldRight in terms of foldLeft
      */
    def foldRightFL[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
      xs.reverse.foldLeft(z)((b, a) => f(a, b))
  }

  object Ex14 {
    def appendFL[A](xs: List[A], item: A): List[A] =
      xs.reverse.foldLeft(item :: Nil)((b, a) => a :: b)
  }

  object Ex15 {
    /**
      * Concatenate list of lists into a single list. Runtime should be linear in the total length of all lists.
      */
    def flatten[A](xss: List[List[A]]): List[A] =
      xss.foldRight(List[A]())((xs, acc) => xs.foldRight(acc)((x, acc) => x :: acc))
  }

  object Ex24 {
    /**
      * A pure but not necessarily an efficient implementation.
      */
    def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
      @tailrec
      def hasPrefix[B](list: List[B], pref: List[B]): Boolean = {
        pref match {
          case Nil => true
          case p :: ps =>
            list match {
              case Nil => false
              case x :: xs => if (p == x) hasPrefix(xs, ps) else false
            }
        }
      }

      sup match {
        case Nil => false
        case x :: xs => if (x == sub.head) {
          val m = hasPrefix(xs, sub.tail)
          if (m) m else hasSubSequence(xs, sub)
        } else {
          hasSubSequence(xs, sub)
        }
      }
    }
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Ex25 {
    def size[A](tree: Tree[A]): Int =
      tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
      }
  }

  object Ex26 {
    def maximum(tree: Tree[Int]): Int =
      tree match {
        case Leaf(v) => v
        case Branch(l, r) => Math.max(maximum(l), maximum(r))
      }
  }

  object Ex27 {
    def depth[A](tree: Tree[A]): Int =
      tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + Math.max(depth(l), depth(r))
      }
  }

  object Ex28 {
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      tree match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }
  }

  object Ex29 {
    def fold[A, B](tree: Tree[A])(fl: A => B, fb: (B, B) => B): B = {
      tree match {
        case Leaf(v) => fl(v)
        case Branch(l, r) => fb(fold(l)(fl, fb), fold(r)(fl, fb))
      }
    }

    def size[A](tree: Tree[A]): Int = fold[A, Int](tree)(_ => 1, (l, r) => 1 + l + r)

    def maximum(tree: Tree[Int]): Int = fold[Int, Int](tree)(v => v, (l, r) => Math.max(l, r))

    def depth[A](tree: Tree[A]): Int = fold[A, Int](tree)(_ => 1, (l, r) => 1 + Math.max(l, r))

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree)(v => Leaf(f(v)), (l, r) => Branch(l, r))
  }

}
