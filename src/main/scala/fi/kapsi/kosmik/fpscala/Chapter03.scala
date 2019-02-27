package fi.kapsi.kosmik.fpscala

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

}
