package nyaya.gen

import scala.collection.compat._

object ScalaVerSpecific {

  trait SetLikeImplicits {
  }

  trait GenClassExt[+A] extends Any { self: Gen[A] =>
    // final def to[F[_]](implicit ss: SizeSpec, f: Factory[A, F[A]]): Gen[F[A]] =
    //   fillSS(ss)(f)
  }
}
