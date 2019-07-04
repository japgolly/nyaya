package nyaya.util

object ScalaVerSpecificUtil {

  object Implicits {

    implicit final class ScalaVerSpecificArrayAnyRefOps[A <: AnyRef](private val self: Array[A]) extends AnyVal {
      def sortInPlace()(implicit o: Ordering[A]): Array[A] = {
        java.util.Arrays.sort(self, o)
        self
      }

      def sortInPlaceBy[B](f: A => B)(implicit o: Ordering[B]): Array[A] =
        sortInPlace()(Ordering.by(f)(o))
    }

  }
}