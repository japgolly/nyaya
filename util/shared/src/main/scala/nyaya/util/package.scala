package nyaya

package object util {

  @inline implicit final class NyayaUtilAnyExt[A](private val a: A) extends AnyVal {
    @inline def `JVM|JS`(js: => A): A = Platform.choose(a, js)
  }
}
