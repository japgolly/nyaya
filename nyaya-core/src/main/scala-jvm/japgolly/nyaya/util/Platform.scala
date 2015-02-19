package japgolly.nyaya.util

object Platform {

  @inline final def choose[A](jvm: => A, js: => A): A = jvm
}