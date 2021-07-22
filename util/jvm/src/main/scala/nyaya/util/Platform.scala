package nyaya.util

import scala.annotation.nowarn

object Platform {

  @nowarn("cat=unused")
  @inline final def choose[A](jvm: => A, js: => A): A = jvm
}