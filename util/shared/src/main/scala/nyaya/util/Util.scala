package nyaya.util

import scala.annotation.tailrec
import scala.collection.Iterable

object Util {

  @inline def quickSB(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }

  @inline def quickSB(start: String, f: StringBuilder => Unit): String = {
    val sb = new StringBuilder(start)
    f(sb)
    sb.toString
  }

  def asciiTree[N](root: Iterable[N])(leaves: N => Iterable[N], show: N => String, indent: String = ""): String =
    quickSB(asciiTreeSB(root)(_, leaves, show, indent))

  def asciiTreeSB[N](root: Iterable[N])(sb: StringBuilder, leaves: N => Iterable[N], show: N => String, indent: String = ""): Unit = {
    val pm = "│  "
    val pl = "   "
    val cm = "├─ "
    val cl = "└─ "
    var first = true
    @inline def loop2 = loop(_, _, _)
    @tailrec
    def loop(parentLvlLast: Vector[Boolean], fs: List[N], root: Boolean): Unit = fs match {
      case Nil =>
      case h :: t =>
        def indentPrefix(): Unit = {
          sb append indent
          for (b <- parentLvlLast) sb.append(if (b) pl else pm)
        }

        if (first) first = false else sb append '\n'
        var indentlen = sb.length
        indentPrefix()
        val last = t.isEmpty
        if (!root) sb.append(if (last) cl else cm)
        indentlen = sb.length - indentlen

        var firstLine = true
        for (l <- show(h).split("\n")) {
          if (firstLine) firstLine = false else {
            sb append '\n'
            indentPrefix()
            sb append "   "
          }
          sb append l
        }

        val nextLvl = if (root) Vector.empty[Boolean] else parentLvlLast :+ last
        loop2(nextLvl, leaves(h).toList, false)
        loop(parentLvlLast, t, root)
    }
    loop(Vector.empty, root.toList, true)
  }
}
