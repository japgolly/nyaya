package nyaya.util

import utest._

object UtilTest extends TestSuite {

  final case class Tree(name: String, children: List[Tree] = Nil)

  override def tests = Tests {

    "quickSB" - {
      "empty"       - assert(Util.quickSB(_ => ()) == "")
      "append"      - assert(Util.quickSB(sb => { sb.append("abc").append(123); () }) == "abc123")
      "withInitial" - assert(Util.quickSB("prefix: ", sb => { sb.append("suffix"); () }) == "prefix: suffix")
    }

    "escapeString" - {
      "empty"      - assert(Util.escapeString("") == "")
      "plain"      - assert(Util.escapeString("hello world 123!") == "hello world 123!")
      "standard"   - assert(Util.escapeString("a\n\r\t\\\"b") == "a\\n\\r\\t\\\\\\\"b")
      "control"    - {
        assert(Util.escapeString("\u0000") == "\\u0000")
        assert(Util.escapeString("\u0007") == "\\u0007")
        assert(Util.escapeString("\u001b") == "\\u001b")
        assert(Util.escapeString("\u001f") == "\\u001f")
      }
      "boundaries" - {
        assert(Util.escapeString(" ") == " ")
        assert(Util.escapeString("~") == "~")
        assert(Util.escapeString("\u007f") == "\u007f")
      }
      "unicode"    - {
        assert(Util.escapeString("\u0080") == "\\u0080")
        assert(Util.escapeString("\u00ff") == "\\u00ff")
        assert(Util.escapeString("→") == "\\u2192")
        assert(Util.escapeString("Scala: 日本語") == "Scala: \\u65e5\\u672c\\u8a9e")
      }
    }

    "asciiTree" - {
      def tree(root: Tree*): String =
        Util.asciiTree(root.toList)(_.children, _.name)

      "empty" - {
        assert(tree() == "")
      }

      "singleNode" - {
        assert(tree(Tree("root")) == "root")
      }

      "multipleRoots" - {
        val expected =
          """A
            |B
            |C""".stripMargin
        assert(tree(Tree("A"), Tree("B"), Tree("C")) == expected)
      }

      "simpleHierarchy" - {
        val t = Tree("root", List(
          Tree("child1"),
          Tree("child2"),
        ))
        val expected =
          """root
            |├─ child1
            |└─ child2""".stripMargin
        assert(tree(t) == expected)
      }

      "nestedHierarchy" - {
        val t = Tree("root", List(
          Tree("c1", List(
            Tree("gc1"),
            Tree("gc2"),
          )),
          Tree("c2", List(
            Tree("gc3"),
          )),
        ))
        val expected =
          """root
            |├─ c1
            |│  ├─ gc1
            |│  └─ gc2
            |└─ c2
            |   └─ gc3""".stripMargin
        assert(tree(t) == expected)
      }

      "multilineNode" - {
        val t = Tree("root", List(
          Tree("line1\nline2\nline3"),
          Tree("last"),
        ))
        val expected =
          """root
            |├─ line1
            |   line2
            |   line3
            |└─ last""".stripMargin
        assert(tree(t) == expected)
      }

      "nestedMultilineNode" - {
        val t = Tree("root", List(
          Tree("c1", List(
            Tree("deep1\ndeep2"),
          )),
          Tree("c2"),
        ))
        val expected =
          """root
            |├─ c1
            |│  └─ deep1
            |│     deep2
            |└─ c2""".stripMargin
        assert(tree(t) == expected)
      }

      "customIndent" - {
        val t = Tree("root", List(Tree("c1")))
        val actual = Util.asciiTree(List(t))(_.children, _.name, indent = ">> ")
        val expected =
          """>> root
            |>> └─ c1""".stripMargin
        assert(actual == expected)
      }

      "asciiTreeSB" - {
        val sb = new StringBuilder("prefix:\n")
        Util.asciiTreeSB(List(Tree("a", List(Tree("b")))))(sb, _.children, _.name)
        val expected =
          """prefix:
            |a
            |└─ b""".stripMargin
        assert(sb.toString == expected)
      }
    }
  }
}
