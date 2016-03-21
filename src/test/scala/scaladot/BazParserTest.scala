package scaladot

import java.util.NoSuchElementException

import collection.mutable.Stack
import org.scalatest._

import scala.util.parsing.combinator._
import scala.util.{Failure, Success, Try}


/**
  * Created by gio on 3/21/16.
  */
class BazParserTest extends FreeSpec with Matchers {

  "it should say" - {
    "hello" in {
      println("hello")
    }
  }

  "it should throw" - {
    "NoSuchElementException if an empty stack is popped" in {
      val emptyStack = new Stack[Int]
      a [NoSuchElementException] should be thrownBy {
        emptyStack.pop()
      }
    }
  }

  object BazTest extends BazParser {

    def parse[T](p: Parser[T], success: Boolean, input: String) = {
      var x = phrase(p)(new lexical.Scanner(input)) match {
        case Success(result, _) => println("Success!"); println(result); assertResult(success)(true); Some(result)
        case n @ _ => println(n);  assertResult(success)(false); None
      }
    }

  }

  def go[T](p: BazTest.Parser[T], success: Boolean, input: String) = {
    BazTest.parse(p, success,input.stripMargin)
  }


  //---< ID >--------------------------------------------------------------------------

  "ID" - {
    "should parse" - {
      var p = BazTest.ID
      "as valid " - {
        val rc = true
        "a simple string literal" in {
          go(p, rc,
            """
              |"AAA"
            """)
        }
        "a simple identifier" in {
          go(p, rc,
            """
              |AAA
            """)
        }
      }
      "as illegal " - {
        val rc = false
        "a integrer literal " in {
          go(p, rc,
            """
              |123
            """)
        }
      }
    }
  }


  //---< id >--------------------------------------------------------------------------

  "id" - {
    "should parse" - {
      var p = BazTest.id
      "as valid " - {
        val rc = true
        "a simple string literal" in {
          go(p, rc,
            """
              |( "AAA" )
            """)
        }
        "a simple identifier" in {
          go(p, rc,
            """
              |(AAA)
            """)
        }
      }
      "as illegal " - {
        val rc = false
        "a integrer literal " in {
          go(p, rc,
            """
              |( 123 )
            """)
        }
      }
    }
  }


  //---< bar >--------------------------------------------------------------------------

  "bar" - {
    "should parse" - {
      var p = BazTest.bar
      "as valid " - {
        val rc = true
        "a simple string literal ctor" in {
          go(p, rc,
            """
              |bar ( "AAA" )
            """)
        }
        "a simple identifier  ctor" in {
          go(p, rc,
            """
              |bar(AAA)
            """)
        }
      }
      "as illegal " - {
        val rc = false
        "a integrer literal ctor" in {
          go(p, rc,
            """
              |bar( 123 )
            """)
        }
        "a no args ctor" in {
          go(p, rc,
            """
              |bar
            """)
        }
      }
    }
  }

  //---< foo >--------------------------------------------------------------------------

  "foo" - {
    "should parse" - {
      var p = BazTest.foo
      "as valid " - {
        val rc = true
        "a simple string literal ctor" in {
          go(p, rc,
            """
              |foo ( "AAA" ) -> {}
            """)
        }
        "a simple identifier  ctor" in {
          go(p, rc,
            """
              |foo(AAA) -> {}
            """)
        }
        "a single bar ctor" in {
          go(p, rc,
            """
              |foo(AAA) -> { bar(x1) }
            """)
        }
        "a list of bars ctor" in {
          go(p, rc,
            """
              |foo(AAA) -> { bar(x1),bar(x2) , bar(x3) }
            """)
        }
      }
      "as illegal " - {
        val rc = false
        "a integer literal ctor" in {
          go(p, rc,
            """
              |foo( 123 ) -> {}
            """)
        }
        "a no args ctor" in {
          go(p, rc,
            """
              |foo -> {}
            """)
        }
        "a no bars ctor" in {
          go(p, rc,
            """
              |foo( AAA )
            """)
        }
        "a foos args ctor" in {
          go(p, rc,
            """
              |foo(AAA) -> { foo(BBB) }
            """)
        }
        "a list of bars ctor" in {
          go(p, rc,
            """
              |foo(AAA) -> { bar(x1),bar(x2) , bar(x3), }
            """)
        }
      }
    }
  }


  //---< baz >--------------------------------------------------------------------------

  "baz" - {
    "should parse" - {
      var p = BazTest.baz
      "as valid " - {
        val rc = true
        "a simple string literal ctor" in {
          go(p, rc,
            """
              |baz ( "NNN" ) => []
            """)
        }
        "a simple identifier  ctor" in {
          go(p, rc,
            """
              |baz(NNN) => []
            """)
        }
        "a single bar ctor" in {
          go(p, rc,
            """
              |baz(NNN) => [ foo(aaa) -> {} ]
            """)
        }
        "a list of foos ctor" in {
          go(p, rc,
            """
              |baz(NNN) => [ foo(a1) -> {},foo(a2) -> {} , foo(a3) -> {} ]
            """)
        }
        "a nested list of foos ctor" in {
          go(p, rc,
            """
              |baz(NNN) => [ foo(a1) -> {},foo(a2) -> { bar(x1), bar(x2), bar(x3) } , foo(a3) -> {} ]
            """)
        }
      }
      "as illegal " - {
        val rc = false
        "a integer literal ctor" in {
          go(p, rc,
            """
              |foo( 123 ) => []
            """)
        }
        "a no args ctor" in {
          go(p, rc,
            """
              |baz => []
            """)
        }
        "a no bars ctor" in {
          go(p, rc,
            """
              |baz( NNN )
            """)
        }
        "a bars args ctor" in {
          go(p, rc,
            """
              |baz(NNN) => [ bar(x1) ]
            """)
        }
        "an incomplete of foos ctor" in {
          go(p, rc,
            """
              |baz(NNN) => [ foo(a1) -> {},foo(a2) -> {} , foo(a3) -> {}, ]
            """)
        }
        "an incomplete nested list of foos ctor" in {
          go(p, rc,
            """
              |baz(NNN) => [ foo(a1) -> {},foo(a2) -> { bar(x1), bar(x2), bar(x3), } , foo(a3) -> {} ]
            """)
        }
      }
    }
  }


}
