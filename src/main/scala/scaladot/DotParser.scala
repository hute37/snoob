package scaladot;
 
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
 
/**
 * A parser for the GraphViz "dot" language. This code is in the public domain.
  
 *  @author Ross Judson
 */
class DotParser extends StdTokenParsers with ImplicitConversions {
  // Fill in abstract defs
  type Tokens = DotLexer
  val lexical = new Tokens
 
  // Configure lexical parsing
  lexical.reserved ++= List("strict", "graph", "digraph", "node", "edge", "subgraph")
  lexical.delimiters ++= List("{", "}", "[", "]", ":", "=", ";", ",", "->", "--","\"")
 
  import Dot._
  import lexical._
   
  /** We want to map an Option of None to the empty string identifier,
  and Some(s) to s.
  */
//  implicit def emptyIdentifier(n: Option[String]) = n match {
//    case Some(id) => id
//    case _ => ""
//  }

    def emptyIdentifier(n: Option[String]) = n match {
      case Some(id) => id
      case _ => ""
    }

  /** It seems that when we need to get the implicit versions of sequences
  of results out into a list of those results, the automatic conversion
  can be performed by this implicit function. Note that this function
  uses existential typing; we don't care about the type of the second part
  of the ~'s type, so we ignore it. */
  //implicit def convertList[B](lst: List[~[B,_]]) = lst.map(_._1)
   
  //graph   :   ostrict gtype optid  stmt_list
  def dot = ostrict ~ gtype ~ optid ~ stmt_list ^^
    { case str ~ typ ~ id ~ statements => Graph(str, typ, id, statements:_*) }
     
  //gtype   :   graph | digraph
  def gtype: Parser[Boolean] = ("graph" ^^^ false | "digraph" ^^^ true)

  //ostrict   :   [ kstrict ]
  def ostrict: Parser[Boolean] = opt(kstrict) ^^ {
    case Some(_) => true
    case None => false
  }

  //kstrict   :   [ strict ]
  def kstrict = "strict" ^^^ "strict"

  //optid   :   [ ID ]
  def optid: Parser[String] = opt(ID) ^^ {
    case Some(id) => id
    case _ => ""
  }

  //stmt_list   :   '{' [ stmt [ ';' ] [ stmt_list ] ] '}'
  def stmt_list: Parser[Seq[Statement]]  = ("{" ~> repsep(stmt,";") <~ "}") ^^ { case stmts : List[_] => stmts }
   
  // stmt   :   node_stmt
  //    |   edge_stmt
  //    |   attr_stmt
  //    |   ID '=' ID
  //    |   subgraph
  def stmt :Parser[Statement] = subgraph | attr_set | edge_stmt | attr_stmt | node_stmt


  def attr_set = ID ~ ( "=" ~> a_value ) ^^
    { case left ~ right => right match { case Pair(q,v) => Attr(left, Some(v), q) }}
   
  //attr_stmt          :          (graph | node | edge) attr_list
  def  attr_stmt = attr_list_type ~ attr_list ^^
    { case at ~ al => AttrList(at, al:_*) }
   
  def attr_list_type =
    "graph" ^^^ "graph" |
    "node" ^^^ "node" |
    "edge" ^^^ "edge"
     
  //attr_list   :   '[' [ a_list ] ']' [ attr_list ]
  def  attr_list : Parser[List[Attr]]  = (("[" ~> a_list <~ "]")*) ^^
    { case lists => lists.flatMap(l => l) }


  case class P(z: Int*)

  def  pq = ("[" ~> ps <~ "]") ^^ {P(_:_*)}
  def  ps = repsep(px, ",")
  def  px = "0" ^^^ 0



  case class Q(z: Int*)

  def  qq: Parser[Q] = (qk1 ~> qs <~ qk2) ^^ {Q(_:_*)}
  def  qs: Parser[List[Int]] = repsep(qx, qk)
  def  qx: Parser[Int] = "0" ^^^ 0
  def  qk: Parser[String] = ","
  def  qk1: Parser[String] = "["
  def  qk2: Parser[String] = "]"



  //a_list      :   ID [ '=' ID ] [ ',' ] [ a_list ]
  def  a_list = repsep(a_part, ",")

  def  a_part : Parser[Attr] = log(
    (ID ~ opt("=" ~> a_value) ^^ {
      case n ~ Some((q,v)) => Attr(n,Some(v),q)
      case n ~ None => Attr(n, None, false)
      } ))("a_part")
   
  def a_value =
    accept("string", { case StringLit(v) => (true,v)}) |
    (ID ^^ { case v => (false,v) })
     
  def a_string = log(accept("string", { case StringLit(v) => v }))("a_string")     
   
  //edge_stmt   :   (node_id | subgraph) edgeRHS [ attr_list ]
  def  edge_stmt = (node_id | subgraph) ~ ( "->" ~> rep1sep(node_id | subgraph, "->")) ~ attr_list ^^
    { case head ~ rest ~ attrs => Edge("?", attrs, (head :: rest):_*) }
   
  //node_stmt   :   node_id [ attr_list ]
  def  node_stmt = node_id ~ attr_list ^^
    { case Node(n,p) ~ a => Node(n,p,a:_*) }
   
  //node_id     :   ID [ port ]
  def  node_id = ID ~ opt(port) ^^
    { case n ~ p => Node(n, p) }
    
  //port    :   ':' ID [ ':' compass_pt ]
  //    |   ':' compass_pt
  def  port: Parser[Port] = ":" ~> (
    (ID ~ opt(":" ~> ID)) ^^ flatten2(Port) |
     ID ^^ { Port (_, None) } )
   
  //subgraph    :   [ subgraph optid ] stmt_list
  def  subgraph = "subgraph" ~> ( optid ~ stmt_list ) ^^
    { case n ~ s => Subgraph(n, s:_*) }
     
  //compass_pt      :   (n | ne | e | se | s | sw | w | nw)
  def compass_pt =
    "n" ^^^ "n"   |
    "ne" ^^^ "ne" |
    "e" ^^^ "e"   |
    "se" ^^^ "se" |
    "s" ^^^ "s"   |
    "sw" ^^^ "sw" |
    "w" ^^^ "w"   |
    "nw" ^^^ "nw"
   
  def ID:Parser[String] = IDs | IDi
  def IDs = accept("string", { case StringLit(n) => n })
  def IDi = accept("identifier", { case Identifier(n) => n})
   
}
