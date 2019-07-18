package recapfunpatmat

class RecapFunPatMat {
  abstract class JSON
  case class JSeq(elems: List[JSON]) extends JSON
  case class JObj(bindings: Map[String, JSON]) extends JSON
  case class JNum(num: Double) extends JSON
  case class JStr(str: String) extends JSON
  case class JBool(b: Boolean) extends JSON
  case object JNull extends JSON

  val data = JObj(Map(
    "firstName" -> JStr("john"),
    "lastName" -> JStr("smith"),
    "address" -> JObj(Map(
      "street" -> JStr("123 main"),
      "city" -> JStr("Cambridge"),
      "postal" -> JNum(12345)
      )),
    "phoneNumbers" -> JSeq(List(
      JStr("12345"),
      JStr("5555555555")
      ))
    ))


  def show(json: JSON): String = {
    json match {
      case JSeq(seq) => {
        "[\n" + seq.map(show).mkString(", ") + "\n]\n"
      }
      case JObj(bindings) => {
        val assocs = bindings.map { case (key, value) => "\"" + key + "\": " + show(value)}
        "{" + assocs.mkString(",\n")+ "}\n"

      }
      case JNum(num) => num.toString
      case JStr(str) => "\"" + str + "\""
      case JBool(b) => b.toString
      case JNull => "null"
    }
  }

  // println(show(data))

  // val f = { case "ping" => "pong" } // the arguement types of expected
  // functions must be fully known (SLS 8.5)
  val f: String => String = { case "ping" => "pong" }

  def test: Unit = {
    println(f("ping"))
    // println(f("pong")) //MatchError: pong
  }

  val g: PartialFunction[String, String] = { case "ping" => "pong" }
  println(g.isDefinedAt("ping"))
  println(g.isDefinedAt("pong"))

  val h: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: y :: rest => "two"
  }

  println(h.isDefinedAt(List(1,2,3)))

  val i: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: rest => {
      rest match {
        case Nil => "two"
      }
    }
  }

  println(i.isDefinedAt(List(1,2,3))) // => true!
}
