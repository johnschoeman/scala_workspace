// Extended Examples

//4.7.0.1 A Calculator
sealed trait Expression {
  def eval: Calculation = {
    this match {
      case Addition(left, right) =>
        left.eval match {
          case Failure(reason) => Failure(reason)
          case Success(leftVal) =>
            right.eval match {
              case Failure(reason) => Failure(reason)
              case Success(rightVal) => Success(leftVal + rightVal)
            }
        }
      case Subtraction(left, right) =>
        left.eval match {
          case Failure(reason) => Failure(reason)
          case Success(leftVal) =>
            right.eval match {
              case Failure(reason) => Failure(reason)
              case Success(rightVal) => Success(leftVal - rightVal)
            }
        }
      case Division(left, right) =>
        left.eval match {
          case Failure(reason) => Failure(reason)
          case Success(leftVal) =>
            right.eval match {
              case Failure(reason) => Failure(reason)
              case Success(rightVal) =>
                if (rightVal == 0)
                  Failure("Division by zero")
                else
                  Success(leftVal / rightVal)
            }
        }
      case SquareRoot(input) =>
        input.eval match {
          case Failure(reason) => Failure(reason)
          case Success(value) =>
            if (value < 0)
              Failure("Square root of negative number")
            else
              Success(math.sqrt(value))
        }
      case Number(value) => Success(value)
    }
  }
}

case class Addition(left: Expression, right: Expression) extends Expression

case class Subtraction(left: Expression, right: Expression) extends Expression

case class Division(left: Expression, right: Expression) extends Expression

case class SquareRoot(value: Expression) extends Expression

case class Number(value: Double) extends Expression

sealed trait Calculation

case class Success(value: Double) extends Calculation

case class Failure(reason: String) extends Calculation

val expr1 = Addition(Number(2), Number(3))
val expr2 = Subtraction(Number(4), Number(1))
val expr3 = Addition(expr1, expr2)

assert(Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval == Failure("Square root of negative number"))
assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))
assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))

//4.7.0.2 JSON
// Json ::= JsSequence
//        | JsObject
//        | Primative
//
// JsSequence ::= JsSeqCell head:Json tail:JsSequence
//              | JsSeqEnd
//
// JsObject ::= JsObjCell key:Primative value:Json tail:JsObject
//           | JsObjEnd
//
// Primative ::= JsString value:String
//             | JsDouble value:Double
//             | JsBoolean value:Boolean
//             | JsNull

sealed trait Json {
  def print: String = {
    def quote(str: String): String = "\"" + str + "\""
    def objToString(obj: JsObject): String = {
      obj match {
        case JsObjCell(key, value, tail) =>
          val keyString: String = key.print
          val valueString: String = value.print
          s", $keyString: $valueString" + objToString(tail)
        case JsObjEnd => ""
      }
    }
    def seqToString(seq: JsSequence): String = {
      seq match {
        case JsSeqCell(head, tail) =>
          val headString: String = head.print
          s", $headString" + seqToString(tail)
        case JsSeqEnd => ""
      }
    }

    this match {
      case JsSeqCell(head, tail) => 
        val headString: String = head.print
        "[" + headString + seqToString(tail) + "]"
      case JsSeqEnd => "[]"
      case JsObjCell(key, value, tail) =>
        val keyString: String = key.print
        val valueString: String = value.print
        val tailString: String = objToString(tail)
        s"{$keyString: $valueString$tailString}"
      case JsObjEnd => "{}"
      case JsString(value) => quote(value)
      case JsDouble(value) => value.toString
      case JsBoolean(value) => value.toString
      case JsNull => "null"
    }
  }
}

sealed trait JsSequence extends Json
sealed trait JsObject extends Json
sealed trait Primative extends Json
case class JsSeqCell(head: Json, tail: JsSequence) extends JsSequence
case object JsSeqEnd extends JsSequence
case class JsObjCell(key: Primative, value: Json, tail: JsObject) extends JsObject
case object JsObjEnd extends JsObject
case class JsString(value: String) extends Primative
case class JsDouble(value: Double) extends Primative
case class JsBoolean(value: Boolean) extends Primative
case object JsNull extends Primative

// ["a string", 1.0, true]
val json1 = JsSeqCell(JsString("a string"), JsSeqCell(JsDouble(1.0), JsSeqCell(JsBoolean(true), JsSeqEnd)))

// {
//   "a": [1,2,3]
//   "b": ["a","b","c"]
//   "c": { "doh":true, "ray":false, "me":1 }
// }

val json123 = JsSeqCell(JsDouble(1.0), JsSeqCell(JsDouble(2.0), JsSeqCell(JsDouble(3.0), JsSeqEnd)))
val jsonabc = JsSeqCell(JsString("a"), JsSeqCell(JsString("b"), JsSeqCell(JsString("c"), JsSeqEnd)))
val jsondoh = JsObjCell(JsString("doh"), JsBoolean(true), JsObjCell(JsString("ray"), JsBoolean(false), JsObjCell(JsString("me"), JsDouble(1.0), JsObjEnd)))
val jsonobj = JsObjCell(JsString("a"), json123, JsObjCell(JsString("b"), jsonabc, JsObjCell(JsString("c"), jsondoh, JsObjEnd)))
