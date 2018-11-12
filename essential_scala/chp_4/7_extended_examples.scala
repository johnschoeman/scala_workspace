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
