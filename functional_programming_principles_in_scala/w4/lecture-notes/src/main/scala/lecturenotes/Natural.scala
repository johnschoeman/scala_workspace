package natural

// Peano Numbers

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new java.util.NoSuchElementException
  def successor: Nat = new Succ(Zero)
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) this else throw new java.util.NoSuchElementException

  override def toString: String = "0"
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = this.predecessor + that.successor
  def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor

  override def toString: String = "1 + " + n
}
