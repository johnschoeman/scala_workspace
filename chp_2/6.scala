// Compound Expressions
//
// Conditionals
if(1 < 2) "Yes" else "No"
if(1 < 2) println("Yes") else println("No")

// Blocks
{ 1; 2; 3 }
{
  println("This is a side-effect")
  println("This too")
  3
}
def name: String = {
  val title = "Professor"
  val name = "Funkenstein"
  title + " " + name
}
name

if(1 > 2) "alien" else "preditor"
// type String, value "preditor"
if(1 > 2) "alien" else 2001
// type Any, value 2001
if(false) "hello"
// type Any, value ()
