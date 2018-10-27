// Writing Methods
//
// 1: Identify the Input and Output
// 2: Prepare Test Cases
// 3: Write the Declaration
// 4: Run the Code
// 5: Write the Body
// 5.1: Consider the Result Type
// 5.2: Consider the Input Type
// 6: Run the Code, Again
//
// ex. square(2.0) -> 4.0
// 1: Double -> Double
// 2:
assert(square(2.0) == 4.0)
assert(square(3.0) == 9.0)
assert(square(-2.0) == 4.0)
// 3:
// def square(in: Double): Double = ???
// 5:
def square(in: Double): Double =
  in * in

// test: make cube (Int) -> (Double)
assert(cube(1) == 1.0)
assert(cube(2) == 8.0)
assert(cube(3) == 27.0)

def cube(in: Int): Double =
  in * in * in
