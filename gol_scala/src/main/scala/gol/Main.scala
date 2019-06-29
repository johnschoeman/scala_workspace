object Main extends App {
  def generateBoard: Board = {
    new Board(List(
      List(Cell(false), Cell(false), Cell(false),Cell(false), Cell(false), Cell(false), Cell(false), Cell(false)),
      List(Cell(false), Cell(true), Cell(false),Cell(false), Cell(false), Cell(true), Cell(false), Cell(false)),
      List(Cell(false), Cell(true), Cell(true),Cell(false), Cell(true), Cell(true), Cell(false), Cell(false)),
      List(Cell(false), Cell(true), Cell(false),Cell(true), Cell(false), Cell(true), Cell(false), Cell(false)),
      List(Cell(false), Cell(true), Cell(false),Cell(false), Cell(false), Cell(true), Cell(false), Cell(false)),
      List(Cell(false), Cell(true), Cell(false),Cell(false), Cell(false), Cell(true), Cell(false), Cell(false)),
      List(Cell(false), Cell(true), Cell(false),Cell(false), Cell(false), Cell(true), Cell(false), Cell(false)),
      List(Cell(false), Cell(true), Cell(false),Cell(false), Cell(false), Cell(true), Cell(false), Cell(false)),
      List(Cell(false), Cell(true), Cell(false),Cell(false), Cell(false), Cell(true), Cell(false), Cell(false)),
      List(Cell(false), Cell(false), Cell(false),Cell(false), Cell(false), Cell(false), Cell(false), Cell(false)),
      List(Cell(false), Cell(false), Cell(false),Cell(false), Cell(false), Cell(false), Cell(false), Cell(true)),
      List(Cell(false), Cell(false), Cell(false),Cell(false), Cell(false), Cell(true), Cell(false), Cell(true)),
      List(Cell(false), Cell(false), Cell(false),Cell(false), Cell(false), Cell(false), Cell(true), Cell(true)),
    ))
  }

  def loop(board: Board, interval: Int, n: Int): Unit = {
    if (n > 0) {
      println("")
      println("")
      println("")
      println("")
      println("")
      println("")
      println("")
      println("")
      println("- - - - - - - - - - - - -")
      println(board)
      println("- - - - - - - - - - - - -")
      Thread.sleep(interval)
      loop(board.tick, interval, n - 1)
    }
  }

  val board = generateBoard
  loop(board, 100, 100)
}

case class Board(val rows: List[List[Cell]]) {
  def tick: Board = {
    new Board(
      (0 to bottomRowIndex).toList.map(rowIndex => {
        (0 to rightColumnIndex).toList.map(colIndex => {
          val aliveCellCount = neighbors(rowIndex, colIndex).filter(_.alive).length
          this.rows(rowIndex)(colIndex).nextGen(aliveCellCount)
        })
      })
    )
  }


  def neighbors(row: Int, col: Int): List[Cell] = {
    List(
      (reduceIndex(row, bottomRowIndex), reduceIndex(col, rightColumnIndex)),
      (reduceIndex(row, bottomRowIndex), col),
      (reduceIndex(row, bottomRowIndex), increaseIndex(col, rightColumnIndex)),
      (row, reduceIndex(col, rightColumnIndex)),
      (row, increaseIndex(col, rightColumnIndex)),
      (increaseIndex(row, bottomRowIndex), reduceIndex(col, rightColumnIndex)),
      (increaseIndex(row, bottomRowIndex), col),
      (increaseIndex(row, bottomRowIndex), increaseIndex(col, rightColumnIndex)),
    ).map{case (row: Int, col: Int) => rows(row)(col)}
  }

  def bottomRowIndex: Int = rows.length - 1
  def rightColumnIndex: Int = rows(0).length - 1

  def reduceIndex(number: Int, max: Int): Int = number match {
    case 0 => max
    case _ => number - 1
  }

  def increaseIndex(number: Int, max: Int): Int = if (number == max) 0 else number + 1

  override def toString = {
    rows.map(row => row.map(_.toString).mkString("")).mkString("\n")
  }
}

case class Cell(val alive: Boolean = true) {
  override def toString = if (alive) "*" else " "

  def nextGen(aliveNeighborCount: Int): Cell = this match {
    case Cell(true) => {
      if (aliveNeighborCount == 2 || aliveNeighborCount == 3) Cell(true) else Cell(false)
    }

    case Cell(false) => {
      if (aliveNeighborCount == 3) Cell(true) else Cell(false)
    }
  }
}
