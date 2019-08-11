package pouring

case class Pouring(capacity: Vector[Int]) {
  type State = Vector[Int]
  val initialState: State = capacity map (x => 0)

  trait Move
  case class Empty(glass: Int) extends Move
  case class Fill(glass: Int) extends Move
  case class Pour(from: Int, to: Int) extends Move

  def makeMove(move: Move, state: State): State = {
    move match {
      case Empty(glass) => state.updated(glass, 0)
      case Fill(glass) => state.updated(glass, capacity(glass))
      case Pour(from, to) => {
        val fromAmt: Int = state(from)
        val toAmt: Int = state(to)
        val maxAvail: Int = capacity(to) - toAmt
        if (fromAmt <= maxAvail) {
          state.updated(from, 0).updated(to, toAmt + fromAmt)
        } else {
          state.updated(from, fromAmt - maxAvail).updated(to, toAmt + maxAvail)
        }
      }
    }
  }

  val glasses = 0 until capacity.length

  val moves: List[Move] =
    (
      (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))
    ).toList

  case class Path(states: List[(Move, State)]) {
    def endState: State = {
      states.head._2
    }

    override def toString: String = {
      states.reverse.mkString(" -> ") + " : " + endState
    }
  }

  def generateRungs: Stream[Stream[Path]] = {
    val state: State = initialState

    def addMovesToPaths(paths: Stream[Path]): Stream[Path] = {
      paths.flatMap { (path: Path) => addMovesToPath(path) }
    }

    def addMovesToPath(path: Path): List[Path] = {
      val states = path.states
      val lastState: State = states.head._2

      addMovesToState(lastState).map { (tuple: (Move, State)) => Path(tuple :: path.states) }
    }

    def addMovesToState(state: State): List[(Move, State)] = {
      moves map { (move: Move) => (move, makeMove(move, state)) }
    }

    def nextStates(state: => State): Stream[Stream[Path]] = {
      lazy val results: Stream[Stream[Path]] = {
        Stream(Path(List((Empty(0), state)))) #:: (results map addMovesToPaths)
      }
      results
    }

    nextStates(initialState)
  }

  def isAtTarget(targetVolume: Int)(path: Path): Boolean = {
    path.states.head._2.contains(targetVolume)
  }

  def hasAValidPath(rung: List[Path], targetVolume: Int): Boolean = {
    rung exists isAtTarget(targetVolume)
  }

  def validPaths(rung: Stream[Path], targetVolume: Int): Stream[Path] = {
    rung filter isAtTarget(targetVolume)
  }

  def isNotEmpty(rung: Stream[Path]): Boolean = rung.length > 0

  def solveFor(targetVolume: Int): Path = {
    generateRungs.map(validPaths(_, targetVolume)).filter(isNotEmpty(_)).take(1).toList.head.take(1).toList.head
  }

  override def toString: String = {
    moves.mkString(",")
  }
}
