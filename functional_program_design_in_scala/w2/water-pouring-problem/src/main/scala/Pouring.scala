package pouring

case class Pouring(capacity: Vector[Int]) {
  type State = Vector[Int]
  val initialState: State = capacity map (x => 0)

  trait Move {
    def update(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def update(state: State): State = state.updated(glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def update(state: State): State = state.updated(glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def update(state: State): State = {
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

  val glasses = 0 until capacity.length

  val moves: List[Move] =
    (
      (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))
    ).toList

  case class Path(states: List[Move]) {
    def endState: State = {
      states.foldRight(initialState) { _ update _ }
    }

    override def toString: String = {
      states.reverse.mkString(" -> ") + " : " + endState
    }
  }

  def rungs: Stream[Stream[Path]] = {
    val state: State = initialState

    def nextStates(state: => State): Stream[Stream[Path]] = {
      lazy val results: Stream[Stream[Path]] = {
        Stream(Path(List())) #:: (results map addMovesToPaths)
      }
      results
    }

    def addMovesToPaths(paths: Stream[Path]): Stream[Path] = {
      for {
        path <- paths
        move <- moves
      } yield (Path(move :: path.states))
    }
    nextStates(initialState)
  }

  def solveFor(targetVolume: Int): Path = {
    (for {
      rung <- rungs
      path <- rung
      if path.endState contains targetVolume
    } yield path
    ).take(1).toList.head
  }
}
