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

  case class Path(history: List[Move], val endState: State) {
    def extend(move: Move): Path = new Path(move :: history, move update endState)

    override def toString: String = {
      history.reverse.mkString(" -> ") + " : " + endState
    }
  }

  val initialPath: Path = Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }
  }

  val pathSets: Stream[Set[Path]] = from(Set(initialPath), Set(initialState))

  def solutions(targetVolume: Int): Stream[Path] = {
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains targetVolume
    } yield path
  }
}
