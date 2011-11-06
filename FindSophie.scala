// http://www.facebook.com/careers/puzzles.php?puzzle_id=11

object FindSophie {

  def main (args: Array[String]): Unit = {
    val expectedTime = calculateExpectedTime(RoomLoader.load())
    println("%3.2f".format(expectedTime))
  }

  /**
   * Returns the expected time to find Sophie on the optimal path through the room.
   */
  private def calculateExpectedTime(room: Room): Double = {
    val bestPath = new GreedyWalker(room).findBestPath

    if (bestPath == null) {
      return -1
    }

    println("going for " + bestPath)
    bestPath.expectedTime
  }
}

/**
 * Finds an approximated best path by going to the 'best' unvisited location, one step at a time.
 */
class GreedyWalker(room: Room) {

  def findBestPath(): Path = {
    var path = room.start

    while (!room.eachRequiredLocationVisited(path)) {
      val nextMove = room.remainingLocations(path)
        .flatMap(findPathsTo(room, path, _))
        .sortWith(leastCostlyHop)
      if (nextMove.isEmpty) return null
      path = nextMove.head
    }

    path
  }

  private def leastCostlyHop(p1: Path, p2: Path) = {
    def cost(p: Path) = (1 - p.head.probability) * p.maximumTime
    cost(p1) < cost(p2)
  }

  private def findPathsTo(room:Room, p: Path, to: Location): List[Path] = {
    def traverse(p: Path, visited: Set[Location]): List[Path] = {
      if (visited.contains(p.head)) return Nil
      room.connections
        .filter(_.contains(p.head))
        .flatMap { c =>
          val next = new Path(c.other(p.head), c.time, p)
          if (next.head == to) List(next)
          else traverse(next, visited + p.head)
        }
    }

    traverse(p, Set())
  }
}

/**
 * A room containing the locations & connections, i.e. a graph.
 */
class Room(val locations: List[Location], val connections: List[Connection]) {
  val start = new Path(locations.head)
  val requiredLocations = locations.filter(_.probability > 0)

  def remainingLocations(p: Path) = {
    val locations = p.locations
    requiredLocations.filterNot(locations.contains(_))
  }

  def eachRequiredLocationVisited(p: Path) = remainingLocations(p).isEmpty
}

/**
 * A location where Sophie can be found, i.e. a vertex.
 */
class Location(id: String, val probability: Double) {
  override def toString = id
}

/**
 * A connection between two locations, i.e. an edge.
 */
class Connection(val from: Location, val to: Location, val time: Double) {
  def contains(l: Location) = from == l || to == l
  def other(l: Location) = if (l == from) to else from
}

/**
 * A path used to walk between locations to find Sophie.
 */
class Path(val head: Location, val time: Double, val tail: Path) {
  def this(head: Location) = this(head, 0, null)
  def locations: List[Location] = List(head) ::: (if (tail == null) List() else tail.locations)

  private def contains(l: Location): Boolean = head == l || (tail != null && tail.contains(l))

  /**
   * True if this is the first time 'head' is encountered. This is important because when we visit a location
   * more than once, we should disregard the probability of finding Sophie since we've already checked.
   */
  private val firstVisit = tail == null || !tail.contains(head)

  /**
   * Returns the maximum time it would take to find Sophie on this path.
   */
  def maximumTime: Double = time + (if (tail == null) 0 else tail.maximumTime)

  /**
   * Returns the expected time, i.e. weighted average, it would take to find Sophie on this path.
   */
  def expectedTime: Double = {
    val probability = if (firstVisit) head.probability else 0
    (probability * maximumTime) +
      (if (tail == null) 0 else tail.expectedTime)
  }

  override def toString = {
    (if (tail == null) "" else (tail.toString + " > ")) +
    head
  }
}

/**
 * Pretending to read this stuff from a file..
 *   4
 *   front_door    .2
 *   in_cabinet    .3
 *   under_bed     .4
 *   behind_blinds .1
 *   5
 *   front_door under_bed     5
 *   under_bed  behind_blinds 9
 *   front_door behind_blinds 5
 *   front_door in_cabinet    2
 *   in_cabinet behind_blinds 6
 */
import scala.collection.mutable.LinkedHashMap
object RoomLoader {
  def load(): Room = {

  val l = new LinkedHashMap[String, Location]()
    l += "front_door" -> new Location("front_door",       .2)
    l += "in_cabinet" -> new Location("in_cabinet",       .3)
    l += "under_bed" -> new Location("under_bed",         .4)
    l += "behind_blinds" -> new Location("behind_blinds", .1)

  val c = List(
    new Connection(l.get("front_door").orNull, l.get("under_bed").orNull,     5.),
    new Connection(l.get("under_bed").orNull, l.get("behind_blinds").orNull,  9.),
    new Connection(l.get("front_door").orNull, l.get("behind_blinds").orNull, 5.),
    new Connection(l.get("front_door").orNull, l.get("in_cabinet").orNull,    2.),
    new Connection(l.get("in_cabinet").orNull, l.get("behind_blinds").orNull, 6.)
    )

    new Room(l.values.toList, c)
  }
}

