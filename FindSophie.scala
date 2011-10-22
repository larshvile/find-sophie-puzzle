// http://www.facebook.com/careers/puzzles.php?puzzle_id=11

object FindSophie extends scala.App {
  val started = System.currentTimeMillis

  val room = RoomLoader.load()
  var path = new Path(room.start)

  // TODO This algorithm may not be able to find _the_ best route in a general graph, since it does one
  // hop at a time. In a graph with fewer edges there may be some booby traps, vertices that have low cost,
  // but ultimately leads to a greater cost because we have to backtrack or something..
  while (path.locations.distinct.size < room.locations.size) {
    val remainingLocations = room.locations.filterNot(path.locations.contains(_)) // TODO what about locations with 0 probability??
    val possiblePaths = remainingLocations.flatMap(room.findPathsTo(path, _))

    if (possiblePaths.isEmpty) {
      println("-1.00") // TODO cleanup, the traversal stuff should be a method that returns the expected-time as a double, nothing else..
      System.exit(0)
    }

    // let's go with the least costly path to one of the remaining locations
    path = possiblePaths
      .sortWith(_.cost < _.cost)
      .head
  }

  println("going for " + path)
  println("expected time: " + "%3.2f".format(path.expectedTime))
  println("completed in " + (System.currentTimeMillis - started) + "ms")
}

/**
 * A room containing the locations & connections, i.e. a graph.
 */
class Room(val locations: List[Location], connections: List[Connection]) {
  val start = locations.head

  def findPathsTo(path: Path, to: Location): List[Path] = {
    traverse(path, to, List(path.head))
  }

  private def traverse(path: Path, to: Location, visited: List[Location]): List[Path] = {
    var result: List[Path] = List()

    for (c <- connections.filter(_.contains(path.head))) {
      if (c.other(path.head) == to) {
        result = result ::: List(new Path(to, c.time, path))
      } else {
        val next = c.other(path.head)
        // TODO if the algorithm is improved, this should be replaced with a cycle-check
        if (!visited.contains(next)) { // don't backtrack on edges visited this traversal
          result = result ::: traverse(new Path(next, c.time, path),
            to,
            visited ::: List(next))
        }
      }
    }

    result
  }
}

/**
 * A location where Sophie can be found, i.e. a vertex.
 */
class Location(id: String, val probability: Double) {
  override def toString = id + ":" + probability
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
  def contains(l: Location): Boolean = head == l || (tail != null && tail.contains(l))
  def locations: List[Location] = (if (tail == null) List() else tail.locations) ::: List(head)

  /**
   * The cost, defined as time vs probability, of walking the path.
   */
  val cost: Double = (1. - head.probability) * time + (if (tail == null) 0 else tail.cost)

  /**
   * True if this is the first time 'head' is encountered. This is important because when we visit a location
   * more than once, we should disregard the probability of finding Sophie since we've already checked.
   */
  private val firstVisit = tail == null || !tail.contains(head)

  /**
   * Returns the maximum time it would take to find Sophie on this path.
   */
  private def maximumTime: Double = time + (if (tail == null) 0 else tail.maximumTime)

  /**
   * Returns the expected time, i.e. weighted average, it would take to find Sophie on this path.
   */
  def expectedTime: Double = {
    val probability = if (firstVisit) head.probability else 0
    (probability * maximumTime) +
      (if (tail == null) 0 else tail.expectedTime)
  }

  override def toString = {
    (if (tail == null) "" else (tail.toString + " >" + time + " ")) +
    (if (firstVisit) "" else "!") +
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
    l += "front_door" -> new Location("front_door", .2)
    l += "in_cabinet" -> new Location("in_cabinet", .3)
    l += "under_bed" -> new Location("under_bed", .4)
    l += "behind_blinds" -> new Location("behind_blinds", .1)

    val c = List(
      new Connection(l.get("front_door").orNull, l.get("under_bed").orNull, 5.),
      new Connection(l.get("under_bed").orNull, l.get("behind_blinds").orNull, 9.),
      new Connection(l.get("front_door").orNull, l.get("behind_blinds").orNull, 5.),
      new Connection(l.get("front_door").orNull, l.get("in_cabinet").orNull, 2.),
      new Connection(l.get("in_cabinet").orNull, l.get("behind_blinds").orNull, 6.)
      )

    new Room(l.values.toList, c)
  }
}

