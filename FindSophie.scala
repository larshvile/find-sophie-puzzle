// http://www.facebook.com/careers/puzzles.php?puzzle_id=11
import scala.collection.mutable.ListBuffer

object FindSophie {

  def main (args: Array[String]): Unit = {
    val started = System.currentTimeMillis
    val expectedTime = calculateExpectedTime(RoomLoader.load())
    println("%3.2f".format(expectedTime))
    System.err.println("> completed in " + (System.currentTimeMillis - started) + "ms")
  }

  private def calculateExpectedTime(room: Room): Double = {
    val shortestPath = room.wander()
      .sortWith(fastestPath)
      .head

    // TODO make sure that all required locations have been found
    // TODO watch out for locations with prob 0
//      return -1  impossible to find Sophie, disconnected vertcies..

    System.err.println("going for '" + shortestPath + "'")
    shortestPath.expectedTime
  }

  private def fastestPath(o1: Path, o2: Path) = o1.expectedTime < o2.expectedTime
}

/**
 * A room containing the locations & connections, i.e. a graph.
 */
class Room(val locations: List[Location], connections: List[Connection]) {

  /**
   * Wanders randomly through the room, returing each unique, acyclic path that contains each connected
   * location.
   */
  def wander(): List[Path] = {
    traverse(new Path(locations.head))
  }

  /*
   * Finding Sophie, the brute force way =)
   */
  private def traverse(p: Path): List[Path] = {
    if (hasCycle(p)) return Nil
    if (eachLocationsVisited(p)) return List(p)
    connections
      .filter(_.contains(p.head))
      .flatMap(c => traverse(new Path(c.other(p.head), c.time, p)))
  }

  private def eachLocationsVisited(p: Path) = p.locations.distinct.size == locations.size

  private def hasCycle(p: Path): Boolean = {
    var prev: Location = null
    p.locations.dropRight(2).foreach { l =>
      if (p.head == l && p.tail.head == prev) return true
      prev = l
    }
    false
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
  def locations: List[Location] = (if (tail == null) List() else tail.locations) ::: List(head)

  private def contains(l: Location): Boolean = head == l || (tail != null && tail.contains(l))

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

  /*  // booby trapped version of the graph above, front -> under seems like the best route when considering
      // only one hop at a time..
    val l = new LinkedHashMap[String, Location]()
    l += "front_door" -> new Location("front_door",       .2)
    l += "in_cabinet" -> new Location("in_cabinet",       .1)
    l += "under_bed" -> new Location("under_bed",         .15)
    l += "behind_blinds" -> new Location("behind_blinds", .55)

    val c = List(
      new Connection(l.get("front_door").orNull, l.get("under_bed").orNull,       2.),
//      new Connection(l.get("under_bed").orNull, l.get("behind_blinds").orNull,  9.),
//      new Connection(l.get("front_door").orNull, l.get("behind_blinds").orNull, 5.),
      new Connection(l.get("front_door").orNull, l.get("in_cabinet").orNull,      2.),
      new Connection(l.get("in_cabinet").orNull, l.get("behind_blinds").orNull,   1.)
      )*/

    new Room(l.values.toList, c)
  }
}

