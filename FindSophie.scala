// http://www.facebook.com/careers/puzzles.php?puzzle_id=11

object FindSophie {

  def main (args: Array[String]): Unit = {
    val started = System.currentTimeMillis
    val expectedTime = calculateExpectedTime(RoomLoader.load())
    println("%3.2f".format(expectedTime))
    System.out.println("> completed in " + (System.currentTimeMillis - started) + "ms")
  }

  /**
   * Returns the expected time to find Sophie on the optimal path through the room.
   */
  private def calculateExpectedTime(room: Room): Double = {
    val approximation = new GreedyNearestNeighborWalker(room).bestPath

    if (approximation == null) {
      return -1
    }

    println("approximated best path: " + approximation + " / " + approximation.expectedTime)

    val bestPath = new BruteForceWalker(room, approximation.expectedTime).bestPath
    println("best path:              " + bestPath + " / " + bestPath.expectedTime)

    return bestPath.expectedTime
  }
}

/**
 * TODO
 */
trait Walker {
  def bestPath(): Path
}

/**
 * Finds an approximated best path by going to the 'best' unvisited location, one hop at a time. TODO neh
 */
class GreedyNearestNeighborWalker(room: Room) extends Walker { // TODO not really nereast neighbor, since it may go for less costly !neighbours

  def bestPath(): Path = {
    var path = room.start

    while (!room.eachRequiredLocationVisited(path)) {
      val nextMove = room.remainingLocations(path)
        .flatMap(findPathsTo(room, path, _))
        .sortWith(leastCostlyHop)
      if (nextMove.isEmpty) return null
      println() // TODO remove
      nextMove.foreach(println)
      path = nextMove.head
    }
    println("\n") // TODO remove

    path
  }

  private def leastCostlyHop(p1: Path, p2: Path) = {
    def cost(p: Path) = (1 - p.head.probability) * p.maximumTime // TODO this kind of finds the optmial path.. why??
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
 * TODO
 */
import scala.collection.mutable.HashSet

class BruteForceWalker(room: Room, approximatedExpectedTime: Double) extends Walker {

  private val cyclicPaths = new HashSet[Path]() // TODO possible to move this out of here??

  def bestPath(): Path = {
    findAllPaths()
      .sortWith(lowestExpectedTime)
      .head
  }

  private def lowestExpectedTime(p1: Path, p2: Path) = p1.expectedTime < p2.expectedTime

  private def findAllPaths(): List[Path] = {
    def traverse(p: Path): List[Path] = {
      if (containsCycle(p)) return Nil
      // TODO if (currentExpectedTime(p) > approximatedExpectedTime) return Nil // TODO approximated or current lowest?
      if (room.eachRequiredLocationVisited(p)) return List(p)

      room.connections
        .filter(_.contains(p.head))
        .flatMap(c => traverse(new Path(c.other(p.head), c.time, p)))
    }

    traverse(room.start)
  }

  private def currentExpectedTime(p: Path): Double = {
    if (p.firstVisit) return p.expectedTime

    val mostProbableRemainingLocation = room.remainingLocations(p) // TODO naming
      .sortWith(_.probability > _.probability)
      .head

    mostProbableRemainingLocation.probability * p.maximumTime
  }

  // TODO externalize the entire cycle-checking..
  private def containsCycle(p: Path): Boolean = {
    if (tailIsCyclic(p)) return true
    val locations = p.locations

    for (step <- 1 to locations.size / 2) {
      if (locations.slice(0, step) == locations.slice(step, step * 2)) {
        storeCycle(p, step - 1) // TODO -2 when step starts as 1??
        println("cycle at step " + step + ", current: " + cyclicPaths.size)
        return true
      }
    }

    false
  }

  private def tailIsCyclic(p:Path) = {
    p.tail != null && cyclicPaths.contains(p.tail)
  }

  private def storeCycle(p: Path, depth: Int): Unit = {
    if (depth == 0) return
    cyclicPaths += p
    storeCycle(p.tail, depth - 1)
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
  val firstVisit = tail == null || !tail.contains(head)

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

//  val l = new LinkedHashMap[String, Location]()
//    l += "front_door" -> new Location("front_door",       .2)
//    l += "in_cabinet" -> new Location("in_cabinet",       .3)
//    l += "under_bed" -> new Location("under_bed",         .4)
//    l += "behind_blinds" -> new Location("behind_blinds", .1)
//    l += "void" -> new Location("void", 0) // Booby trap.. no way to reach the void =) But there's no need to either..

//  val c = List(
//    new Connection(l.get("front_door").orNull, l.get("under_bed").orNull,     5.),
//    new Connection(l.get("under_bed").orNull, l.get("behind_blinds").orNull,  9.),
//    new Connection(l.get("front_door").orNull, l.get("behind_blinds").orNull, 5.),
//    new Connection(l.get("front_door").orNull, l.get("in_cabinet").orNull,    2.),
//    new Connection(l.get("in_cabinet").orNull, l.get("behind_blinds").orNull, 6.)
//    )

// more edges...
//  val l = new LinkedHashMap[String, Location]()
//    l += "front_door" -> new Location("front_door",       .2)
//    l += "in_cabinet" -> new Location("in_cabinet",       .3)
//    l += "under_bed" -> new Location("under_bed",         .2)
//    l += "behind_blinds" -> new Location("behind_blinds", .1)
//    l += "x" -> new Location("x", .1)
//    l += "y" -> new Location("y", .1)

//  val c = List(
//    new Connection(l.get("front_door").orNull, l.get("under_bed").orNull,     5.),
//    new Connection(l.get("under_bed").orNull, l.get("behind_blinds").orNull,  9.),
//    new Connection(l.get("front_door").orNull, l.get("behind_blinds").orNull, 5.),
//    new Connection(l.get("front_door").orNull, l.get("in_cabinet").orNull,    2.),
//    new Connection(l.get("in_cabinet").orNull, l.get("behind_blinds").orNull, 6.),
//    new Connection(l.get("under_bed").orNull, l.get("in_cabinet").orNull,     2.),
//    new Connection(l.get("under_bed").orNull, l.get("x").orNull,              2.),
//    new Connection(l.get("x").orNull, l.get("y").orNull,                      10.),
//    new Connection(l.get("y").orNull, l.get("in_cabinet").orNull,             4.)
//    )

    // booby trapped version of the graph above, front -> under seems like the best route in greedy-mode
    val l = new LinkedHashMap[String, Location]()
    l += "front_door" -> new Location("front_door",       .2)
    l += "in_cabinet" -> new Location("in_cabinet",       .1)
    l += "under_bed" -> new Location("under_bed",         .15)
    l += "behind_blinds" -> new Location("behind_blinds", .55)

    val c = List(
      new Connection(l.get("front_door").orNull, l.get("under_bed").orNull,       2.),
      new Connection(l.get("front_door").orNull, l.get("in_cabinet").orNull,      2.),
      new Connection(l.get("in_cabinet").orNull, l.get("behind_blinds").orNull,   1.)
      )

  // requires 'proper' cycle detection (best path is a -> b -> c -> b -> d -> b -> c -> e)
//  val l = new LinkedHashMap[String, Location]()
//    l += "a" -> new Location("a",       .2)
//    l += "b" -> new Location("b",       .2)
//    l += "c" -> new Location("c",       .2)
//    l += "d" -> new Location("d",       .2)
//    l += "e" -> new Location("e",       .2)

//  val c = List(
//    new Connection(l.get("a").orNull, l.get("b").orNull,     10),
//    new Connection(l.get("b").orNull, l.get("d").orNull,     10),
//    new Connection(l.get("b").orNull, l.get("c").orNull,     1),
//    new Connection(l.get("c").orNull, l.get("e").orNull,     20)
//    )

    new Room(l.values.toList, c)
  }
}

