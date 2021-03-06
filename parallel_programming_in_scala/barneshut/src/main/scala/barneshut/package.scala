import java.util.concurrent._
import scala.{collection => coll}
import scala.util.DynamicVariable
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad extends QuadInterface {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0f
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, coll.Seq(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val quadList = List(nw, ne, sw, se)
//    val quadListX = quadList.map(_.centerX)
//    val quadListY = quadList.map(_.centerY)
//    val centerX: Float = (quadListX.min + quadListX.max) / 2.0f
//    val centerY: Float = (quadListY.min + quadListY.max) / 2.0f
    val centerX: Float = quadList.map(_.centerX).sum / 4f
    val centerY: Float = quadList.map(_.centerY).sum / 4f
    val size: Float = nw.size * 2
    val mass: Float = quadList.map(_.mass).sum
    val massX: Float = if (mass> 0) quadList.map(q => q.mass * q.massX).sum / mass else centerX
    val massY: Float = if (mass> 0) quadList.map(q => q.mass * q.massY).sum / mass else centerY
    val total: Int = quadList.map(_.total).sum

    def insert(b: Body): Fork = {
      (b.x <= centerX, b.y <= centerY) match {
        case (false, false) => Fork(nw, ne, sw, se.insert(b))
        case (false, true) => Fork(nw, ne, sw.insert(b), se)
        case (true, false) => Fork(nw, ne.insert(b), sw, se)
        case (true, true) => Fork(nw.insert(b), ne, sw, se)
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: coll.Seq[Body])
  extends Quad {
    val (mass, massX, massY) = (bodies.map(_.mass).sum : Float, bodies.map(b=>b.mass * b.x).sum / bodies.map(_.mass).sum : Float, bodies.map(b=>b.mass * b.y).sum / bodies.map(_.mass).sum : Float)
    val total: Int = bodies.length
    def insert(b: Body): Quad = {
      if (size <= minimumSize) Leaf(centerX, centerY, size, b +: bodies)
      else {
        val halfSize = size / 2.0f
        val quaterSize = size / 4.0f
        val westX = centerX - quaterSize
        val eastX = centerX + quaterSize
        val northY = centerY - quaterSize
        val southY = centerY + quaterSize

        val fork = Fork(
          Empty(westX, northY, halfSize),
          Empty(eastX, northY, halfSize),
          Empty(westX, southY, halfSize),
          Empty(eastX, southY, halfSize)
        )
        (b +: bodies).foreach {fork.insert(_)}
        fork
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies.foreach {b => addForce(b.mass, b.x, b.y)}
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body,
          // or recursion is needed
          if (quad.size / distance(quad.massX, quad.massY, x, y) < theta)
            addForce(quad.mass, quad.massX, quad.massY)
          else Seq(nw, ne, sw, se).foreach {traverse}
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) extends SectorMatrixInterface {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      def sectorToGo(p1: Float, p2: Float): Int = {
        ((p1-p2)/sectorSize).toInt.max(0).min(sectorPrecision - 1)
      }
      this(sectorToGo(b.x, boundaries.minX), sectorToGo(b.y, boundaries.minY)) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for (i <- 0 until matrix.length) matrix.update(i, matrix(i).combine(that.matrix(i)))
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }

  def main(args: Array[String]):Unit = {
    val b = new Body(1.0f, 20.0f, 30.0f, 0f, 0f)
    val nw = Empty(10.0f, 30.0f, 10.0f)
    val ne = Empty(20.0f, 30.0f, 10.0f)
    val sw = Empty(10.0f, 40.0f, 10.0f)
    val se = Empty(20.0f, 40.0f, 10.0f)
    println(Fork(nw, ne, sw, se))
    println(Fork(nw, ne.insert(b), sw.insert(b), se.insert(b)))
  }
}
