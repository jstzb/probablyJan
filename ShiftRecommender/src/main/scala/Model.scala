import java.time.LocalDate

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.collection.{Container, FixedSizeArray}
import com.cra.figaro.patterns.learning.{ModelParameters, ParameterCollection}

import scala.util.Random


/**
  *
  * Model of Shift Recommender
  *
  * constraints implemented:
  * one worker may only work once a shift
  * one worker may only work if he wants to work
  *
  *
  *
  * Created by jan on 21.07.16.
  */
object Model {
  val numDays: Int = 2
  val numShifts: Int = 2
  val numTasks: Int = 3
  val numWorkers: Int = 4

  val defaultQuality: Element[Int] = Uniform(0 to 10: _*)
  val defaultTime: Element[Boolean] = Flip(0.5)

  val maxShifts: Int = 2
  val trainSize: Int = 5


  class Worker(id: Int) {
    val taskQuality = new FixedSizeArray[Int](numTasks, i => Uniform(0 to 10: _*))
    // i d like defaultQuality.clone here
    val timetable = for (i <- 0 until numDays) yield for (j <- 0 until numShifts) yield Random.nextBoolean()

    def hasTime(day: Int, shift: Int): Boolean = {
      timetable(day)(shift)
    }
  }

  /**
    * Shift(workers) represents an assignment of possible workers to tasks
    * Attention: number of possible workers > number of tasks
    */
  class Shift(workers: Seq[Worker], day: Int, shift: Int) {
    var j = 0;
    lazy val index = generateUniqueIndex(numTasks)
    //val qualifications = new FixedSizeArray[Int](numTasks, i => Chain(index, (ls:List[Int]) => workers(ls(i)).taskQuality(i)))
    val qualifications = new FixedSizeArray[Int](numTasks, i => workers(i).taskQuality(i))
    val sumQuality = qualifications.foldLeft(0)(_ + _)

    def generateUniqueIndex(num: Int): Element[List[Int]] = {
      val values = 0 until numWorkers
      def uniqueCondition(seq: Seq[Int]) = seq.distinct.size == seq.size
      val index = new FixedSizeArray[Int](num, i => Uniform(values: _*))
      val allIndices: Element[List[Int]] = Inject(index.elements: _*)
      allIndices.setCondition(uniqueCondition)
      allIndices
    }


    def chainQual(num: Int): Element[Int] = {
      val res = workers(num).taskQuality(j)
      j += 1
      res
    }

    def getSumQuality(): Element[Int] = qualifications.foldLeft(0)(_ + _)

  }

  /**
    * Week(end) represents an assignment of possible workers to a each task in a
    * week
    *
    */

  class Week(workers: Seq[Worker]) {
    lazy val shifts = Seq.tabulate(numDays, numShifts)((day, shift) => new Shift(workers, day, shift))
    lazy val sumQuality = getSumQuality()


    def getSumQuality(): Element[Int] = Container(shifts.flatten.map(_.sumQuality): _*).foldLeft(0)(_ + _)
  }

  def underline = println("#############################################################")

  def title(s: String) = {
    underline; println(s); underline;
  }

  def main(args: Array[String]) {
    Universe.createNew()

    val workers = Seq.tabulate(numWorkers)(i => new Worker(i))

    for (i <- 0 until trainSize * 30) {
      val shift = new Shift(workers, 0, 0)
      shift.sumQuality.observe(20)
    }
    //val elements: List[Element[_]]= Universe.universe.activeElements
    //for (element <- elements) println(element.toNameString)
    title("Registered variables are: ")
    val futureShift = new Shift(workers, 0, 0)
    val alg = Importance(1000, futureShift.qualifications.elements: _*)
    title("Starting sampling")
    alg.start()
    val test1 = alg.distribution(futureShift.qualifications(0)).sortWith((lh, rh) => lh._1 > rh._1)
    for ((probability, value) <- test1) println(value + ": " + probability)
    underline
  }
}