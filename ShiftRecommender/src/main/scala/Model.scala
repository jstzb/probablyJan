import java.time.LocalDate

import com.cra.figaro.algorithm.sampling._
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
  val random = new scala.util.Random()

  val numDays: Int = 2
  val numShifts: Int = 2
  val numTasks: Int = 3
  val numWorkers: Int = 5

  val defaultQuality: Element[Int] = Uniform((0 to 10): _*)
  val defaultTime: Element[Boolean] = Flip(0.5)

  val maxShifts: Int = 2
  val trainSize: Int = 3

  /* Worker 0 : 7,3,2
     Worker 1 : 2,7,3
     Worker 2 : 2,3,7
     Worker 3 : 5,2,5
     Worker 4 : 2,5,2
   */
  val data = Map(//(Assignment,Quality)
    0 -> (Seq(0,1,2),21),
    1 -> (Seq(1,0,2),12),
    2 -> (Seq(1,2,0),7),
    3 -> (Seq(2,1,0),9),
    4 -> (Seq(0,1,2),21)
  )

  val assignmentToPredict = Seq(0,2,1)


  class Worker(id: Int) {
    val taskQuality = new FixedSizeArray[Int](numTasks, i => Uniform((0 to 10): _*))
    // i d like defaultQuality.clone here
    val timetable = for (i <- 0 until numDays) yield for (j <- 0 until numShifts) yield Random.nextBoolean()

    def hasTime(day: Int, shift: Int): Boolean = {
      timetable(day)(shift)
    }
  }

  class Assignment(worker: Worker,shift: Shift){

  }

  class TimeTable(workerId: Int, weekId: Int){

  }

  /**
    * Shift(workers) represents an assignment of possible workers to tasks
    * Attention: number of possible workers > number of tasks
    */
  class Shift(id: Int,workers: Seq[Worker],assign:Seq[Int]) {
    var j = 0;

    lazy val index: Element[List[Int]] = generateUniqueIndex(numTasks)

    //val qualifications = new FixedSizeArray[Int](numTasks, i => Chain(index, (ls:List[Int]) => workers(ls(i)).taskQuality(i)))
     val qualifications = new FixedSizeArray[Int](numTasks, i => workers(assign(i)).taskQuality(i))

    val sumQuality = qualifications.foldLeft(0)(_ + _)

    def customScheme() : ProposalScheme = {
      TypedScheme(
        () => index,
        (ls:List[Int]) => Some(ProposalScheme(qualifications.elements: _*))
      )
    }

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

  class Week(id: Int,workers: Seq[Worker]) {
    lazy val shifts = Seq.tabulate(numDays*numShifts)( i => new Shift(i,workers,data(i)._1))
    lazy val sumQuality = getSumQuality()


    def getSumQuality(): Element[Int] = Container(shifts.map(_.sumQuality): _*).foldLeft(0)(_ + _)
  }

  def underline = println("#############################################################")

  def title(s: String) = {
    underline; println(s); underline;
  }

  def main(args: Array[String]) {
    Universe.createNew()

    val workers = Seq.tabulate(numWorkers)(i => new Worker(i))

    for (i <- 0 until trainSize) {
      val shift = new Shift(i,workers, data(i)._1)
      shift.sumQuality.observe(data(i)._2)
    }
    title("Registered variables are: ")
    val elements: List[Element[_]]= Universe.universe.activeElements
    for (element <- elements) println(element.toNameString)
    val futureShift = new Shift(10,workers,assignmentToPredict)
    //val alg = MetropolisHastings(10000, ProposalScheme.default,futureShift.qualifications.elements: _*)
    val alg = Importance(100,futureShift.qualifications.elements: _*)
    title("Starting sampling")
    alg.start()
    println("Prediction based on data for assignment: " + assignmentToPredict)
    for (i <- 0 until numTasks){
      val out = alg.computeExpectation(futureShift.qualifications(i), (i:Int)=> i.toDouble)
      println("Worker " + assignmentToPredict(i) + " will do task " + i + " at quality " + out )
    }
    underline
  }
}