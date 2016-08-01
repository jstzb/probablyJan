import java.time.LocalDate

import com.cra.figaro.algorithm.OneTimeMPE
import com.cra.figaro.algorithm.factored.MPEVariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.MPEBeliefPropagation
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.collection.{Container, FixedSizeArray}

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
  val numDays: Int = 1
  val numShifts: Int = 1
  val numTasks: Int = 3
  val numWorkers: Int = 5

  lazy val defaultQuality: Element[Int] = Uniform((0 to 10): _*)
  lazy val defaultTime: Element[Boolean] = Flip(0.5)

  val maxShifts: Int = 2
  val trainSize: Int = 18

  /**
    * Data Generation
    *
    * workers maps a worker id to a sequence of qualities we want to observe
    */
  val workers = Map(
    0 -> Seq(2, 3, 2),
    1 -> Seq(5, 2, 3),
    2 -> Seq(2, 3, 5),
    3 -> Seq(3, 2, 7),
    4 -> Seq(2, 5, 2)
  )
  /**
    * Permutations generates some possible permutations of worker ids
    */
  val permutations: Seq[Seq[Int]] = (0 until numTasks).permutations.toSeq ++ (1 until numTasks + 1).permutations.toSeq ++ (2 until numTasks + 2).permutations.toSeq

  /**
    * Data calculates for each permutation its corresponding sum of worker task quality
    */
  val data = Seq.tabulate(permutations.length)(i => (permutations(i),
    workers(permutations(i)(0))(0)
      + workers(permutations(i)(1))(1)
      + workers(permutations(i)(2))(2)))

  /**
    *
    * @param id
    */

  class Worker(id: Int) {
    val taskQuality = new FixedSizeArray[Int](numTasks, i => Uniform((0 to 10): _*))
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
  class Shift(id: Int, workers: Seq[Worker], assign: Seq[Int]) {
    var j = 0;

    lazy val index: Element[List[Int]] = generateUniqueIndex(numTasks)

    //val qualifications = new FixedSizeArray[Int](numTasks, i => Chain(index, (ls:List[Int]) => workers(ls(i)).taskQuality(i)))
    val qualifications = new FixedSizeArray[Int](numTasks, i => workers(assign(i)).taskQuality(i))

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

  class Week(id: Int, workers: Seq[Worker]) {
    lazy val shifts = Seq.tabulate(numDays * numShifts)(i => new Shift(i, workers, data(i)._1))
    lazy val sumQuality = getSumQuality()


    def getSumQuality(): Element[Int] = Container(shifts.map(_.sumQuality): _*).foldLeft(0)(_ + _)
  }

  def underline = println("#############################################################")

  def title(s: String) = {
    underline;
    println(s);
    underline;
  }

  def recommend(alg: OneTimeMPE, workers: Seq[Worker]): Seq[Int] = {
    title("Recommender started ...")
    alg.start()
    val tuple = Seq.tabulate(numWorkers)(i => (i, Seq.tabulate(numTasks)(j => alg.mostLikelyValue(workers(i).taskQuality(j)))))
    for (j <- 0 until numWorkers) println("Calculated: " + tuple(j)._2 + ", Original: " + this.workers(j))
    val max = Seq.tabulate(numTasks)(i => tuple.max(Ordering.by((x: (Int, Seq[Int])) => x._2(i)))._1)
    println("Recommanding following worker combination for an optimal shift " + max)
    max
  }

  def calculateShiftQuality(alg:OneTimeMPE, workers: Seq[Worker],assign:Seq[Int]) : Int = {
    title("Calculation started ...")
    val shift = new Shift(trainSize+1,workers,assign)
    alg.start()
    val result = alg.mostLikelyValue(shift.sumQuality)
    println("Calculated following quality for given shift " + result)
    result
  }

  def main(args: Array[String]) {

    Universe.createNew()
    val workers = Seq.tabulate(numWorkers)(i => new Worker(i))
    title("Data input:")
    for (i <- data) println(i)
    println("Adding evidence to model: ")
    for (i <- 0 until trainSize) {
      val shift = new Shift(i, workers, data(i)._1)
      shift.sumQuality.observe(data(i)._2)
    }
    args.length match {
      case 1 => args(0) match {
        case "MH" => recommend(MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0)), workers)
        case "VE" => recommend(MPEVariableElimination(), workers)
        case "BP" => recommend(MPEBeliefPropagation(10), workers)
        case "help" =>
        case _ =>
          println("Input unknown")
          println("Please run with following arguments:")
          println("<MPE Algorithm> <size of evidence>")
      }
      case 2 => args match {
        case Array("VE",x) =>
        case Array("MH",x) =>
          val input = for (i:Char <- x) yield i.asDigit
          calculateShiftQuality(MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0)), workers,input)

      }
    }

    /*
    title("Registered variables are: ")
    val elements: List[Element[_]]= Universe.universe.activeElements
    for (element <- elements) println(element.toNameString)
     */
    underline
  }
}