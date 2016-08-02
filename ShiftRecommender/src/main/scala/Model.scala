import com.cra.figaro.algorithm.OneTimeMPE
import com.cra.figaro.algorithm.factored.MPEVariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.MPEBeliefPropagation
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.{Poisson, Uniform}
import com.cra.figaro.library.collection.{Container, FixedSizeArray}
import com.quantifind.charts.Highcharts._

import scala.util.Random


/**
  *
  * Model of Shift Recommender
  *
  * Created by jan on 21.07.16.
  */
object Model {
  val numDays: Int = 1
  val numShifts: Int = 1
  val numTasks: Int = 3
  val numWorkers: Int = 5

  lazy val defaultQuality: Element[Int] = Poisson(5)
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
  val permutations: Seq[Seq[Int]] = (0 until numTasks).permutations.toSeq ++
    (1 until numTasks + 1).permutations.toSeq ++
    (2 until numTasks + 2).permutations.toSeq

  /**
    * Data calculates for each permutation its corresponding sum of worker task quality
    */
  val data = Seq.tabulate(permutations.length)(i => (permutations(i),
    workers(permutations(i)(0))(0) +
      workers(permutations(i)(1))(1) +
      workers(permutations(i)(2))(2)))
  /**
    * names maps each workerId to a name
    */
  val names = Map(
    0 -> "Anny",
    1 -> "Bob",
    2 -> "Cindy",
    3 -> "Dex",
    4 -> "Eddy"
  )

  /**
    * Model of a Worker
    *
    * @param id of a worker
    */

  class Worker(id: Int) {
    val taskQuality = new FixedSizeArray[Int](numTasks, i => Poisson(5))
    //val taskQuality = new FixedSizeArray[Int](numTasks, i => Uniform(0 to 10: _*))
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
    var j = 0

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
  }

  /**
    * Week(end) represents an assignment of possible workers to a each task in a
    * week
    *
    */

  class Week(id: Int, workers: Seq[Worker]) {
    lazy val shifts = Seq.tabulate(numDays * numShifts)(i => new Shift(i, workers, data(i)._1))
    lazy val sumQuality = Container(shifts.map(_.sumQuality): _*).foldLeft(0)(_ + _)
  }


  def recommend(alg: OneTimeMPE, workers: Seq[Worker]): Seq[Int] = {
    title("Recommender started ...")
    alg.start()
    val tuple = Seq.tabulate(numWorkers)(i => (i, Seq.tabulate(numTasks)(j => alg.mostLikelyValue(workers(i).taskQuality(j)))))
    for (j <- 0 until numWorkers) println(names(j) + " calculated quality: " + tuple(j)._2 + ", original: " + this.workers(j))
    val max = Seq.tabulate(numTasks)(i => tuple.max(Ordering.by((x: (Int, Seq[Int])) => x._2(i)))._1)
    underline()
    println("Recommending following worker combination for an optimal shift: ")
    println(max.map(names(_)))
    max
  }

  def calculateShiftQuality(alg: OneTimeMPE, workers: Seq[Worker], assign: Seq[Int]): Int = {
    title("Calculation started for " + assign.map(names(_)))
    val shift = new Shift(trainSize + 1, workers, assign)
    alg.start()
    val result = alg.mostLikelyValue(shift.sumQuality)
    println("Calculated following quality for given shift " + result)
    result
  }

  def rMSError(sampleSize: Int, workers: Seq[Worker]) = {
    title("Starting calculating most probable explanation with BP")
    var total0 = 0.0;
    val workerError0 = List.fill(numWorkers, numTasks)(0.0)
    for (i <- 0 until sampleSize) {
      val alg = MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0))
      alg.start()
      val tuple = Seq.tabulate(numWorkers)(i => (i, Seq.tabulate(numTasks)(j => alg.mostLikelyValue(workers(i).taskQuality(j)))))
      for (i <- 0 until numWorkers) {
        for (j <- 0 until numTasks) {
          val diff = tuple(i)._2(j) - this.workers(i)(j)
          val square = Math.pow(diff, 2)
          total0 += square
          workerError0(i).updated(j, square)
        }
      }
    }
    total0 = Math.sqrt(total0 / sampleSize)
    println("Root Mean Square error of all workers is " + total0.toString)
    println("RMS Error per Worker is " + (total0 / numWorkers))
    underline()

    title("Starting calculating most probable estimation with MH")
    var total1 = 0.0;
    val workerError1 = List.fill(numWorkers, numTasks)(0.0)
    for (i <- 0 until sampleSize) {
      val alg = MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0))
      alg.start()
      val tuple = Seq.tabulate(numWorkers)(i => (i, Seq.tabulate(numTasks)(j => alg.mostLikelyValue(workers(i).taskQuality(j)))))
      for (i <- 0 until numWorkers) {
        for (j <- 0 until numTasks) {
          val diff = tuple(i)._2(j) - this.workers(i)(j)
          val square = Math.pow(diff, 2)
          workerError1(i).updated(j, square)
          total1 += square
        }
      }
    }
    val perWorker0 = List.tabulate(numWorkers)((i: Int) => workerError0(i).foldLeft(0.0)(_ + _))
    val perWorker1 = List.tabulate(numWorkers)((i: Int) => workerError1(i).foldLeft(0.0)(_ + _))

    total1 = Math.sqrt(total1 / sampleSize)
    println("Root Mean Square error of all workers is " + total1.toString)
    println("Average RMS Error per Worker is " + (total1 / numWorkers))
    underline()
    generatePlot((0 until 4).toList, List(total0, total1, total0 / numWorkers, total1 / numWorkers), "Average per Worker / Total Error", "Error", "Error calculation")
  }

  def generatePlot(x: List[Int], y: List[Double], xl: String, yl: String, title: String) {
    bar(x, y)
    com.quantifind.charts.Highcharts.title(title)
    xAxis(xl)
    yAxis(yl)
  }

  def underline() = println("#############################################################")

  def title(s: String) = {
    underline()
    println(s)
    underline()
  }


  def main(args: Array[String]) {

    Universe.createNew()
    val workers = Seq.tabulate(numWorkers)(i => new Worker(i))
    title("Generating evidence to add to Model:")
    for (i <- 0 until trainSize) println("Combo " + data(i)._1.map(names(_)) + " has total shift quality " + data(i)._2)
    for (i <- 0 until trainSize) {
      val shift = new Shift(i, workers, data(i)._1)
      shift.sumQuality.observe(data(i)._2)
    }
    args.length match {
      case 0 => rMSError(20, workers)
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
        case Array("VE", x) =>
          val input = for (i: Char <- x) yield i.asDigit
          calculateShiftQuality(MPEVariableElimination(), workers, input)
        case Array("MH", x) =>
          val input = for (i: Char <- x) yield i.asDigit
          calculateShiftQuality(MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(0.5)), workers, input)
        case Array("BP", x) =>
          val input = for (i: Char <- x) yield i.asDigit
          calculateShiftQuality(MPEBeliefPropagation(trainSize), workers, input)
        case _ => println("Input unknown")
      }
    }

    /*
    title("Registered variables are: ")
    val elements: List[Element[_]]= Universe.universe.activeElements
    for (element <- elements) println(element.toNameString)
     */
    underline()
  }
}