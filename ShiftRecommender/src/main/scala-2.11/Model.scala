import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.collection.FixedSizeArray
import com.quantifind.charts._
import scala.util.Random
/**
  *
  * Model of Shift Recommender
  *
  * Created by jan on 21.07.16.
  */
object Model {
  lazy val defaultQuality: Element[Int] = Poisson(5)
  lazy val defaultTime: Element[Boolean] = Flip(0.5)
  val numDays: Int = 7
  val numShifts: Int = 2
  val numTasks: Int = 5
  val numWorkers: Int = 24
  val maxShifts: Int = 4
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
    * Data calculates for each permutation its corresponding sum of worker task quality
    */
  def generateData(): Seq[(Seq[Int], Int)] = {
    val comb: Seq[IndexedSeq[Int]] = (0 to numWorkers-1).combinations(numTasks).toSeq
    val perm: Seq[IndexedSeq[Int]] = comb.map(_.permutations).flatten
    Seq.tabulate(perm.length)(i=>
      (perm(i),Seq.tabulate(numTasks)(j => workers(perm(i)(j))(j)).foldLeft(0)(_+_)))
  }

  /**
    * names maps each workerId to a name
    */
  val names = Map(
    0 -> "Anny",
    1 -> "Bob",
    2 -> "Cindy",
    3 -> "Dex",
    4 -> "Eddy",
    5 -> "David",
    6 -> "Chris",
    7 -> "Maria",
    8 -> "Maike",
    9 -> "Sonja",
    10 -> "Alex",
    11 -> "Peter",
    12 -> "Moritz",
    13 -> "Josef",
    14 -> "Benni",
    15 -> "Test",
    16 -> "Test",
    17 -> "Test",
    18 -> "Test",
    19 -> "Test",
    20 -> "Test",
    21 -> "Test",
    22 -> "Test",
    23 -> "Test",
    24 -> "Test",
    25 -> "Test",
    26 -> "Test",
    27 -> "Test",
    28 -> "Test",
    29 -> "Test",
    30 -> "Test"
  )

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

  def accuracy(samplesize: Int) = {
    title("Starting accuracy calculations")
    var total = 0.0
    val data = generateData()
    for (i <- 0 until samplesize) {
      Universe.createNew()
      val workers = Seq.tabulate(numWorkers)(j => new Worker(j))
      for (j <- 0 until trainSize - 1) {
        val shift = new Shift(0, workers, data(j)._1)
        shift.sumQuality.observe(data(j)._2)
      }
      val shift = new Shift(0, workers, data(trainSize - 1)._1)
      val alg = MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0))
      alg.start()
      val calc = alg.mostLikelyValue(shift.sumQuality)
      val exact = data(trainSize-1)._2
      val result = Math.pow(calc - exact, 2)
      println( "Error " + i + ": " +Math.sqrt(result))
      total += result
    }
    Math.sqrt(total)/samplesize
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
    //generatePlot((0 until 4).toList, List(total0, total1, total0 / numWorkers, total1 / numWorkers), "Average per Worker / Total Error", "Error", "Error calculation")
  }

//  def generatePlot(x: List[Int], y: List[Double], xl: String, yl: String, title: String) {
//    bar(x, y)
//    com.quantifind.charts.Highcharts.title(title)
//    xAxis(xl)
//    yAxis(yl)
//  }

  def underline() = println("#############################################################")

  def title(s: String) = {
    underline()
    println(s)
    underline()
  }

  def main(args: Array[String]) {
    val week = new Week()
    week.readTimetableFromFile("/home/jan/time1.csv")
    val recommender = new Recommender(week)
    recommender.readQualificationsFromFile("/home/jan/qual.csv")
    recommender.writeFile("/home/jan/recom.csv")

  }

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
    lazy val index: Element[List[Int]] = generateUniqueIndex(numTasks)
    //val qualifications = new FixedSizeArray[Int](numTasks, i => Chain(index, (ls:List[Int]) => workers(ls(i)).taskQuality(i)))
    val qualifications = new FixedSizeArray[Int](numTasks, i => workers(assign(i)).taskQuality(i))
    val sumQuality = qualifications.foldLeft(0)(_ + _)
    var j = 0

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

}
