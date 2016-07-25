import java.time.LocalDate

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.collection.{Container, FixedSizeArray}

/**
  * Created by jan on 21.07.16.
  */
object Model {
  val numDays: Int = 2
  val numShifts: Int = 2
  val numTasks: Int = 4
  val numWorkers: Int = 4

  val defaultQuality: Element[Double] = Beta(1, 1)
  val defaultTime: Element[Boolean] = Flip(0.5)

  val maxShifts: Int = 2

  class Worker() {
    val taskQuality = new FixedSizeArray[Double](numTasks, _ => Beta(1, 1))
    // i d like defaultQuality.clone here
    val timetable = new FixedSizeArray[Boolean](numDays * numShifts, (_: Int) => Flip(0.5))

    def hasTime(day: Int, shift: Int): Element[Boolean] = {
      timetable((day - 1) * numShifts + shift)
    }
  }

  /**
    * Shift(workers) represents an assignment of possible workers to tasks
    * Attention: number of possible workers > number of tasks
    */
  class Shift(workers: Seq[Worker]) {
    val index = generateIndexWoDup(numTasks)
    val taskQualifications = new FixedSizeArray[Double](numTasks, i => workers(index(i).value).taskQuality(i))

    def generateIndexWoDup(num:Int):Seq[Element[Int]] = {
      for (i <- 0 to num) yield
        Chain(Uniform(0,numWorkers),chainFunction)
    }

    def chainFunction(num:Int): Element[Int] = {
      val values: Seq[Int] = Seq.range(0,numWorkers-1)
      var duplicates = List[Int]()

      duplicates = num :: duplicates
      Uniform(values.filterNot(duplicates.contains(_)): _*)
    }

    def sumQuality(): Element[Double] = taskQualifications.foldLeft(0.0)(_ + _)
  }

  /**
    * Week(end) represents an assignment of possible workers to a each task in a
    * week
    *
    */

  class Week(end: LocalDate) {
    val workers = Seq.fill(numWorkers)(new Worker())
    val shifts = Seq.fill(numShifts*numDays)(new Shift(workers))


    def sumQuality(): Element[Double]= Container(shifts.map(_.sumQuality()): _*).foldLeft(0.0)(_ + _)
  }

  def main(args: Array[String]) {
    val week = new Week(LocalDate.now())
    week.sumQuality().addCondition(_ > 2.0)
    val alg = Importance(10000, week.shifts(0).taskQualifications.elements: _*)
    alg.start()
    val test = alg.probability(week.shifts(0).taskQualifications(1), (x: Double) => x > 0.1)
    println(test)

  }
}