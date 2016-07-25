import java.time.LocalDate

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Apply, Constant, Element, Flip}
import com.cra.figaro.library.atomic.continuous.Beta
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
    val timetable = new FixedSizeArray[Boolean](numDays * numShifts, (_: Int) => Flip(0.5)) // multidimensional fixedarrays are not supported

    def hasTime(day: Int, shift: Int): Element[Boolean] = {
      timetable((day - 1) * numShifts + shift)
    }
  }

  /**
    * Shift(workers) represents an assignment of possible workers to tasks
    * Attention: number of possible workers > number of tasks
    */
  class Shift(workers: Array[Worker]) {
    val taskQualifications = new FixedSizeArray[Double](numTasks, i => workers(i).taskQuality(i))

    def sumQuality(): Element[Double] = taskQualifications.foldLeft(0.0)(_ + _)
  }

  /**
    * Week(end) represents an assignment of possible workers to a each task in a
    * week
    *
    */

  class Week(end: LocalDate) {
    val workers = Array.fill(numWorkers)(new Worker())
    val shifts = Array.fill(numShifts)(new Shift(workers))

    def sumQuality(): Element[Double]= Container(shifts.map(x => x.sumQuality()): _*).foldLeft(0.0)(_ + _)
  }

  def main(args: Array[String]) {
    val week = new Week(LocalDate.now())
    week.shifts(0).sumQuality().addCondition(_ > 2.0)
    val alg = Importance(1000, week.shifts(0).taskQualifications.elements: _*)
    alg.start()
    val test = alg.probability(week.shifts(0).taskQualifications(1), (x: Double) => x > 0.1)
    println(test)

  }
}