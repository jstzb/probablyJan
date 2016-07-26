import java.time.LocalDate

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.collection.{Container, FixedSizeArray}
import com.cra.figaro.patterns.learning.{ModelParameters, ParameterCollection}


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

  val defaultQuality: Element[Double] = Beta(1, 1)
  val defaultTime: Element[Boolean] = Flip(0.5)

  val maxShifts: Int = 2

  class Worker(id: Int,parameters:ModelParameters) {
    val taskQuality = new FixedSizeArray[Double](numTasks, i => Beta(1, 1)("worker"+id+",task" + i,parameters))
    // i d like defaultQuality.clone here
    val timetable = for (i <- 0 until numDays) yield new FixedSizeArray[Boolean](numShifts, (_: Int) => Flip(0.5))

    def hasTime(day: Int, shift: Int): Boolean = {
      timetable(day)(shift).value
    }
  }

  /**
    * Shift(workers) represents an assignment of possible workers to tasks
    * Attention: number of possible workers > number of tasks
    */
  class Shift(workers: Seq[Worker], day: Int, shift: Int) {
    val index = generateIndexWoDup(numTasks)
    val qualifications = new FixedSizeArray[Double](numTasks, i => workers(index(i).value).taskQuality(i))

    def generateIndexWoDup(num: Int): Seq[Element[Int]] = {
      for (i <- 0 until num) yield
        Chain(Uniform(0, numWorkers), chainFunction)
    }

    def chainFunction(num: Int): Element[Int] = {
      val values: Seq[Int] = Seq.range(0, numWorkers - 1)
      var duplicates = List[Int]()

      duplicates = num :: duplicates
      Uniform(values
        .filterNot(duplicates.contains(_)) // ensures there are no duplicates
        .filterNot(workers(_).hasTime(day, shift)) // ensures workers has time
        : _*)
    }

    def sumQuality(): Element[Double] = qualifications.foldLeft(0.0)(_ + _)

    def observeWorkerAssign(is: Seq[Int]): Unit = for (i <- 0 until is.size) index(i).observe(is(i))
  }

  /**
    * Week(end) represents an assignment of possible workers to a each task in a
    * week
    *
    */

  class Week(parameters:ModelParameters,modelUniverse:Universe) {
    val workers = Seq.tabulate(numWorkers)(i=> new Worker(i,parameters))
    val shifts = Seq.tabulate(numDays, numShifts)((day, shift) => new Shift(workers, day, shift))


    def sumQuality(): Element[Double] = Container(shifts
      .flatten // from sequence[][] to sequence[]
      .map(_.sumQuality()): _*) // create sequence[] of sums
      .foldLeft(0.0)(_ + _) // create global sum of qualities of a week
  }

  def main(args: Array[String]) {
    val modelUniverse = new Universe
    val parameters = ModelParameters()
    val week = new Week(parameters,modelUniverse)
    week.sumQuality().addCondition(_ > 2.0)
    week.shifts(1)(1).observeWorkerAssign(Seq(0, 1, 2))
    val alg = Importance(10000, week.shifts(1)(1).qualifications.elements: _*)
    alg.start()
    val test = alg.probability(week.shifts(1)(1).qualifications(1), (x: Double) => x > 0.1)
    println(test)

  }
}