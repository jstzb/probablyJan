import java.time.LocalDate

import com.cra.figaro.algorithm.factored.VariableElimination
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

  val defaultQuality: Element[Double] = Beta(1, 1)
  val defaultTime: Element[Boolean] = Flip(0.5)

  val maxShifts: Int = 2
  val trainSize: Int = 5



  class Worker(id: Int) {
    val taskQuality = new FixedSizeArray[Int](numTasks, i => Uniform(0,100))
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
    // val index = generateIndexWoDup(numTasks)
    val sumQuality = qualifications.foldLeft(0)(_+_)
    // new FixedSizeArray[Int](numTasks, i => workers(index(i).value).taskQuality(i))
    val qualifications = new FixedSizeArray[Int](numTasks, i => workers(i).taskQuality(i))
    //val qualifications = index.chain(i => chainQual(i))

    def generateIndexWoDup(num: Int): FixedSizeArray[Int] = {
      var duplicates = List[Int]()
      def chainIndex(num: Int): Element[Int] = {
        val values: Seq[Int] = 0 until numWorkers

        duplicates = num :: duplicates
        Uniform(values
          .filterNot(duplicates.contains(_)) // ensures there are no duplicates
          .filterNot(workers(_).hasTime(day, shift)) // ensures workers has time
          : _*)
      }
      new FixedSizeArray[Int](num,(i:Int) => Chain(Uniform(0, numWorkers), chainIndex))
    }


    def chainQual(num:Int):Element[Int] = {
      val res = workers(num).taskQuality(j)
      j+=1
      res
    }

    def getSumQuality():Element[Int] = qualifications.foldLeft(0)(_ + _)

  }

  /**
    * Week(end) represents an assignment of possible workers to a each task in a
    * week
    *
    */

  class Week() {
    val workers = Seq.tabulate(numWorkers)(i=> new Worker(i))
    val shifts = Seq.tabulate(numDays, numShifts)((day, shift) => new Shift(workers, day, shift))


    /*def sumQuality(): Element[Double] = Container(shifts
      .flatten // from sequence[][] to sequence[]
      .map(_.sumQuality: _*) // create sequence[] of sums
      .foldLeft(0.0)(_ + _) // create global sum of qualities of a week*/
  }

  def main(args: Array[String]) {
    Universe.createNew()
    val workers = Seq.tabulate(numWorkers)(i=> new Worker(i))
    for(i<- 0 until trainSize) {
      val shift = new Shift(workers,0,0)
      shift.sumQuality.observe(100)
      //shift.sumQuality.observe(0.6)
    }
    val futureShift = new Shift(workers,0,0)
    println(Universe.universe.activeElements.toString())
    val alg = Importance(1000,futureShift.qualifications.elements: _*)
    println("Starting sampling")
    alg.start()
    val test = alg.probability(futureShift.qualifications(0),(i:Int) => i < 10)
    println(test)
    println(VariableElimination.probability(futureShift.qualifications(0),75))

  }
}