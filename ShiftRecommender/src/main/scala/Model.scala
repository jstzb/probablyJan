import java.time.LocalDate

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Apply, Element}
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.collection.Container

/**
  * Created by jan on 21.07.16.
  */
object Model{
  class Worker(numTasks:Int) {


  }

class Shift(numTasks:Int) {
  val taskQualifications : Array[Element[Double]]= Array.fill(numTasks)(Beta(1,1))
  val taskQualSum: Element[Double] = Container(taskQualifications: _*).foldLeft(0.0)(_+_)
}

class Week(end:LocalDate)(numDays:Int)(numShifts:Int)(numTasks:Int) {
  val 
}

def main(args: Array[String]) {
  val week = new Week
  week.earlyMonday.taskQualSum.addCondition(_ > 3)
  val alg = Importance(2000,week.earlyMonday.taskQualifications: _*)
  alg.start()
  val test = alg.probability(week.earlyMonday.taskQualifications(4), (x:Double) => x > 0.1)
  println(test)

} }