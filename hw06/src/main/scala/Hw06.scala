/**
  * Created by jan on 20.06.16.
  *
  * Let’s create a model of the success of a new song in the pop charts. As a song is
  * released, more and more people become exposed to it, so it grows in popular-
  * ity. At some point, most of the people who would be interested in the song have
  * already been exposed to it, so fewer buy, and it goes down the charts.
  * We’ll create a model with five variables.
  * Quality represents the overall quality of the song;
  * Newly Exposed is the number of people who first hear the song in a given week; Total Exposed is the total number of people who have heard the
  * song;
  * Newly Bought is the number of people who buy the song in a given week;
  * and
  * Total Bought is the total number of people who have bought the song so
  * far.
  * The Quality doesn’t change over time, while the Total Bought is the previ-
  * ous Total Bought plus the Newly Bought, and similarly for Total Exposed. Newly
  * Bought depends on the Quality and Newly Exposed; the more people are newly
  * exposed, the more will buy the song, but this depends also on the song quality.
  * Finally, Newly Exposed depends on both Total Exposed and Newly Bought; the
  * more people have already heard the song, the fewer there are to hear it new; on
  * the other hand, the more people who buy the song in a given week, the more
  * air time it gets on the radio, so the more people are exposed to it.
  * Because there’s no time limit on the amount of time a song stays in the
  * charts, we have to use Figaro universes to create an ongoing model. Newly
  * Bought is an observed variable. We’ll query Total Exposed. Write a Figaro pro-
  * gram to express this model.
  * a) Generate data from this model. Use particle filtering, with no evidence, to
  * create a sequence of numbers of Newly Bought. Stop when Newly Bought
  * falls below a certain threshold (corresponding to the song falling off the
  * charts).
  * b) Now, using your generated data, observe Newly Bought and estimate the
  * Total Exposed over time.
  */

import com.cra.figaro.algorithm.filtering.ParticleFilter
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.library.compound.{^^}

object Hw06 {
  var quality:Double=0.7
  var potentialListener:Double=10000

  val initial:Universe = Universe.createNew()
  val newlyBought:Element[Int]=Poisson(quality*potentialListener)("newlyBought",initial)
  val totalBought:Element[Int]=Constant(0)("totalBought",initial)
  val newlyExposed:Element[Int]=Poisson(quality*potentialListener)("newlyExposed",initial)
  val totalExposed:Element[Int]=Constant(0)("totalExposed",initial)

  def transition(tbought:Int,nbought:Int,texposed:Int,nexposed:Int):(Element[(Int,Int,Int,Int)])= {
    val totalBought:Element[Int] = Constant(tbought+nbought)
    val newlyBought:Element[Int] = Poisson(quality * nexposed)
    val totalExposed:Element[Int] = Constant(texposed+nexposed)
    val newlyExposed:Element[Int] = Poisson(potentialListener*quality-texposed+nbought)
    ^^(totalBought,newlyBought,totalExposed,newlyExposed)
  }

  def nextUniverse(previous: Universe) : Universe = {
    val next = Universe.createNew()
    val previousBought=previous.get[Int]("totalBought")
    val previousNewlyBought=previous.get[Int]("newlyBought")
    val previousExposed=previous.get[Int]("totalExposed")
    val previousNewlyExposed=previous.get[Int]("newlyExposed")
    val tuple4 = ^^(previousBought,previousNewlyBought,previousExposed,previousNewlyExposed)
    val state = Chain(tuple4, (s:(Int,Int,Int,Int)) => transition(s._1,s._2,s._3,s._4))
    Apply(state, (s:(Int,Int,Int,Int)) => s._1)("totalBought",next)
    Apply(state, (s:(Int,Int,Int,Int)) => s._2)("newlyBought",next)
    Apply(state, (s:(Int,Int,Int,Int)) => s._3)("totalExposed",next)
    Apply(state, (s:(Int,Int,Int,Int)) => s._4)("newlyExposed",next)
    next
  }


  def main(args: Array[String]) {
    /*
     * Handling input
     */
    args match {
      case Array("a", d1, d2) =>
        println("Running task a ...")
        println("Setting song quality to " + args(1))
        println("Setting potential listeners to " + args(2))
        noEv(args(1).toDouble, args(2).toDouble)
      case Array("a", d1) =>
        println("Running task a ...")
        println("Setting song quality to " + args(1))
        noEv(args(1).toDouble, potentialListener)
      case Array("b") =>
        println("Running task b ...")
      case _ =>
        println("No input or input not recognized.")
        println("You may chose song quality as a double in [0,1]")
        println("and potential listeners as a int >0.")
        println("by typing 'run quality potentialListener'")
        println("Running task a ...")
        noEv(quality, potentialListener)
    }
    /*
     * Task a) Running without evidence
     */
    def noEv(quality: Double, potentialListener: Double) = {
      println("Starting ParticleFilter")
      println("song quality is " + quality)
      println("potential listeners are " + potentialListener)
      val alg = ParticleFilter(initial, nextUniverse, 10000)
      alg.start()
      while (alg.currentUniverse.get[Int]("newlyBought").generateValue() > 100) {
        println("######################")
        println("newlyBought" + alg.currentUniverse.get[Int]("newlyBought").generateValue())
        println("totalBought" + alg.currentUniverse.get[Int]("totalBought").generateValue())
        println("newlyExposed" + alg.currentUniverse.get[Int]("newlyExposed").generateValue())
        println("totalExposed" + alg.currentUniverse.get[Int]("totalExposed").generateValue())
        alg.advanceTime()
      }
      alg.stop()
    }
    /*
     * Task b) Running with evidence
     */
    def withEv(quality:Double, potentialListener: Double) = {
      val alg = ParticleFilter(initial,nextUniverse, 10000)
      val evidence = NamedEvidence("newlyBought",Observation(120))
      alg.start()
      while (alg.currentUniverse.get[Int]("newlyBought").generateValue() > 100){
        println("newlyBought" + alg.currentUniverse.get[Int]("newlyBought").generateValue())
        println("totalBought" + alg.currentUniverse.get[Int]("totalBought").generateValue())
        println("newlyExposed" + alg.currentUniverse.get[Int]("newlyExposed").generateValue())
        println("totalExposed" + alg.currentUniverse.get[Int]("totalExposed").generateValue())
        alg.advanceTime(Seq(evidence))
      }
      alg.stop()
    }
  }
}
import Hw06._

