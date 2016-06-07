import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.{Beta, Normal}


/**
  * Created by jan on 05.06.16.
  *
  * Consider a vehicle surveillance application. Multiple kinds of vehicles exist,
  * such as truck, car, and motorcycle. Each vehicle has properties such as size and
  * color. The distribution over these variables is different for each kind of vehicle.
  * You have a camera that takes pictures of vehicles entering your property and an
  * image analysis algorithm that estimates the size and color of the vehicle. The
  * estimated size and color are dependent on, but not necessarily identical to, the
  * true size and color. Use a relational model with type uncertainty to represent
  * this situation. Using this model, infer the type of a given vehicle based on its
  * estimated size and color.
  */

object Hw5 {
  abstract class Vehicle extends ElementCollection {
    val size: Element[Double]
    val color: Element[String]
   }

  class Motorcycle extends Vehicle {
    val size = Normal(3,0.5)("size",this)
    val color = Select(0.25 -> "black",0.1 -> "blue", 0.1 -> "red", 0.01 -> "green", 0.001 -> "pink")("color",this)
  }

  class Car extends Vehicle {
    val size = Normal(2,0.1)("size",this)
    val color = Select(0.5 -> "black", 0.25 -> "blue", 0.25 -> "red")("color",this)
  }

  class Truck extends Vehicle {
    val size = Normal(8,2)("size",this)
    val color = Select(0.1 -> "black",0.1 -> "blue", 0.1 -> "red", 0.01 -> "green", 0.001 -> "pink")("color",this)
  }

  class Picture() extends ElementCollection {
    val vehicle = Select(
      0.1 -> new Motorcycle,
      0.8 -> new Car,
      0.1 -> new Truck)("vehicle",this)
    val light = Normal(0,0.25)
  }

  class PicAnalyzer(val pic: Picture) {
    val estSize = Normal(pic.get[Double]("vehicle.size"),pic.light)
    val estColor = Select(0.99 -> pic.get[String]("vehicle.color"),0.01 -> "black")
    val isMotorcycle = Apply(pic.vehicle,(v:Vehicle) => v.isInstanceOf[Motorcycle])
    val isCar = Apply(pic.vehicle,(v:Vehicle) => v.isInstanceOf[Car])
    val isTruck = Apply(pic.vehicle,(v:Vehicle) => v.isInstanceOf[Truck])
  }

  def main(args: Array[String]) {
    val pic = new Picture
    val picAnalyzer = new PicAnalyzer(pic)
    val algo = Importance(1000,picAnalyzer.estSize,picAnalyzer.estColor,picAnalyzer.isMotorcycle,picAnalyzer.isCar,picAnalyzer.isTruck)
    algo.start()
    if(args.length==2){
      println("############################################")
      println("# Input accepted")
      picAnalyzer.estSize.observe(args(0).toDouble)
      picAnalyzer.estColor.observe(args(1))
      println("# Found estimated size= " +args(0) + " and color '" + args(1)+"'")
      println("############################################")
    } else {
      println("#########################################################")
      println("# Observing estimated size of 2.9m and a color of black #")
      println("#########################################################")
      picAnalyzer.estSize.observe(2.9)
      picAnalyzer.estColor.observe("black")
    }
    println("Probability of picture of a motorcycle is:")
    println(algo.probability(picAnalyzer.isMotorcycle,true))
    println("Probability of picture of a car is:")
    println(algo.probability(picAnalyzer.isCar,true))
    println("Probability of picture of a truck is:")
    println(algo.probability(picAnalyzer.isTruck,true))
    algo.stop()
  }
}
