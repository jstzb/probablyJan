import com.cra.figaro.language.{Flip, Select}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

object Main extends App {
  val morning = Flip(0.4)
  val greeting = If(morning,
    Select(0.1 -> "Good Morning!",0.9 -> "Where is my coffee?"),
    Select(0.5 -> "Hi there!", 0.5 -> "Where is my coffee?"))

  def predict(): Unit ={
    val result = VariableElimination.probability(greeting, "Where is my coffee?")
    println("Your greeting will be \"Where is my coffee?\" " +
    "with probability " + result + ".")
  }
  predict()
}