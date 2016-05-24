import com.cra.figaro.language.Element
/**
  * Created by jan on 09.05.16.
  */
abstract class Model {
  val isWinning : Element[Boolean]

  val hasHand : Array[(Card,Element[Boolean])]

  val hasComm : Array[(Card,Element[Boolean])]

  val players : Element[Int]
}

class ReasoningModel extends Model {
  val hasHand = Array
}

object Model {
  val binomialNumTrials = 42
}