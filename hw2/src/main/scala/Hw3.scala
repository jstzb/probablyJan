import com.cra.figaro.language.Chain
import com.cra.figaro.library.atomic.discrete
/**
  * Created by jan on 17.05.16.
  */
object Hw3 {
  def main(args: Array[String]) {
    var cards = List.range(1,52)
    val card1 = discrete.Uniform(cards: _ *)
    cards = cards.filter(_!=card1.value)
    val card2 = Chain(card1,c => discrete.Uniform(cards: _ *))
    cards = cards.filter(_!=card2.value)
    val card3 = Chain(card1,c => discrete.Uniform(cards: _ *))
    cards = cards.filter(_!=card3.value)
    val card4 = Chain(card1,c => discrete.Uniform(cards: _ *))
    cards = cards.filter(_!=card4.value)
    val card5 = Chain(card1,c => discrete.Uniform(cards: _ *))
    cards = cards.filter(_!=card5.value)
    val card6 = Chain(card1,c => discrete.Uniform(cards: _ *))
    cards = cards.filter(_!=card6.value)
    val card7 = Chain(card1,c => discrete.Uniform(cards: _ *))
  }
}
