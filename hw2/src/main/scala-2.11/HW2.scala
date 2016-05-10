import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.language.{Chain, Flip}
import com.cra.figaro.library.compound
/**
  * Created by jan on 10.05.16.
  *
  * Exercises 4.7.1 - 4.7.4
  * Probabilistic models and probabilistic programs
  *
  */

/*
  ####Task 1####
  Five Card Poker Game everyone gets a card
    What are possible worlds?
      Answer depends on player and card number
      For 4 Players there would be 5*4*3*2=120 worlds
      Fun Fact: There are 56!/(56-13)! ~ 10^22 possible worlds
      in a Texas Holdem game with 4 players,

    What are the odds for a world assuming discrete uniform distribution?
      1/120 * 120 = 1 => 1/120 per world
  ####Task 2####
    What is possibility second player has spade when first player draws a king or queen?
      If first player has drawn a king or queen. This are 4 possible cards, second player still draw 4 cards.
      This sums up to 16 possible worlds. Assuming A,Q,K are the cards of spades and Q',K' are the cards of hearts.
      , following combinations dont fit our query: KQ',QQ',K'Q',QK',KK',Q'K'. This leaves 10 worlds fitting our query.
      => P = 10/16
  ####Task 3####
    What are the variables?
      mood of player 1,
      mood of player 2,
      card of player 1,
      card of player 2,
      first bet or pass of player 1,
      first bet or fold or pass of player 2,
      nth-bet or fold of player 1,(requires n-1 bets of player 2)
      nth-bet or fold of player 2 (requires n bets of player 1)
    What are the dependencies?
      since we have a set of cards we draw from and all cards are unique, p2's card depends on p1's card
      p1's bet or pass depends on his mood and on his card.
      p2's bet or fold or pass depends on his mood, his card and p1's action.
      p1's n-th bet or fold directly depends on p2's n-1-th bet or fold,
       his mood and his card,
       indirectly it depends on all actions happening before
    What are functional forms of dependencies?
      The card of p1 has a discrete uniform distribution
      The card of p2 has a discrete uniform distribution on remaining cards.
      The mood of p1,p2 may have a conditional distribution on the previous cards.
      The bet or fold of p1 has a conditional distribution.
      All following bet, fold or passes have conditional distributions
    What do we know apriori and what do we learn?
      We have to learn about the mood and the actions of players to reason about it.
      We know the parameters for the cards and can recommend when to bet.
 */

object HW2 {
  case class Card(suit:Char,rank:Int)
  def main(args: Array[String]) {
    val cards = List(1,2,3,4,5)
    val moodP1= Flip(0.6)
    val moodP2= Flip(0.5)
    val cardP1=discrete.Uniform(cards)
    val cardP2=Chain(cardP1,(card:Int) => discrete.Uniform(cards.filter(_ != card)))
    val betP1=Chain(cardP1,moodP1, (card:Int,goodMood:Boolean) =>
      if (card>3) {
        if (goodMood==true)
          Flip(0.9)
        else Flip(0.7)} else
        if (goodMood==true) Flip(0.4)
      else Flip(0.2))
    val betP2=Chain(cardP2,moodP2, (card:Int,goodMood:Boolean) =>
      if (card>3) {
        if (goodMood==true)
          Flip(0.8)
        else Flip(0.7)} else
      if (goodMood==true) Flip(0.3)
      else Flip(0.2))
  }
}
