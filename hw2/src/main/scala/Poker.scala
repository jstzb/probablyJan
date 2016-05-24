/**
  * Created by jan on 08.05.16.
  */
/*
 * a Card has a suit: c for club,d for diamond,h for hearts,s for spades
 * and a rank: begins at 2, 11 for jack, 12 for queen, 13 for king, 14 for ace
 */
case class Card(suit:Char,rank:Int)

class Poker() {
  var deck:Set[Card] = getAllCards()
  var hand:Set[Card] = Set[Card]()
  var comm:Set[Card] = Set[Card]()
  var players:Int = 4
  val hasCard = {
    for{card <- deck}yield {

    }
  }

  def getAllCards(): Set[Card] ={
    val suits = List('c','d','h','s')
    val ranks = List(2,3,4,5,6,7,8,9,10,11,12,13,14)
    var cards = Set[Card]()
    for(suit <- suits){
      for(rank <- ranks){
        cards += Card(suit,rank)
      }
    }
    cards
  }

  def eval(cards:Set[Card]) : Int = {
    val ranks = cards.map(card => card.rank).toList.sorted
    val maxRank = ranks.last
    val duplicate = dupl(cards)
    val maxDupl = dupl(cards).map(card => card.rank).max
    val double = if (duplicate.size == 2) maxDupl+14 else 0
    val triple = if (duplicate.size == 3) maxDupl+42 else 0
    val quadruple = if (duplicate.size == 4) maxDupl+84 else 0
    val street = if (str(cards).size > 4) str(cards).map(c=>c.rank).max+56 else 0
    val fullHouse = 0

    val numCards = cards.size

    if(numCards<5) sys.error("At least 5 Cards needed for evaluation")
    List(double,triple,quadruple,street,fullHouse).max
  }

  def dupl(cards:Set[Card]) : Set[Card] = {
    val pn:PartialFunction [(Int,Set[Card]),Set[Card]] = {
      case(_,cards) => if (cards.size>1) cards else Set[Card]()
    }
    val map = cards.groupBy(c => c.rank).collect(pn)
    map.flatten.toSet
  }
  /*
   * hier fehlt noch ein filter in der map der alle nicht aufeinanderfolgenden karten rausnimmt
   */
  def str(cards:Set[Card]) : Set[Card] = {
    val pn:PartialFunction [(Char,Set[Card]),Set[Card]] = {
      case(_,cards) => if (cards.size>4) cards else Set[Card]()
    }
    val map = cards.groupBy(c => c.suit).collect(pn)
    map.flatten.toSet
  }

  def drawRandom(): Card = {
    val r = scala.util.Random
    var card = new Card('a',0)
    val suits = Array('c','d','h','s')
    while (!deck.contains(card)){
      val suit = suits(r.nextInt(4))
      val rank = 2 + r.nextInt(13)
      card = new Card(suit,rank)
    }
    card
  }

  def drawHand(): Unit = {
    for (i <- 0 to 1){
      val card = drawRandom()
      hand += card
      deck -= card
    }
  }

  def drawFlop(): Unit = {
    for ( i <- 0 to 2){
      val card = drawRandom()
      comm += card
      deck -= card
    }
  }

  def drawTurn(): Unit = {
    val card = drawRandom()
    comm += card
    deck -= card
  }

  def drawRiver(): Unit = {
    drawTurn()
  }
}
object Poker {
  def main(args: Array[String]) {
    val poker = new Poker()
    poker.deck=poker.getAllCards()
    poker.hand=Set[Card]()
    poker.comm=Set[Card]()
    poker.drawHand()
    println("Handcards drawn.")
    poker.drawFlop()
    println("Flop shown.")
    poker.drawTurn()
    println("Turn shown.")
    poker.drawRiver()
    println("River shown.")
    println("Your score is " + poker.eval(poker.hand++poker.comm))
  }
}


