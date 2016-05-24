import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.Chain
import com.cra.figaro.library.atomic.discrete
/**
  * Created by jan on 17.05.16.
  */
object Hw3 {
  //http://stackoverflow.com/questions/11581175/how-to-generate-the-power-set-of-a-set-in-scala
  def power[A](s:List[A]): List[List[A]] = {
    @annotation.tailrec
    def pwr(s:List[A],acc : List[List[A]]): List[List[A]] = s match {
      case Nil => acc
      case a :: as => pwr(as, acc ::: (acc map (a :: _)))
    }
    pwr(s, Nil :: Nil)
  }
  def main(args: Array[String]) {

    /* Input */
    val _handsize = 5 // runs out of mem at higher handsize
    val _cards = 52
    val _ranks = 12
    val _suits = 4

    var cards = List.range(1,_cards)

    // number of possible hands 52!/45!
    val hands = cards.toSet.subsets(_handsize)

    // number of tuples   3*52*50*49*48*47*46
    val pairs = for(
      i <- 0 to _suits-2;
      j <- i + 1 to _suits-1;
      r <- 1 to _ranks) yield {
      Set(r+i*_ranks,r+j*_ranks)
    }

    // number of triples   3*2*52*50*49*48*47
    val triples = for(
      i <- 0 to _suits-3;
      j <- i + 1 to _suits-2;
      k <- j+1 to _suits-1;
      r <- 1 to _ranks) yield {
      Set(r+i*_ranks,r+j*_ranks,k+j*_ranks)
    }

    // number of quadruples 3*2*1*52*50*49*48
    val quadruples = for(r <- 1 to _ranks) yield {
      Set[Int](r,r+_ranks,r+2*_ranks, r + 3 * _ranks)
    }

    // assigns combos to set of possible hands
    def applyCombos(s:Set[Int]): String = {
      if (quadruples.filter(_ subsetOf(s)).nonEmpty){"Quad"}
      else if (triples.filter(_ subsetOf(s)).nonEmpty){"Trip"}
      else if (pairs.filter(_ subsetOf(s)).nonEmpty){"Pair"}
      else {"None"}
    }


    val combos = hands.toList.groupBy(applyCombos(_))


    /* performance issues are hard*/

    /* this loop will run out of memory during compilation
      for(fir <- 1 to csize;
          sec <- fir to csize;
          thr <- sec to csize;
          frt <- thr to csize;
          fiv <- frt to csize;
          six <- fiv to csize;
          sev <- six to csize
          ) yield {
        Set(fir,sec,thr,frt,fiv,six,sev)
      }
    */

    val card1 = discrete.Uniform(cards: _ *)
    cards = cards.filter(_!=card1.value)
    val card2=Chain(card1,(c:Int)=> discrete.Uniform(cards: _ *))
    cards = cards.filter(_!=card2.value)
    val card3 = Chain(card1,(c:Int) => discrete.Uniform(cards: _ *))
    cards = cards.filter(_!=card3.value)
    val card4 = Chain(card1,(c:Int) => discrete.Uniform(cards: _ *))
    cards = cards.filter(_!=card4.value)
    val card5 = Chain(card1,(c:Int) => discrete.Uniform(cards: _ *))
    cards = cards.filter(_!=card5.value)
    val card6 = Chain(card1,(c:Int) => discrete.Uniform(cards: _ *))
    cards = cards.filter(_!=card6.value)
    val card7 = Chain(card1,(c:Int) => discrete.Uniform(cards: _ *))
    //println(hands.size.toString)
    card1.observe(1)
    val hline = "###############################"

    /* Output */
    println(hline)
    println("Triples are ")
    for(c <- combos("Trip")){println(c.toString())}
    println(hline)
    println("QuadCombos are ")
    for(c <- combos("Quad")){println(c.toString())}
    println(hline)

    println("There are "  + pairs.size.toString + " possible Pairs")
    println("There are " + triples.size.toString + " possible Triples")
    println("There are " + quadruples.size.toString + " possible Quadruples")
    println(hline)
    // println(VariableElimination.probability(card2,1).toString)
  }
}
