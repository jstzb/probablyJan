import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.Apply
import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.library.compound.^^
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

    /**
      *  INPUT variables start with _
      */
    val _handsize = 5 // runs out of mem at higher handsize than 5
    val _cards = 52
    val _ranks = 12
    val _suits = 4

    /**
      * Card and Combocollection starts here
      */

    var cards = List.range(1,_cards)

    // number of possible hands 52!/45!
    val hands = cards.toSet.subsets(_handsize)

    /* performance issues are hard*/

    /* this loop will run out of memory during compilation

      for(fir <- 1 to csize;
          sec <- fir+1 to csize;
          thr <- sec+1 to csize;
          frt <- thr+1 to csize;
          fiv <- frt+1 to csize;
          six <- fiv+1 to csize;
          sev <- six+1 to csize
          ) yield {
        Set(fir,sec,thr,frt,fiv,six,sev)
      }
    */

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

    // creates Map( Combo -> List[Set[Int]])
    val combos = hands.toList.groupBy(applyCombos(_))

    /**
      * FIGARO usage starts here:
      */

    // assigns uniform distr to variable into an hand sized array
    val cardV=Array.fill(_handsize)(discrete.Uniform(cards: _*))

    // finds all possible combinations of 2 variables in array
    val tuples = for (
      i <- 0 to _handsize-2;
      j <- i+1 to _handsize-1)
      yield {(cardV(i),cardV(j))}

    // checks if values in a pair differ
    def unique(pair : (Int,Int)) =
      if (pair._1 == pair._2) 0.0; else 1.0

    // Constraint declaration of Markov network
    for((p1,p2) <- tuples){^^(p1,p2).setConstraint(unique)}

    for(i <- 0 to _handsize-2) {cardV(i).observe(i+1)}

    val quadChance = Apply(cardV(0),cardV(1),cardV(2),cardV(3),cardV(4),
      (c1:Int,c2:Int,c3:Int,c4:Int,c5:Int) => combos("Quad").contains(Set(c1,c2,c3,c4,c5)))

    val imp =Importance(10000,cardV(_handsize-1))
    imp.start()


    /**
      * OUTPUTS start here
      */




    //println(hands.size.toString)

    val hline = "###############################"

    /*
    println(hline)
    println("Triples are ")
    for(c <- combos("Trip")){println(c.toString())}
     */
    println(hline)

    println("QuadCombos are ")
    for(c <- combos("Quad")){println(c.toString())}

    println(hline)

    println("There are "  + pairs.size.toString + " possible Pairs")
    println("There are " + triples.size.toString + " possible Triples")
    println("There are " + quadruples.size.toString + " possible Quadruples")

    println(hline)

    println("When observing that Ints 1 to 6 have been drawn:")

    println("Probability to draw 1 again is: " + imp.probability(cardV(_handsize-1),1))
    println("Probability to draw 14 again is: " + imp.probability(cardV(_handsize-1),14))

    println(hline)

    //for(i<-0 to _handsize-2) {cardV(i).unobserve()}
    cardV(0).observe(1)
    cardV(1).observe(13)
    cardV(2).observe(25)

    println("When observing that Ints 1,13,25 have been drawn:")
    println("Probability to draw 13 again is: " + imp.probability(cardV(_handsize-1),13))
    println("Probability to win by drawing an Quadruple is: " + imp.probability(cardV(_handsize-1),37))

    println(hline)
    imp.stop()
    cardV(1).unobserve()
    cardV(2).unobserve()
    val imp1= Importance(10000,quadChance)
    imp1.start()
    println("Probability to get a quadruple is " + imp1.probability(quadChance,true))

  }
}
