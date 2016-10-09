import java.io._

import breeze.optimize.linear._

import scala.collection.immutable.{IndexedSeq, TreeMap}
/**
  * Created by jan on 20.08.16.
  */
class Recommender (week: Week) {

  def partialSumConstraints[A, B](lp: LinearProgram)(list: IndexedSeq[(lp.Variable, List[A])])(f: List[A] => B)(g: (lp.Expression, B) => lp.Constraint): Iterable[lp.Constraint] = {
    val grouped: Map[B, IndexedSeq[(lp.Variable, List[A])]] = list.groupBy(x => f(x._2))
    var resultingConstraints = List.empty[lp.Constraint]
    for (elem <- grouped) {
      val expression = elem._2.map(y => y._1).reduce[lp.Expression](_ + _)
      resultingConstraints = g(expression, elem._1) :: resultingConstraints
    }
    resultingConstraints
  }

  def recommendWeek()={
  import week._
  val maxShifts = (for(i <- 0 until numWorkers) yield maxShift(names(i))).sum
  val lp = new LinearProgram()
  import lp._
  val xs = for(w<-0 until numWorkers; d<-0 until numDays; s<-0 until numShifts; t<-0 until numTasks) yield (w,d,s,t)
  val x = xs.filter(x=>timeMap((names(x._1),x._2,x._3))>0)
      .filter(x1=>taskMap(x1._2,x1._3)>x1._4)
      .map(z=>(z._1,z._2,z._3,z._4,Binary(),tq(names(z._1))(z._4),shiftleader(names(z._1))))
     // {if(timeMap((names(w),d,s))>0) {x = x :: (w,d,s,t,Integer(),tq(names(w))(t))}}
     // x.groupBy(x => (x._1,x._2+x._3)).filter(_._2.length==numShifts*numTasks).map(x => (x._1,x._2.map(y => (days(y._2),shifts(y._3),tasks(y._4),names(y._1))))).foreach(_._2.foreach(println))
    val x1 = x.partition(y => maxShift(names(y._1))==5)
  val lpp = ((x.map(x=>x._5*x._6).reduce[Expression](_+_))
    subjectTo(x.map(_._5).map(_ >= 0):_*)
    //subjectTo(x.groupBy(x=>(x._2,x._3)).values.map(y =>y.filter(_._7>0).map(z => z._5).reduce[Expression](_+_)>= 1).toSeq:_*)
    subjectTo(x1._2.groupBy(_._1).values.map(y => y.map(_._5).reduce[Expression](_+_)<=maxShift(names(y.head._1))).toSeq:_*)
    subjectTo(x.groupBy(x=>(x._2,x._3)).values.map(y=> y.map(_._5).reduce[Expression](_+_)<=taskMap(y.head._2,y.head._3)).toSeq:_*)
    subjectTo(x.groupBy(x=>(x._2,x._3,x._4)).values.map(_.map(_._5).reduce[Expression](_+_)<=1).toSeq:_*)
    subjectTo(x.groupBy(x=>(x._1,x._2)).values.map(_.map(_._5).reduce[Expression](_+_)<=1).toSeq:_*)
    subjectTo(x.groupBy(x=>(x._1,x._2+x._3)).values.filter(_.length==numShifts*numTasks).map(_.map(_._5).reduce[Expression](_+_)<=1).toSeq:_*)
    )

    val result = maximize(lpp)
    //println(result)
    val y = result.result.toArray.zipWithIndex.filter(_._1>0)
    //y.foreach(println)
    val assign = y.map(z=> x.apply(z._2)).map(x => (x._2,x._3,x._4,x._1)).sorted.map(x => (days(x._1),shifts(x._2),tasks(x._3),names(x._4)))
    Model.underline()
    println("Result")
    assign.foreach(println)
    Model.underline()
    println("Maximized linear program with " + result.result.length + " variables, included " + result.problem.constraints.length + " constraints.")

    println("Max Worker assigned " + maxShifts)
    println("Total tasks " + numDays*numTasks*numShifts)
    println("Assigned tasks " + y.map(_._1).sum)
    Model.underline()
    assign
  }
  def writeFile(destination: String): Unit = {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(new File(destination))))
    val scheduleGroupedByShift= recommendWeek().toList.groupBy(x => (x._1,x._2)).map(x => (x._1,x._2.map(y=>(y._4))))
    val sortedSchedule = TreeMap(scheduleGroupedByShift.toSeq:_*)(shiftOrdering)
    Model.underline()
    println("Writer output")
    sortedSchedule.foreach(println)
    Model.underline()
    writer.println("Tag;Schicht;Buerger;Herd;Fritte;Saladette;Spuele")
    for (key <- sortedSchedule.keys){
      println(key)
      val value = sortedSchedule(key).reduce(_+ ";" + _)
      writer.append(key._1 + ";" + key._2 + ";" + value + System.getProperty( "line.separator" ))
    }
    writer.close()
  }
  object shiftOrdering extends Ordering[(String,String)]{
    val dayMap=Map(
      (("Mo","Frueh"),15),
      (("Mo","Spaet"),14),
      (("Di","Frueh"),13),
      (("Di","Spaet"),12),
      (("Mi","Frueh"),11),
      (("Mi","Spaet"),10),
      (("Do","Frueh"),9),
      (("Do","Spaet"),8),
      (("Fr","Frueh"),7),
      (("Fr","Spaet"),6),
      (("Sa","Frueh"),5),
      (("Sa","Spaet"),4),
      (("So","Frueh"),3),
      (("So","Spaet"),2)
    ).withDefaultValue(1)
    override def compare(x: (String,String), y: (String,String)): Int = dayMap(y) compare dayMap(x)
  }



}

