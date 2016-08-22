import java.io._

import breeze.optimize.linear._
/**
  * Created by jan on 20.08.16.
  */
class Recommender (week: Week) {

  var tq = Map(
    ("Anny",List(2,1,1,1,1)),
    ("Bob",List(1,2,1,1,1)),
    ("Cindy",List(1,1,2,1,1)),
    ("Dex",List(1,1,1,2,1)),
    ("Eddy",List(1,1,1,1,2)),
    ("David",List(9,5,3,3,3)),
    ("Chris",List(9,7,5,6,9)),
    ("Maria",List(9,6,5,8,8)),
    ("Maike",List(7,9,9,6,6)),
    ("Sonja",List(6,8,8,5,3)),
    ("Alex",List(9,6,7,4,4)),
    ("Peter",List(7,8,7,4,5)),
    ("Moritz",List(8,8,8,6,6)),
    ("Josef",List(2,1,5,3,6)),
    ("Benni",List(3,1,1,1,1)),
    ("Test",List(1,1,1,1,1))
  )

  def readQualificationsFromFile(source:String)={
    val bufferedSource = io.Source.fromFile(source)
    val lines = bufferedSource.getLines().toList
    var key = ""
    for(i<- 1 until lines.size-1){
      val cols = lines(i).split(";")
      key = cols(0)
      val value = cols.toList.slice(1,cols.size-1).map(_.toInt)
      tq = tq.updated(key,value)
    }
    bufferedSource.close()
    println("############################")
    println("Taskqualification:")
    tq.foreach(println)
    println("############################")
  }


  def recommendWeek()={
  import week._
  val maxShifts = (for(i <- 0 until numWorkers) yield maxShift(names(i))).sum
  val lp = new LinearProgram()
  import lp._
  val x = for(w<-0 until numWorkers; d<-0 until numDays; s<-0 until numShifts; t<-0 until numTasks) yield (w,d,s,t,Integer(),tq(names(w))(t))
  //x.groupBy(x => (x._1,x._2+x._3)).filter(_._2.length==numShifts*numTasks).map(x => (x._1,x._2.map(y => (days(y._2),shifts(y._3),tasks(y._4),names(y._1))))).foreach(_._2.foreach(println))
  val lpp = ((x.map(x=>x._5*x._6).reduce[Expression](_+_))
    subjectTo(x.map(_._5).map(_ >= 0):_*)
    subjectTo(x.groupBy(_._1).values.map(y => y.map(_._5).reduce[Expression](_+_)<=maxShift(names(y.head._1))).toSeq:_*)
    subjectTo(x.groupBy(x=>(x._2,x._3)).values.map(_.map(_._5).reduce[Expression](_+_)<=numTasks).toSeq:_*)
    subjectTo(x.groupBy(x=>(x._2,x._3,x._4)).values.map(_.map(_._5).reduce[Expression](_+_)<=1).toSeq:_*)
    subjectTo(x.groupBy(x=>(x._2,x._3,x._4)).values.map(_.map(_._5).reduce[Expression](_+_)>=1).toSeq:_*)
    //subjectTo(x.groupBy(x=>(x._1,x._2)).values.map(_.map(_._5).reduce[Expression](_+_)<=1).toSeq:_*)
    subjectTo(x.groupBy(x=>(x._1,x._2+x._3)).values.filter(_.length==numShifts*numTasks).map(_.map(_._5).reduce[Expression](_+_)<=1).toSeq:_*)
    subjectTo(x.groupBy(x=>(x._1,x._2,x._3)).values.map(y =>y.map(_._5).reduce[Expression](_+_)<= timeMap((names(y.head._1),y.head._2,y.head._3))).toSeq:_*)
    )

    val result = maximize(lpp)
    //println(result)
    val y = result.result.toArray.zipWithIndex.filter(_._1>0)
    //y.foreach(println)
    val assign = y.map(z=> x.apply(z._2)).map(x => (x._2,x._3,x._4,x._1)).sorted.map(x => (days(x._1),shifts(x._2),tasks(x._3),names(x._4)))
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
    val scheduleGroupedByShift: Map[(String, String), List[String]] = recommendWeek().toList.groupBy(x => (x._1,x._2)).map(x => (x._1,x._2.map(y=>(y._4))))
    scheduleGroupedByShift.foreach(println)
    writer.println("Tag;Schicht;Buerger;Herd;Fritte;Saladette;Spuele")
    for (key <- scheduleGroupedByShift.keys){
      val value = scheduleGroupedByShift(key).reduce(_+ ";" + _)
      writer.append(key._1 + ";" + key._2 + ";" + value + System.getProperty( "line.separator" ))
    }
    writer.close()
  }

}
