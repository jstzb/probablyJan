import java.io.File

import scala.collection.mutable

/**
  * Created by jan on 21.08.16.
  */
//def main(args: Array[String]) {
//  val week = new Week()
//  week.readTimetableFromFile("/home/jan/time1.csv")
//  val recommender = new Recommender(week)
//  recommender.readQualificationsFromFile("/home/jan/qual.csv")
//  recommender.writeFile("/home/jan/recom.csv")
//
//}
class Week {
  val numDays: Int = 7
  val numShifts: Int = 2
  val numTasks: Int = 7
  val numWorkers: Int = 28

  /**
    * names maps each workerId to a name
    */
  var names = Map.empty[Int,String]
  var tq = Map.empty[String,List[Int]]
  var maxShift=Map.empty[String,Int]
  var timeMap =Map.empty[(String,Int,Int),Int]
  val taskMap = Map(
    ((0,0),7),
    ((0,1),6),
    ((1,0),7),
    ((1,1),6),
    ((2,0),7),
    ((2,1),6),
    ((3,0),7),
    ((3,1),6),
    ((4,0),7),
    ((4,1),6),
    ((5,0),6),
    ((5,1),6),
    ((6,0),6),
    ((6,1),6)
  )
  var days = Map (
    (0,"Montag"),
    (1,"Dienstag"),
    (2,"Mittwoch"),
    (3,"Donnerstag"),
    (4,"Freitag"),
    (5,"Samstag"),
    (6,"Sonntag"),
    (7,"Ruhetag"),
    (8,"Feiertag")
  )
  var shifts = Map.empty[Int,String]
  val tasks = Map(
    (1,"Buerger"),
    (2,"Herd"),
    (3,"Fritte"),
    (4,"Saladette"),
    (5,"Spuele"),
    (0,"Bons"),
    (6,"Vorbereitung"),
    (7,"Putzen")
  )
  val shiftleader = Map(
    ("Sarah",1),
    ("Chris",1),
    ("Maria",1),
    ("David",1),
    ("Maike",1),
    ("Alex",1),
    ("Sonja",1),
    ("Tarek",1),
    ("Moritz",1),
    ("Tommi",1)
  )   .withDefaultValue(0)

  def readQualificationsFromFile(source:String)={
    val bufferedSource = io.Source.fromFile(source)
    val lines = bufferedSource.getLines().toList
    var key = ""
    for(i<- 1 until lines.size-1){
      val cols = lines(i).split(",")
      key = cols(0)
      val value = cols.toList.slice(1,cols.size-1).map(_.toInt)
      names+=(i-1 -> key)
      tq +=(key -> value)
    }
    bufferedSource.close()
    println("############################")
    println("Taskqualification:")
    tq.foreach(println)
    println("############################")
  }

  def readTimetableFromFile(source: String,numShifts:Int) ={

    val bufferedSource = io.Source.fromFile(source,"UTF8")
    val lines = bufferedSource.getLines().toList
    //lines.foreach(println)
    var key: (String, Int, Int) = ("",0,0)
    days = lines(0).split(",").slice(2,numDays+2).zipWithIndex.map(_.swap).toMap
    shifts = lines.slice(1,numShifts+1).map(_.split(",")).map(_.tail.head).zipWithIndex.map(_.swap).toMap
    println("##############################")
    println("Shifts:")
    shifts.foreach(println)
    println("###############################")
    for (i <- 1 until lines.size-1) {
      val cols = lines(i).split(",")
      for(j<-0 until cols.size){
        j match {
          case 0 =>
            val worker = cols(j).trim
            key = (worker,key._2,key._3)
          case 1 =>
            val shift = cols(j).trim
            val shiftmap: Map[String, Int] = shifts.map(_.swap)
            key = (key._1,key._2,shiftmap(shift))
          case x => x < numDays+2 match {
            case true =>
              val day = x - 2
              key = (key._1,day,key._3)
              timeMap += (key -> cols(x).trim.toInt)
            case false =>
              if(j== numDays + 2) maxShift +=(key._1 -> cols(j).trim.toInt)
            }
          }
        }
      }
    bufferedSource.close()
    Model.underline()
    println("Timetable:")
    timeMap.toSeq.sorted.foreach(println)
//    println("############################")
//    println("MaxShift:")
//    maxShift.foreach(println)
//    println("############################")
  }

}
