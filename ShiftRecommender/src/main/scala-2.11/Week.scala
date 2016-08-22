import java.io.File

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
  val numTasks: Int = 5
  val numWorkers: Int = 21

  /**
    * names maps each workerId to a name
    */
  val names = Map(
    0 -> "Alex",
    1 -> "Alida",
    2 -> "Ami",
    3 -> "Carmen",
    4 -> "Chris",
    5 -> "Cindy",
    6 -> "David",
    7 -> "Eb",
    8 -> "Jason",
    9 -> "Joseph",
    10 -> "Maike",
    11 -> "Maria",
    12 -> "Marion",
    13 -> "Marius",
    14 -> "Mustafa",
    15 -> "Peter",
    16 -> "Ruxi",
    17 -> "Simon",
    18 -> "Simonka",
    19 -> "Sonja",
    20 -> "Tarek",
    21 -> "Tommi",
    22 -> "Test",
    23 -> "Test",
    24 -> "Test",
    25 -> "Test",
    26 -> "Test",
    27 -> "Test",
    28 -> "Test",
    29 -> "Test",
    30 -> "Test"
  )
  var maxShift=Map(
    ("Alex",3),
    ("Alida",3),
    ("Ami",3),
    ("Carmen",4),
    ("Chris",3),
    ("Cindy",5),
    ("David",5),
    ("Eb",5),
    ("Jason",5),
    ("Joseph",5),
    ("Maike",5),
    ("Maria",5),
    ("Marion",5),
    ("Marius",5),
    ("Mustafa",4),
    ("Peter",3),
    ("Ruxi",4),
    ("Simon",5),
    ("Simonka",5),
    ("Sonja",5),
    ("Tarek",4),
    ("Tommi",5),
    ("Test",4)
  )
  var timeMap = Map(
    ("Alex",0,0) -> 1,
    ("Alex",0,1) -> 1,
    ("Alex",1,0) -> 1,
    ("Alex",1,1) -> 1,
    ("Alex",2,0) -> 1,
    ("Alex",2,1) -> 1,
    ("Alex",3,0) -> 0,
    ("Alex",3,1) -> 0,
    ("Alex",4,0) -> 0,
    ("Alex",4,1) -> 0,
    ("Alex",5,0) -> 0,
    ("Alex",5,1) -> 0,
    ("Alex",6,0) -> 1,
    ("Alex",6,1) -> 1,
    ("Alex",7,0) -> 1,
    ("Alex",7,1) -> 1,
    ("Alex",8,0) -> 1,
    ("Alida",0,0) -> 1,
    ("Alida",0,1) -> 1,
    ("Alida",1,0) -> 1,
    ("Alida",1,1) -> 1,
    ("Alida",2,0) -> 1,
    ("Alida",2,1) -> 1,
    ("Alida",3,0) -> 1,
    ("Alida",3,1) -> 1,
    ("Alida",4,0) -> 1,
    ("Alida",4,1) -> 1,
    ("Alida",5,0) -> 1,
    ("Alida",5,1) -> 1,
    ("Alida",6,0) -> 1,
    ("Alida",6,1) -> 1,
    ("Alida",7,0) -> 1,
    ("Alida",7,1) -> 1,
    ("Alida",8,0) -> 1,
    ("Ami",0,0) -> 1,
    ("Ami",0,1) -> 1,
    ("Ami",1,0) -> 0,
    ("Ami",1,1) -> 0,
    ("Ami",2,0) -> 0,
    ("Ami",2,1) -> 0,
    ("Ami",3,0) -> 1,
    ("Ami",3,1) -> 1,
    ("Ami",4,0) -> 1,
    ("Ami",4,1) -> 1,
    ("Ami",5,0) -> 1,
    ("Ami",5,1) -> 1,
    ("Ami",6,0) -> 1,
    ("Ami",6,1) -> 1,
    ("Ami",7,0) -> 1,
    ("Ami",7,1) -> 1,
    ("Ami",8,0) -> 1,
    ("Carmen",0,0) -> 1,
    ("Carmen",0,1) -> 1,
    ("Carmen",1,0) -> 0,
    ("Carmen",1,1) -> 0,
    ("Carmen",2,0) -> 0,
    ("Carmen",2,1) -> 0,
    ("Carmen",3,0) -> 1,
    ("Carmen",3,1) -> 1,
    ("Carmen",4,0) -> 1,
    ("Carmen",4,1) -> 1,
    ("Carmen",5,0) -> 1,
    ("Carmen",5,1) -> 1,
    ("Carmen",6,0) -> 1,
    ("Carmen",6,1) -> 1,
    ("Carmen",7,0) -> 1,
    ("Carmen",7,1) -> 1,
    ("Carmen",8,0) -> 1,
    ("Chris",0,0) -> 1,
    ("Chris",0,1) -> 1,
    ("Chris",1,0) -> 1,
    ("Chris",1,1) -> 1,
    ("Chris",2,0) -> 1,
    ("Chris",2,1) -> 1,
    ("Chris",3,0) -> 1,
    ("Chris",3,1) -> 1,
    ("Chris",4,0) -> 1,
    ("Chris",4,1) -> 1,
    ("Chris",5,0) -> 1,
    ("Chris",5,1) -> 1,
    ("Chris",6,0) -> 1,
    ("Chris",6,1) -> 1,
    ("Chris",7,0) -> 1,
    ("Chris",7,1) -> 1,
    ("Chris",8,0) -> 1,
    ("Cindy",0,0) -> 1,
    ("Cindy",0,1) -> 1,
    ("Cindy",1,0) -> 0,
    ("Cindy",1,1) -> 0,
    ("Cindy",2,0) -> 0,
    ("Cindy",2,1) -> 0,
    ("Cindy",3,0) -> 1,
    ("Cindy",3,1) -> 1,
    ("Cindy",4,0) -> 1,
    ("Cindy",4,1) -> 1,
    ("Cindy",5,0) -> 1,
    ("Cindy",5,1) -> 1,
    ("Cindy",6,0) -> 1,
    ("Cindy",6,1) -> 1,
    ("Cindy",7,0) -> 1,
    ("Cindy",7,1) -> 1,
    ("Cindy",8,0) -> 1,
    ("David",0,0) -> 1,
    ("David",0,1) -> 1,
    ("David",1,0) -> 1,
    ("David",1,1) -> 0,
    ("David",2,0) -> 1,
    ("David",2,1) -> 0,
    ("David",3,0) -> 1,
    ("David",3,1) -> 1,
    ("David",4,0) -> 1,
    ("David",4,1) -> 1,
    ("David",5,0) -> 1,
    ("David",5,1) -> 1,
    ("David",6,0) -> 1,
    ("David",6,1) -> 1,
    ("David",7,0) -> 1,
    ("David",7,1) -> 1,
    ("David",8,0) -> 1,
    ("Eb",0,0) -> 1,
    ("Eb",0,1) -> 1,
    ("Eb",1,0) -> 1,
    ("Eb",1,1) -> 1,
    ("Eb",2,0) -> 1,
    ("Eb",2,1) -> 1,
    ("Eb",3,0) -> 1,
    ("Eb",3,1) -> 1,
    ("Eb",4,0) -> 1,
    ("Eb",4,1) -> 1,
    ("Eb",5,0) -> 1,
    ("Eb",5,1) -> 1,
    ("Eb",6,0) -> 1,
    ("Eb",6,1) -> 1,
    ("Eb",7,0) -> 1,
    ("Eb",7,1) -> 1,
    ("Jason",0,0) -> 1,
    ("Jason",0,1) -> 1,
    ("Jason",1,0) -> 1,
    ("Jason",1,1) -> 1,
    ("Jason",2,0) -> 1,
    ("Jason",2,1) -> 1,
    ("Jason",3,0) -> 1,
    ("Jason",3,1) -> 1,
    ("Jason",4,0) -> 1,
    ("Jason",4,1) -> 1,
    ("Jason",5,0) -> 1,
    ("Jason",5,1) -> 1,
    ("Jason",6,0) -> 1,
    ("Jason",6,1) -> 1,
    ("Jason",7,0) -> 1,
    ("Jason",7,1) -> 1,
    ("Joseph",0,0) -> 1,
    ("Joseph",0,1) -> 1,
    ("Joseph",1,0) -> 1,
    ("Joseph",1,1) -> 1,
    ("Joseph",2,0) -> 1,
    ("Joseph",2,1) -> 1,
    ("Joseph",3,0) -> 1,
    ("Joseph",3,1) -> 1,
    ("Joseph",4,0) -> 1,
    ("Joseph",4,1) -> 1,
    ("Joseph",5,0) -> 1,
    ("Joseph",5,1) -> 1,
    ("Joseph",6,0) -> 1,
    ("Joseph",6,1) -> 1,
    ("Joseph",7,0) -> 1,
    ("Joseph",7,1) -> 1,
    ("Maria",0,0) -> 1,
    ("Maria",0,1) -> 1,
    ("Maria",1,0) -> 1,
    ("Maria",1,1) -> 1,
    ("Maria",2,0) -> 1,
    ("Maria",2,1) -> 1,
    ("Maria",3,0) -> 1,
    ("Maria",3,1) -> 1,
    ("Maria",4,0) -> 1,
    ("Maria",4,1) -> 1,
    ("Maria",5,0) -> 1,
    ("Maria",5,1) -> 1,
    ("Maria",6,0) -> 1,
    ("Maria",6,1) -> 1,
    ("Maria",7,0) -> 1,
    ("Maria",7,1) -> 1,
    ("Maria",8,0) -> 1,
    ("Maike",0,0) -> 1,
    ("Maike",0,1) -> 0,
    ("Maike",1,0) -> 1,
    ("Maike",1,1) -> 0,
    ("Maike",2,0) -> 1,
    ("Maike",2,1) -> 0,
    ("Maike",3,0) -> 1,
    ("Maike",3,1) -> 0,
    ("Maike",4,0) -> 1,
    ("Maike",4,1) -> 0,
    ("Maike",5,0) -> 1,
    ("Maike",5,1) -> 0,
    ("Maike",6,0) -> 1,
    ("Maike",6,1) -> 0,
    ("Maike",7,0) -> 1,
    ("Maike",7,1) -> 0,
    ("Maike",8,0) -> 1,
    ("Marion",0,0) -> 1,
    ("Marion",0,1) -> 1,
    ("Marion",1,0) -> 1,
    ("Marion",1,1) -> 1,
    ("Marion",2,0) -> 1,
    ("Marion",2,1) -> 1,
    ("Marion",3,0) -> 1,
    ("Marion",3,1) -> 1,
    ("Marion",4,0) -> 1,
    ("Marion",4,1) -> 1,
    ("Marion",5,0) -> 1,
    ("Marion",5,1) -> 1,
    ("Marion",6,0) -> 1,
    ("Marion",6,1) -> 1,
    ("Marion",7,0) -> 1,
    ("Marion",7,1) -> 1,
    ("Marius",0,0) -> 1,
    ("Marius",0,1) -> 1,
    ("Marius",1,0) -> 1,
    ("Marius",1,1) -> 1,
    ("Marius",2,0) -> 1,
    ("Marius",2,1) -> 1,
    ("Marius",3,0) -> 1,
    ("Marius",3,1) -> 1,
    ("Marius",4,0) -> 1,
    ("Marius",4,1) -> 1,
    ("Marius",5,0) -> 1,
    ("Marius",5,1) -> 1,
    ("Marius",6,0) -> 1,
    ("Marius",6,1) -> 1,
    ("Marius",7,0) -> 1,
    ("Marius",7,1) -> 1,
    ("Mustafa",0,0) -> 1,
    ("Mustafa",0,1) -> 1,
    ("Mustafa",1,0) -> 1,
    ("Mustafa",1,1) -> 1,
    ("Mustafa",2,0) -> 1,
    ("Mustafa",2,1) -> 1,
    ("Mustafa",3,0) -> 1,
    ("Mustafa",3,1) -> 1,
    ("Mustafa",4,0) -> 1,
    ("Mustafa",4,1) -> 1,
    ("Mustafa",5,0) -> 1,
    ("Mustafa",5,1) -> 1,
    ("Mustafa",6,0) -> 1,
    ("Mustafa",6,1) -> 1,
    ("Mustafa",7,0) -> 1,
    ("Mustafa",7,1) -> 1,
    ("Peter",0,0) -> 1,
    ("Peter",0,1) -> 1,
    ("Peter",1,0) -> 1,
    ("Peter",1,1) -> 1,
    ("Peter",2,0) -> 1,
    ("Peter",2,1) -> 1,
    ("Peter",3,0) -> 1,
    ("Peter",3,1) -> 1,
    ("Peter",4,0) -> 1,
    ("Peter",4,1) -> 1,
    ("Peter",5,0) -> 1,
    ("Peter",5,1) -> 1,
    ("Peter",6,0) -> 1,
    ("Peter",6,1) -> 1,
    ("Peter",7,0) -> 1,
    ("Peter",7,1) -> 1,
    ("Ruxi",0,0) -> 1,
    ("Ruxi",0,1) -> 1,
    ("Ruxi",1,0) -> 1,
    ("Ruxi",1,1) -> 1,
    ("Ruxi",2,0) -> 1,
    ("Ruxi",2,1) -> 1,
    ("Ruxi",3,0) -> 1,
    ("Ruxi",3,1) -> 1,
    ("Ruxi",4,0) -> 1,
    ("Ruxi",4,1) -> 1,
    ("Ruxi",5,0) -> 1,
    ("Ruxi",5,1) -> 1,
    ("Ruxi",6,0) -> 1,
    ("Ruxi",6,1) -> 1,
    ("Ruxi",7,0) -> 1,
    ("Ruxi",7,1) -> 1,
    ("Simon",0,0) -> 1,
    ("Simon",0,1) -> 1,
    ("Simon",1,0) -> 1,
    ("Simon",1,1) -> 1,
    ("Simon",2,0) -> 1,
    ("Simon",2,1) -> 1,
    ("Simon",3,0) -> 1,
    ("Simon",3,1) -> 1,
    ("Simon",4,0) -> 1,
    ("Simon",4,1) -> 1,
    ("Simon",5,0) -> 1,
    ("Simon",5,1) -> 1,
    ("Simon",6,0) -> 1,
    ("Simon",6,1) -> 1,
    ("Simon",7,0) -> 1,
    ("Simon",7,1) -> 1,
    ("Simonka",0,0) -> 1,
    ("Simonka",0,1) -> 1,
    ("Simonka",1,0) -> 1,
    ("Simonka",1,1) -> 1,
    ("Simonka",2,0) -> 1,
    ("Simonka",2,1) -> 1,
    ("Simonka",3,0) -> 1,
    ("Simonka",3,1) -> 1,
    ("Simonka",4,0) -> 1,
    ("Simonka",4,1) -> 1,
    ("Simonka",5,0) -> 1,
    ("Simonka",5,1) -> 1,
    ("Simonka",6,0) -> 1,
    ("Simonka",6,1) -> 1,
    ("Simonka",7,0) -> 1,
    ("Simonka",7,1) -> 1,
    ("Sonja",0,0) -> 1,
    ("Sonja",0,1) -> 1,
    ("Sonja",1,0) -> 1,
    ("Sonja",1,1) -> 1,
    ("Sonja",2,0) -> 1,
    ("Sonja",2,1) -> 1,
    ("Sonja",3,0) -> 1,
    ("Sonja",3,1) -> 1,
    ("Sonja",4,0) -> 1,
    ("Sonja",4,1) -> 1,
    ("Sonja",5,0) -> 1,
    ("Sonja",5,1) -> 1,
    ("Sonja",6,0) -> 1,
    ("Sonja",6,1) -> 1,
    ("Sonja",7,0) -> 1,
    ("Sonja",7,1) -> 1,
    ("Sonja",0,0) -> 0,
    ("Sonja",0,1) -> 1,
    ("Sonja",1,0) -> 0,
    ("Sonja",1,1) -> 1,
    ("Sonja",2,0) -> 0,
    ("Sonja",2,1) -> 1,
    ("Sonja",3,0) -> 0,
    ("Sonja",3,1) -> 1,
    ("Sonja",4,0) -> 0,
    ("Sonja",4,1) -> 1,
    ("Sonja",5,0) -> 0,
    ("Sonja",5,1) -> 1,
    ("Sonja",6,0) -> 1,
    ("Sonja",6,1) -> 1,
    ("Sonja",7,0) -> 1,
    ("Sonja",7,1) -> 1,
    ("Sonja",8,0) -> 1,
    ("Tarek",0,0) -> 1,
    ("Tarek",0,1) -> 1,
    ("Tarek",1,0) -> 1,
    ("Tarek",1,1) -> 1,
    ("Tarek",2,0) -> 1,
    ("Tarek",2,1) -> 1,
    ("Tarek",3,0) -> 1,
    ("Tarek",3,1) -> 1,
    ("Tarek",4,0) -> 1,
    ("Tarek",4,1) -> 1,
    ("Tarek",5,0) -> 1,
    ("Tarek",5,1) -> 1,
    ("Tarek",6,0) -> 1,
    ("Tarek",6,1) -> 1,
    ("Tarek",7,0) -> 1,
    ("Tarek",7,1) -> 1,
    ("Tommi",0,0) -> 1,
    ("Tommi",0,1) -> 1,
    ("Tommi",1,0) -> 1,
    ("Tommi",1,1) -> 1,
    ("Tommi",2,0) -> 1,
    ("Tommi",2,1) -> 1,
    ("Tommi",3,0) -> 1,
    ("Tommi",3,1) -> 1,
    ("Tommi",4,0) -> 1,
    ("Tommi",4,1) -> 1,
    ("Tommi",5,0) -> 1,
    ("Tommi",5,1) -> 1,
    ("Tommi",6,0) -> 1,
    ("Tommi",6,1) -> 1,
    ("Tommi",7,0) -> 1,
    ("Tommi",7,1) -> 1,
    ("Test",0,0) -> 1,
    ("Test",0,1) -> 1,
    ("Test",1,0) -> 1,
    ("Test",1,1) -> 1,
    ("Test",2,0) -> 1,
    ("Test",2,1) -> 1,
    ("Test",3,0) -> 1,
    ("Test",3,1) -> 1,
    ("Test",4,0) -> 1,
    ("Test",4,1) -> 1,
    ("Test",5,0) -> 1,
    ("Test",5,1) -> 1,
    ("Test",6,0) -> 1,
    ("Test",6,1) -> 1,
    ("Test",7,0) -> 1,
    ("Test",7,1) -> 1
  )
  val days = Map (
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
  val shifts = Map(
    (0,"Frueh"),
    (1,"Spaet"),
    (2,"Nacht"),
    (3,"FrueherMorgen")
  )
  val tasks = Map(
    (0,"Buerger"),
    (2,"Fritte"),
    (3,"Saladette"),
    (1,"Herd"),
    (4,"Spuele"),
    (5,"Bons"),
    (6,"Vorbereitung"),
    (7,"Putzen")
  )

  def readTimetableFromFile(source: String) ={

    val bufferedSource = io.Source.fromFile(source)
    val lines = bufferedSource.getLines().toList
    var key: (String, Int, Int) = ("",0,0)
    for (i <- 1 until lines.size-1) {
      val cols = lines(i).split(";")
      for(j<-0 until cols.size){
        j match {
          case 0 => key = (cols(j),key._2,key._3)
          case 1 =>
            val shift = shifts.filter(_._2==cols(j)).head._1
            key = (key._1,key._2,shift)
          case x => x < numDays+2 match {
            case true =>
              val day = x - 2
              key = (key._1,day,key._3)
              timeMap = timeMap + (key -> cols(x).toInt)
            case false =>
              if(j== numDays + 2) maxShift = maxShift + (key._1 -> cols(j).toInt)
            }
          }
        }
      }
    bufferedSource.close()
    println("############################")
    println("Timetable:")
    timeMap.foreach(println)
    println("############################")
    println("MaxShift:")
    maxShift.foreach(println)
    println("############################")
  }

}