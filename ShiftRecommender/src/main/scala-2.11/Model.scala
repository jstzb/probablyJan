/**
  *
  * Model of Shift Recommender
  *
  * Created by jan on 21.07.16.
  */
object Model {
  def underline() = println("#############################################################")

  def title(s: String) = {
    underline()
    println(s)
    underline()
  }

  def main(args: Array[String]) {
    val week = new Week()
    val dir: String = System.getProperty("user.home")
    val sep: String = System.getProperty("line.separator")
    week.readTimetableFromFile(dir+"/time.csv",2)
    week.readQualificationsFromFile(dir+"/qual.csv")
    val recommender = new Recommender(week)
    recommender.writeFile(dir+"/recom.csv")

  }

}
