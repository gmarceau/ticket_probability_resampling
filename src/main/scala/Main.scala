import java.io.File

import com.github.tototoshi.csv.CSVReader
import cc.spray.json._
import DefaultJsonProtocol._
import scalala.library.Plotting._
import scalala.library.plotting.StaticHistogramBins
import scalala.tensor.dense.DenseVector

object Main extends App {

  case class Ticket(sprint: Int, completed: Boolean, ticket: Int, hours: Double, scope: Double) {
    def hoursPerPt = hours / scope
  }
  implicit val ticketFormat = jsonFormat5(Ticket)

  def parseRow(r: Map[String, String]) =
    Ticket(r("sprint").toInt, r("completed").toBoolean, r("ticket").toInt, r("hours").toDouble, r("scope").toDouble)


  val tickets = CSVReader.open(new File("c:/downloads/Hours log for bks_on_rails.csv"))
                         .allWithHeaders()
                         .map(parseRow)
                         .filter(t => t.hours > 0 && t.completed)
                         .filter(_.sprint != 1)

  implicit def List_extension[T](lst: Iterable[T]) = new {
    def findLast(p: T => Boolean): Option[T] = {
      val r = lst.takeWhile(p)
      if (r.isEmpty) None else Some(r.last)
    }
  }

  def randomTicket: Ticket = tickets((math.random * tickets.size).floor.toInt)

  case class Sprint(ts: List[Ticket]) {
    def points = ts.map(_.scope).sum
    def hours = ts.map(_.hours).sum
  }


  def randomSprint(nPoints: Int): Sprint = {
    val ts = Stream.continually { randomTicket }
    Sprint(ts.take(ts.map(_.scope).scan(0d)(_+_).takeWhile(_ < nPoints).size).toList)
  }

  def randomSprints(nSprints: Int, nPoints: Int): List[Sprint] = List.fill(nSprints) { randomSprint(nPoints) }

  def successRate(sprints: List[Sprint])(p: Sprint => Boolean): Double = {
    val r = sprints.count(p).toDouble / sprints.size
    println(r*100 + "%")
    println(sprints.map(_.points).toJson)
    r
  }

  val sampleRate = 1000

  def findPointCutoff(desiredSuccessRate: Double, desiredHours: Double): (Int, List[Sprint]) = {
    val samples = for (nPoints <- Stream.from(1)) yield (nPoints, randomSprints(sampleRate, nPoints))

    def isSuccessful(sprint: Sprint): Boolean = sprint.hours < desiredHours

    def isGoodSample(sprints: List[Sprint]): Boolean = successRate(sprints)(isSuccessful) > desiredSuccessRate

    samples.findLast { case (_, sprints) => isGoodSample(sprints) }.get
  }

  def histogramOfHours(sprints: List[Sprint], output: File) {
    val histogramBins = StaticHistogramBins((5d until(60, 5)).toArray)
    hist(DenseVector(sprints.map(_.hours): _*), histogramBins)
    title("probability distribution of sprint outcomes")
    xlabel("hours to deliver the sprint")
    ylabel("%% (per milli)")
    saveas(output.toString)
  }

  val (nPoints, sample) = findPointCutoff(0.95, 40)
  println("Points cutoff: " + nPoints)
  histogramOfHours(sample, new File("histogram.png"))

}
