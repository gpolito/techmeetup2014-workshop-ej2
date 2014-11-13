import java.text.SimpleDateFormat
import java.util.Date
import java.io.File
import scala.io.Source
import scala.util.Try

/** XXX
 * Created by f on 11/12/14.
 */
object FinancialTool {

  type Sym      = String
  type SymValue = (Sym, Double)

  type Column = (String, Double)
  case class Row(date: Date, columns: Map[String, Double])

  val columnNames = Seq("Open","High","Low","Close","Volume", "AdjClose")

  def findFiles(root: String, symbols: Set[Sym]): Seq[String] = symbols.toSeq match {
    case Nil => Nil
    case (sym +: xs) => {
      val file = new File(root, sym + ".csv")
      if (file.exists())
        file.getAbsolutePath() +: findFiles(root, xs.toSet)
      else
        findFiles(root, xs.toSet)
    }
  }

  def readLines(file: String):  Seq[String] = 
    Source.fromFile(file).getLines.toSeq.tail.filter(!_.isEmpty)

  def parseLine(line: String): Row = {
    val lineValues = line split ","
    val zipList = columnNames zip lineValues.tail
    val rowMap = zipList.foldLeft (Map[String, Double]()) ((map, t) => map + (t._1 -> stringToDouble(t._2))) 
    Row(dateFromString(lineValues.head), rowMap)
  }

  def query(root: String, symbols: Seq[Sym], dates: Seq[Date], col: String): Map [Date, Seq[SymValue]] = 
    (symbols, dates) match {
      case (syms, Nil) => Map()
      case (Nil, dts) => Map()
      case (s +: syms, d +: dts) => findFiles(root, )
    }


  private def dateFromString(date: String): Date = {
    val simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
    simpleDateFormat.parse(date)
  }

  private def stringToDouble(str: String): Double = {
    Try(str.toDouble).getOrElse(0D)
  }
}
