package observatory

import java.time.LocalDate

import scala.io.Source

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import org.apache.log4j.{Level, Logger}


/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  def toCelsius(f: Double): Double = ( f-32 ) / 1.8

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    locateTemperaturesSpark(year, stationsFile, temperaturesFile).collect().toSeq
  }

  def locateTemperaturesSpark(year: Year, stationsFile: String, temperaturesFile: String): RDD[(LocalDate, Location, Temperature)] = {
    val stationsStream = Source.getClass.getResourceAsStream(stationsFile)
    val temperaturesStream = Source.getClass.getResourceAsStream(temperaturesFile)

    val stations = spark.sparkContext.makeRDD(Source.fromInputStream(stationsStream).getLines().toList)
      .map(_.split(','))
      .filter(a => a.length == 4 && a(2).nonEmpty && a(3).nonEmpty)
      .map(a => ((a(0), a(1)), Location(a(2).toDouble, a(3).toDouble)))

    val temperatures = spark.sparkContext.makeRDD(Source.fromInputStream(temperaturesStream).getLines().toList)
      .map(_.split(','))
      .filter(a => a.length == 5 && a(4) != "9999.9")
      .map(a => ((a(0), a(1)), (LocalDate.of(year, a(2).toInt, a(3).toInt), toCelsius(a(4).toDouble))))

    stations.join(temperatures).mapValues(v => (v._2._1, v._1, v._2._2)).values
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    locationYearlyAverageRecordsSpark(spark.sparkContext.parallelize(records.toSeq)).collect().toSeq
  }

  def locationYearlyAverageRecordsSpark(records: RDD[(LocalDate, Location, Temperature)]): RDD[(Location, Temperature)] = {
    records.groupBy(_._2)
      .mapValues(es=> es.map( e=> (e._3, 1)))
      .mapValues(_.reduce((v1, v2) => (v1._1 + v2._1 , v1._2 + v2._2)))
      .mapValues( v => v._1 /v._2)
  }

//  def main(args: Array[String]): Unit = {
//    println(locateTemperatures(1975, "/stations.csv", "/1975.csv"))
//  }
}
