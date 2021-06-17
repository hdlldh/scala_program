package observatory

import com.sksamuel.scrimage.nio.{ImageWriter, PngWriter}

object Main extends App {
  val year = 2021
  val fileName = s"target/test_year_$year.png"
  val colors = Array(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0)))



  implicit val writer = PngWriter.NoCompression
  val temperatures = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year,"/stations.csv",s"/$year.csv"))
  temperatures.toList.sortBy(_._2).foreach(println)
//  println(temperatures.minBy(_._2))
//  println(temperatures.maxBy(_._2))
  val predTemp = Visualization.predictTemperature(temperatures, Location(31.0, -180.0))
  println(predTemp)
  val color = Visualization.interpolateColor(colors, predTemp)
  println(color)

//  Visualization.interpolateColor()
//  val myImage = Visualization.visualize(temperatures, colors)
//  myImage.output(new java.io.File(fileName))

  Interaction.tile(temperatures, colors, Tile(3, 4, 10)).output(new java.io.File(fileName))
}
