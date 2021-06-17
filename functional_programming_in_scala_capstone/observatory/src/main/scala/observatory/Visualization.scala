package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  val EarthRadius = 6371.0
  val Width = 360
  val Height = 180
  val OrderP = 6

//  def toRadius(d: Double): Double = d * PI / 180
//
//  def areAntipodes(l1: Location, l2: Location): Boolean = {
//    (l1.lat == -l1.lat) && (abs(l1.lon - l2.lon) == 180)
//  }
//
//  def distance(l1: Location, l2: Location):Double = {
//    if (l1==l2) 0.0
//    else if (areAntipodes(l1, l2)) EarthRadius * PI
//    else {
//      val lat1 = toRadius(l1.lat)
//      val lat2 = toRadius(l2.lat)
//      val deltaLon = abs(toRadius(l1.lon - l2.lon))
//      EarthRadius * acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(deltaLon))
//    }
//  }

  def distance(l1: Location, l2: Location): Double = {
    val deltaLat = toRadians(l1.lat - l2.lat)
    val deltaLng = toRadians(l1.lon - l2.lon)
    val sinLat = sin(deltaLat / 2)
    val sinLng = sin(deltaLng / 2)
    val a = sinLat * sinLat + cos(toRadians(l1.lat)) * cos(toRadians(l2.lat)) * sinLng * sinLng
    val c = 2 * atan2(sqrt(a), sqrt(1-a))
    EarthRadius * c
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val distTemp = temperatures.map( e=> (distance(e._1, location), e._2))
    val closest = distTemp.minBy(_._1)

    if (closest._1 <= 1) closest._2
    else {
      val weightedTemp = distTemp.map {
        case (dist, temp) => (temp/pow(dist, OrderP), 1/pow(dist, OrderP))
      }.reduce((e1, e2) => (e1._1 + e2._1, e1._2 + e2._2))
      weightedTemp._1 /weightedTemp._2
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val (c, w) = points.partition(_._1 < value)
    if (c.isEmpty) {
      w.minBy(_._1)._2
    }
    else if (w.isEmpty) {
      c.maxBy(_._1)._2
    }
    else {
      val closestC = c.maxBy(_._1)
      val closestW = w.minBy(_._1)
      val diffC = value - closestC._1
      val diffW = closestW._1 - value

      def helper(x1: Int, x2: Int): Int = ((x1 * diffC + x2 * diffW) / (diffC + diffW)).round.toInt

      Color(
        helper(closestW._2.red, closestC._2.red),
        helper(closestW._2.green, closestC._2.green),
        helper(closestW._2.blue, closestC._2.blue))
    }

  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val coords = for {
      y <- 0 until Height
      x <- 0 until Width
    } yield (x, y)

    def toLocation(coord: (Int, Int)): Location = {
      Location((Height/2 - coord._2) * 180.0 / Height, (coord._1 - Width/2) * 360.0 /Width)
    }

    val pixels = coords.map(toLocation)
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(c => Pixel(c.red, c.green, c.blue, 255))
      .toArray

    Image(Width, Height, pixels)

  }

}

