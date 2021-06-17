package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  val Width = 256
  val Height = 256
  val SubtileZoom = 8

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val lat = atan(sinh(PI - tile.y / pow(2, tile.zoom) * 2 * PI)) * 180 / PI
    val lng = tile.x / pow(2, tile.zoom) * 360 - 180
    Location(lat, lng)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val subtileScale = 1 << SubtileZoom
    val x0 = tile.x * subtileScale
    val y0 = tile.y * subtileScale
    val zoom = tile.zoom

    val coords = for {
      y <- 0 until Height
      x <- 0 until Width
    } yield (x, y)

    val pixels = coords.par
      .map {case (x, y) => tileLocation(Tile(x + x0,y+y0, zoom + SubtileZoom))}
      .map(Visualization.predictTemperature(temperatures, _))
      .map(Visualization.interpolateColor(colors, _))
      .map(c => Pixel(c.red, c.green, c.blue, 127))
      .toArray
    Image(Width, Height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 until 4
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
    } generateImage(year, Tile(x, y, zoom), data)
  }
}
