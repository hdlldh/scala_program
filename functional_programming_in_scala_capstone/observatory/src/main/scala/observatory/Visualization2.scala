package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00   Top-left value
    * @param d01   Bottom-left value
    * @param d10   Top-right value
    * @param d11   Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             point: CellPoint,
                             d00: Temperature,
                             d01: Temperature,
                             d10: Temperature,
                             d11: Temperature
                           ): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) +
      d10 * point.x * (1 - point.y) +
      d01 * (1 - point.x) * point.y +
      d11 * point.x * point.y
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param tile   Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: GridLocation => Temperature,
                     colors: Iterable[(Temperature, Color)],
                     tile: Tile
                   ): Image = {
    val Height = 256
    val Width = 256

    val coords = for {
      y <- 0 until Height
      x <- 0 until Width
    } yield Interaction.tileLocation(Tile(
      (x.toDouble / Width + tile.x).round.toInt,
      (y.toDouble / Height + tile.y).round.toInt,
      tile.zoom)
    )

    val pixels = coords.map { loc =>
//      val latInt = loc.lat.ceil.toInt
//      val top = if (latInt == -90) -89 else latInt
//      val bottom = if (top == -89) 90 else top - 1
//
//      val lonInt = loc.lon.floor.toInt
//      val left = if (lonInt == 180) -180 else lonInt
//      val right = if (left == 179) -180 else left + 1

      val scaledLat = (loc.lat + 90.0) * 179.0 / 180.0 - 89.0
      val scaledLon = (loc.lon + 180.0) * 359.0 / 360.0 - 180.0
      val top = scaledLat.ceil.toInt
      val bottom = scaledLat.floor.toInt
      val left = scaledLon.floor.toInt
      val right = scaledLon.ceil.toInt

      val d00 = grid(GridLocation(top, left))
      val d01 = grid(GridLocation(bottom, left))
      val d10 = grid(GridLocation(top, right))
      val d11 = grid(GridLocation(bottom, right))
//      val xfrac = loc.lon - lonInt
//      val yfrac = latInt - loc.lat
      val xfrac = loc.lon - left
      val yfrac = top - loc.lat
      Visualization.interpolateColor(
        colors,
        bilinearInterpolation(CellPoint(xfrac, yfrac), d00, d01, d10, d11))
    }.map {c=>Pixel(c.red, c.green, c.blue, 127)}.toArray

    Image(Width, Height, pixels)

//    val coords = for {
//      y <- 0 until Height
//      x <- 0 until Width
//    } yield Interaction.tileLocation(Tile((x.toDouble /Width).ceil.toInt + tile.x, (y.toDouble/Height).floor.toInt + tile.y, tile.zoom))
//
//    val pixels = coords.par.map { coord =>
//      val top = coord.lat.ceil.toInt
//      val bottom = top - 1
//
//      val left = coord.lon.floor.toInt
//      val right = left + 1
//
//      val xfrac = loc.lon -left
//      val yfrac = loc.lat -top
//
//      val d00 = grid(GridLocation(top, left))
//      val d01 = grid(GridLocation(bottom, left))
//      val d10 = grid(GridLocation(top, right))
//      val d11 = grid(GridLocation(bottom, right))
//
////      val d00 = grid(GridLocation(ceil(loc.lat).toInt, floor(loc.lon).toInt))
////      val d01 = grid(GridLocation(floor(loc.lat).toInt, floor(loc.lon).toInt))
////      val d10 = grid(GridLocation(ceil(loc.lat).toInt, ceil(loc.lon).toInt))
////      val d11 = grid(GridLocation(floor(loc.lat).toInt, ceil(loc.lon).toInt))
////      val xfrac = loc.lon - floor(loc.lon)
////      val yfrac = ceil(loc.lat) - loc.lat
//
//      val c = Visualization.interpolateColor(colors,
//        bilinearInterpolation(CellPoint(xfrac, yfrac), d00, d01, d10, d11))
//      Pixel(c.red, c.green, c.blue, 127)
//    }.toArray
//    Image(Width, Height, pixels)
  }
}
