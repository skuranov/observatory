package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import java.nio.file.{Files, Paths}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    Location(
      scala.math.atan(scala.math.sinh(scala.math.Pi * (1 - tile.y * 2) / scala.math.pow(2, tile.zoom))) * 180.0 / scala.math.Pi,
      tile.x * 360.0 / scala.math.pow(2, tile.zoom) - 180.0
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    import Visualization._
    val size = 256
    val subTileZoomFactor = (scala.math.log(size) / scala.math.log(2)).toInt
    val targetSize = 256
    val pixels = Array.fill[Pixel](size * size)(new Pixel(getARGBPackedInt(Color(0, 0, 0))))
    var rowNum = 0

    val t2 = System.nanoTime
    (0 until size).map(x => (0 until size).map((x, _))).foreach {
      case v =>
        val t1 = System.nanoTime
        v.par.foreach {
          case (x, y) =>
            val requiredLocation = tileLocation(Tile(size * tile.x + x, size * tile.y + y, tile.zoom + subTileZoomFactor))
            val predictedTemperature = Visualization.predictTemperature(temperatures, requiredLocation)
            val interpolatedColor = Visualization.interpolateColor(colors, predictedTemperature)
            val arrIndex = y * size + x
            pixels(arrIndex) = new Pixel(getARGBPackedInt(interpolatedColor))
        }
        rowNum += 1
        println("Elapsed time for row " + rowNum + " = " + (System.nanoTime - t1) / 1e9d)
    }
    println("Elapsed time for tile " + tile + " = " + (System.nanoTime - t2) / 1e9d)

    if (targetSize == size) Image(size, size, pixels)
    else Image(size, size, pixels).scale(targetSize / size)
  }

  private def getARGBPackedInt(color: Color) = {
    (127 << 24) + (color.red << 16) + (color.green << 8) + color.blue
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
    val zooms = Seq(0, 1, 2, 3)
    zooms
      .flatMap(zoom => (0 until 1 << zoom)
        .flatMap(x => (0 until 1 << zoom)
          .map((x, _, zoom))))
      .grouped(3)
      .toList
      .foreach {
        case list =>
          yearlyData.foreach {
            case (year, data) =>
              list.par.foreach {
                case (x, y, zoom) =>
                  println("Generating Image at Tile : ", (x, y, zoom))
                  generateImage(year, Tile(x, y, zoom), data)
                  println("Generated Image Successfully at Tile : ", (x, y, zoom))
              }
          }
      }
  }

  def generateImage(year: Year, tile: Tile, locationYearlyAverageRecords: Iterable[(Location, Temperature)]) = {
    val colors = Seq[(Temperature, Color)](
      (-60, Color(0, 0, 0)),
      (-50, Color(33, 0, 107)),
      (-27, Color(255, 0, 255)),
      (-15, Color(0, 0, 255)),
      (0, Color(0, 255, 255)),
      (12, Color(255, 255, 0)),
      (32, Color(255, 0, 0)),
      (60, Color(255, 255, 255))
    )
    val image = Interaction.tile(locationYearlyAverageRecords, colors, tile)
    Files.createDirectories(Paths.get("target/temperatures/" + year + "/" + tile.zoom));
    image.output(new java.io.File("target/temperatures/" + year + "/" + tile.zoom + "/" + tile.x + "-" + tile.y + ".png"))
  }

}