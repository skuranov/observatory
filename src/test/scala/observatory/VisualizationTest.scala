package observatory

import observatory.Visualization._
import observatory.Extraction._
import org.junit.Test

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _


  @Test def interpolateColorsTest: Unit = {
    val rgb = List((0.0, Color(255, 0, 0)), (1.0, Color(0, 0, 255)))
    val result = interpolateColor(rgb, 10)
    assert(result.equals(Color(0, 0, 255)))
    val result2 = interpolateColor(rgb, 54)
    assert(result2.equals(Color(0, 0, 255)))
  }


  @Test def predictTemperatureTest: Unit = {
    val locTemp = locationYearlyAverageRecords(
      locateTemperatures(2015, "/stations_test.csv", "/temperatures_test.csv"))
    val result = predictTemperature(locTemp, Location(0.0, 0.0))
    assert(result == 2f)
  }

  @Test def visualizeTest: Unit = {
    val locTemp = locationYearlyAverageRecords(
      locateTemperatures(2015, "/stations_test.csv", "/temperatures_test.csv"))
    val rgb = List((10d, Color(255, 255, 255)), (0d, Color(0, 0, 0)))
    val result = visualize(locTemp, rgb)
    result.output(new java.io.File("target/some-image.png"))
    assert(0.equals(result.points(0)_1))
    assert(0.equals(result.points(0)_2))
    assert(55.equals(result.points(10000)_1))
    assert(100.equals(result.points(10000)_2))
  }
}
