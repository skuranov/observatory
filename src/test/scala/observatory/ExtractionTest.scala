package observatory

import observatory.Extraction._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _


  @Test def locateTemperaturesTest: Unit = {
    val locTemp = locateTemperatures(2015, "/stations_test.csv",
      "/temperatures_test.csv")
    assert(locTemp.head._1.getYear == 2015)
    assert(locTemp.head._1.getMonthValue == 12)
    assert(locTemp.head._1.getDayOfMonth == 6)
    assert(locTemp.head._2.lat == 90f)
    assert(locTemp.head._2.lon == 80f)
    assert(locTemp.head._3 == 10f)
  }
}
