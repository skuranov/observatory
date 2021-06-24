package observatory

import observatory.Manipulation.makeGrid
import observatory.Visualization2.visualizeGrid
import org.junit.Test

trait Visualization2Test extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("value-added information visualization", 5) _

  @Test def visualizeGridTest:Unit = {
    val temps = List(28d, 19d, 14d)
    val locs = List(Location(23.6721, -53.9245), Location(55.2331, 72.4528), Location(11.8740, 84.3457))
    val ts = locs.zip(temps)
    val cs = List((-30d, Color(0, 0, 0)), (30d, Color(255, 255, 255)))
    val tile = Tile(0, 0, 0)

    visualizeGrid(makeGrid(ts), cs, tile)
  }


}
