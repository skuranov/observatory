package observatory

import java.util.concurrent.atomic.AtomicInteger

import observatory.Interaction.generateTiles
import org.junit.Test

trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  @Test def generateTilesTest: Unit = {
    val atomicInt = new AtomicInteger()

    def trackCalls(year: Int, tile: Tile, data: Any): Unit = {
      val _ = atomicInt.incrementAndGet()
    }

    val years = Range.inclusive(1901, 2000).map(year => (year, Nil))

    generateTiles[Any](years, trackCalls)
    assert(atomicInt.get() == (1 + 4 + 16 + 64) * years.size)
  }

}
