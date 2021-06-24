package observatory

import observatory.Manipulation.{average, deviation, makeGrid}
import org.junit.Test

trait ManipulationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data manipulation", 4) _

  @Test def makeGridTest: Unit = {
    val t1 = 0.0
    val t2 = -60.0
    val x = Location(0, 0)
    val y = Location(90, 90)
    val temperatures = List(
      (x, t1),
      (y, t2)
    )
    val grid: GridLocation => Temperature = makeGrid(temperatures)

    assert(grid(GridLocation(0, 0)) == 0.0)
    assert(grid(GridLocation(90, 90)) == -60.0)

    val intermediateResult = grid(GridLocation(45, 45))
    assert(intermediateResult < 0.0 && intermediateResult > -60.0)
  }

  @Test def averageTest: Unit = {
    val t1 = 0.0
    val t2 = -60.0
    val x = Location(0, 0)
    val y = Location(90, 90)
    val temps1 = List(
      (x, t1),
      (y, t2)
    )
    val temps2 = List(
      (x, t2),
      (y, t1)
    )
    val avg = average(Seq(temps1, temps2))

    assert(avg(GridLocation(0, 0)) == -30.0)
  }

  @Test def deviationTest: Unit = {
    val t1 = 0.0
    val t2 = -60.0
    val x = Location(0, 0)
    val y = Location(90, 90)
    val temps1 = List(
      (x, t1),
      (y, t2)
    )
    val temps2 = List(
      (x, t2),
      (y, t1)
    )
    val avg = average(Seq(temps1, temps2))

    val dvs = deviation(temps1, avg)

    assert(dvs(GridLocation(0, 0)) == 30.0)
  }

}
