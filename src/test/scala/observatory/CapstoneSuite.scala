package observatory


class CapstoneSuite
extends ExtractionTest
    with VisualizationTest
    with InteractionTest
    with ManipulationTest
    with Visualization2Test
    with Interaction2Test

trait MilestoneSuite {

  def namedMilestoneTest(milestoneName: String, level: Int)(block: => Unit): Unit =
    if (Grading.milestone >= level) {
      block
    } else {
      throw new Exception
      //fail(s"Milestone $level ($milestoneName) is disabled. To enable it, set the 'Grading.milestone' value to '$level'.")
    }

}

