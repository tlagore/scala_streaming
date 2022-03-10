package streams

import scala.io.Source

object SampleMain extends App {
  def printSampleNumber(sample: Int): Unit = {
    println(s"Current sample: $sample")
  }

  def printSamples(sample: Vector[Int], curElement: Int): Unit = {
    printSampleNumber(curElement)
    println(s"Elements: $sample")
  }

  def averages(sample: Vector[Int], curElement: Int): Unit = {
    println(s"avg: ${sample.sum / sample.length}")
    Thread.sleep(1000)
  }

  // process arguments
  if (args.size != 2) {
    args.foreach(println)
    throw new IllegalArgumentException(s"Arguments missing <filename> <delimiter> <relativeSupportThreshold> [limit] ")
  }
  val filename = args(0)
  val sizeSample = args(1).toInt
  if (sizeSample < 0) {
    throw new IllegalArgumentException(s"sizeSample (arg2) must be between > 0")
  }

  private val PRIME =  4294967311L
  // seed the  random number generator, so we have reproducible results
  private val rand = new scala.util.Random(PRIME)

  val lines = my_utils.getLines(filename).iterator

  reservoirSample.process(lines.map(_.toInt), sizeSample, rand, List[Standing_Query](printSamples, averages))
}
