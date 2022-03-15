package streams

import scala.io.Source

object SampleMain extends App {
  def printSampleNumber(sample: Int): Unit = {
    println(s"Current sample: $sample")
  }

  var lastSample = Vector[Int]()
  def verifySamples(sample: Vector[Int], curElement: Int): Unit = {
    if (lastSample.length == 0)
      lastSample = sample
    else {
      val diff = lastSample.diff(sample)
      println(diff)
      assert(diff.length == 1)
      lastSample = sample
    }
  }

  def averages(sample: Vector[Int], curElement: Int): Unit = {
    println(s"avg: ${sample.sum / sample.length}")
  }

  // process arguments
  if (args.length != 2) {
    args.foreach(println)
    throw new IllegalArgumentException(s"Arguments missing <filename> <sample size>")
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

  reservoirSample.process(lines.map(_.toInt), sizeSample, rand, List[Standing_Query](verifySamples, averages))
}
