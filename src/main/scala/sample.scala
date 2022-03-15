package streams

object SampleMain extends App {
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

  // expects numbers < 0. checks tolerance close to decimal place
  def fuzzyEquals(a: Double, b:Double): Boolean = {
    val decimalsInN = decimals(a)
    val tolerance = 1/math.pow(10, decimalsInN)

    // println(s"checking if $a > ${b-tolerance} or $a < ${b+tolerance}, tolerance is ${tolerance}")

    a > (b-tolerance) || a < (b+tolerance)
  }

  @scala.annotation.tailrec
  def decimals(n: Double, sum: Int = 0): Int = {
    if (n > 1)
      sum
    else
      decimals(n*10, sum+1)
  }

  def testAutoIncrementStream(sample: Vector[Int], curElementCount: Int): Unit = {
    val realAverage = curElementCount/2
    val sampleAverage = sample.sum / sample.length

    val larger = math.max(realAverage, sampleAverage)
    val smaller = math.min(realAverage, sampleAverage)
    // should be within 15% of real average
    val comp = larger*0.85

    println(s"Real Average: $realAverage, Sample average: $sampleAverage")
    assert(smaller >= comp)

    // we also know that in this stream, the most recent element is just the index, so the sample *must* contain
    // curElementCount
    assert(sample.contains(curElementCount))
  }

  var lastElementCount = 0
  var lastSample = Vector[Int]()
  def verifySamples(sample: Vector[Int], curElementCount: Int): Unit = {
    if (lastSample.isEmpty) {
      lastSample = sample
      lastElementCount = curElementCount
    } else {
      val diff = lastSample.diff(sample)

      // should only be one different element in this sample from last sample
      assert(diff.length == 1)

      // samples happen with increasing rarity,
      // this test checks to see that the probability we sampled this item
      // and the distance between samples is related (with some tolerance for randomness)
      val estimatedChance = 1/(curElementCount - lastElementCount).toFloat
      val chance = sizeSample.toFloat / curElementCount
      assert(fuzzyEquals(chance, estimatedChance))
      lastElementCount = curElementCount
      lastSample = sample
    }
  }

  def dummyFunc(sample: Vector[Int], curElement: Int): Unit = {
    println("I don't do anything but ensure it calls all the functions in the list")
  }

  private val PRIME =  4294967311L
  // seed the  random number generator, so we have reproducible results
  private val rand = new scala.util.Random(PRIME)

  val lines = my_utils.getLines(filename).iterator

  reservoirSample.process(lines.map(_.toInt), sizeSample, rand, List[Standing_Query](verifySamples, dummyFunc))

  reservoirSample.process((1 to 400000).iterator, 100, rand, List[Standing_Query](testAutoIncrementStream))
}
