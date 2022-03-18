package streams

object TestUtils {
  def runTest(testMsg: String, test: () => Unit): Unit = {
    val now = System.currentTimeMillis()
    println("--------------------")
    println(testMsg)
    test()

    // we assume the test will throw exception if it fails, if we get here it passed
    println(s"Test passed. Took ${(System.currentTimeMillis() - now).toFloat / 1000} seconds.")
    println("--------------------")
  }
}

object SampleMain extends App {

  // process arguments
  if (args.length != 1) {
    args.foreach(println)
    throw new IllegalArgumentException(s"Arguments missing <data_directory>. Tests will iterate all files immediately under this data directory.")
  }
  val data_dir: String = args(0)
  val dirFile = new java.io.File(data_dir)

  assert(dirFile.exists(), s"The supplied data directory: '$data_dir' does not exist.")
  assert(dirFile.isDirectory, s"The supplied data directory: '$data_dir' is not a directory")

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
    // should be within 20% of real average
    val comp = larger*0.80

//    println(s"Real Average: $realAverage, Sample average: $sampleAverage")
    assert(smaller >= comp, s"Average test failed. Expected $smaller >= $comp. Real average: $realAverage. Sample average: $sampleAverage")

    // we also know that in this stream, the most recent element is just the index, so the sample *must* contain
    // curElementCount
    assert(sample.contains(curElementCount))
  }

  var lastElementCount = 0
  var lastSample = Vector[Int]()
  val sizeSample = 100

  def verifySamples(sample: Vector[Int], curElementCount: Int): Unit = {
    if (lastSample.isEmpty) {
      lastSample = sample
      lastElementCount = curElementCount
    } else {
      val diff = lastSample.diff(sample)

      // should only be one different element in this sample from last sample
      assert(diff.length == 1, s"Single different element test failed. Expected diff.length == 1 but got ${diff.length}")

      // samples happen with increasing rarity,
      // this test checks to see that the probability we sampled this item
      // and the distance between samples is related (with some tolerance for randomness)
      val estimatedChance = 1/(curElementCount - lastElementCount).toFloat
      val chance = sizeSample.toFloat / curElementCount
      assert(fuzzyEquals(chance, estimatedChance), s"Fuzzy equals of sample chance failed. True chance: $chance, estimated chance: $estimatedChance")
      lastElementCount = curElementCount
      lastSample = sample
    }
  }

  var dummyFuncCount = 0
  def dummyFunc(sample: Vector[Int], curElement: Int): Unit = {
    dummyFuncCount += 1
  }

  private val PRIME =  4294967311L
  // seed the  random number generator, so we have reproducible results
  private val rand = new scala.util.Random(PRIME)

  val filenames: List[String] = List[String](data_dir + "/movies.ints", data_dir + "/4096.ints")

  filenames.foreach(filename => {
    val lines = my_utils.getLines(filename).iterator

    TestUtils.runTest("Testing stream sampling sanity and multiple standing queries. File: " +
      s"${filename}",
      () => reservoirSample.process(lines.map(_.toInt), sizeSample, rand, List[Standing_Query](verifySamples, dummyFunc)))

    // reset test mutables
    lastElementCount = 0
    lastSample = Vector[Int]()
  })

  (1 to 30).foreach(c => {
    TestUtils.runTest(s"Testing stream average sanity on auto-incrementing stream. test $c of 30",
      () => reservoirSample.process((1 to 400000).iterator, 150, rand, List[Standing_Query](testAutoIncrementStream)))
  })

  // test that a sample of sample size is the whole list (iterator is smaller than sample size)
  // function should only be called once
  TestUtils.runTest("Testing small sample sizes",
    () => {
      var count = 0
      reservoirSample.process((1 to 9).iterator, 10, rand, List[Standing_Query](
        (sample, sampleCount) => {
          assert(sampleCount == 9, s"Assert sample count on small sample failed. Expected sampleCount == 9, but got $sampleCount")
          assert(sample.equals(1 to 9), s"Assert sample format on small sample failed. " +
            s"Expected sample == ${1 to 9}, but got $sample")
          assert(count == 0, s"Assert only one call on small sample failed. This query should have only been called once.")
          count = count + 1
        }))
    })

  TestUtils.runTest("Testing empty iterator",
    () => reservoirSample.process(List().iterator, sizeSample=10, rand, List[Standing_Query](
      (_, _) => assert(false, "This function should not have been called")
    )))

  println("Reservoir Sampling: All tests passed.")
}
