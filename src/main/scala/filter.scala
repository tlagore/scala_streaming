package streams

object FilterMain extends App {
  val random = new scala.util.Random()

  /*
    NOTE ON THESE TESTS: Due to working with probabilities, these tests have the potential to fail on a test or 2.
      - It should pass most runs.
      - Of course if this were production, we would not have flaky tests ;)
   */

  def createRandomInt(size: Int): String = {
    (1 to size).foldLeft("")((str, _) =>
      str + ('0'.toInt + random.nextInt(10)).toChar
    )
  }

  def createRandomString(size: Int): String = {
    (1 to size).foldLeft("")((str, _) =>
      str + ('a'.toInt + random.nextInt(26)).toChar
    )
  }

  // process arguments
  if (args.size != 1) {
    args.foreach(println)
    throw new IllegalArgumentException(s"Required arguments: <data_path>")
  }
  val data_path = args(0)
  val testFiles = new java.io.File("data/").listFiles.filter(_.isFile)

  val minFalsePos = 0.03
  val maxFalsePos = 0.15

  testFiles.foreach(file => {
    val filename = file.getAbsolutePath
    println(s"testing $filename")
    val falsePositiveRate = random.nextDouble() * (maxFalsePos-minFalsePos)+minFalsePos
    val filter = new Bloom_Filter(filename, falsePositiveRate, utils.hashes)

    val (numMatched, count) = my_utils.getLines(filename).foldLeft((0, 0)) {
      case ((numMatched, count), line) =>
        if (filter.in(line))
          (numMatched + 1, count + 1)
        else
          (numMatched, count + 1)
    }

    // ensure we have 100% correct rate for elements in the file
    assert(numMatched == count)

    println(s"numMatched: $numMatched -- count: $count")

    println("doing random tests")
    val numberTests = 1000000
    val falsePositives = (1 to numberTests).foldLeft(0)((falsePositives, _) => {
      // +1 on string size so we don't have 0 length string
      val str = if (random.nextDouble < 0.5) createRandomString(random.nextInt(35) + 1) else createRandomInt(random.nextInt(35) + 1)
      if (filter.in(str))
        falsePositives + 1
      else
        falsePositives
    })

    val actualFalsePositiveRate = falsePositives.toDouble / numberTests
    println(s"$falsePositives false positives, FP rate for tests: ${actualFalsePositiveRate}, Specified filter false positive rate: ${falsePositiveRate}")

    // Ensure our actual false positive is no more than 5% higher than specified positive rate
    // unless it's a small file filter, then it may have up to 8% more false positives
    val wiggleRoom = if (count < 100) 0.08 else 0.05
    assert(actualFalsePositiveRate < (falsePositiveRate + wiggleRoom))
  })
}
