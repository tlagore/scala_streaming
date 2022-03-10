package streams

object FilterMain extends App {
  val random = new scala.util.Random()

  def createRandomString(size: Int): String = {
    (1 to size).foldLeft("")((str, _) =>
      str + ('a'.toInt + random.nextInt(26)).toChar
    )
  }

  // process arguments
  if (args.size != 2) {
    args.foreach(println)
    throw new IllegalArgumentException(s"Arguments missing <filename> <delimiter> <relativeSupportThreshold> [limit] ")
  }
  val filename = args(0)
  val falsePositiveRate = args(1).toDouble
  if (falsePositiveRate < 0 || falsePositiveRate > 1) {
    throw new IllegalArgumentException(s"False positive rate (arg2) must be between 0 and 1")
  }
  val filter = new Bloom_Filter(filename, falsePositiveRate, utils.hashes)

  val (numMatched, count) = my_utils.getLines(filename).foldLeft((0,0)) {
    case ((numMatched, count), line) =>
      if (filter.in(line))
        (numMatched + 1, count + 1)
      else
        (numMatched, count+1)
  }

  println(s"numMatched: $numMatched -- count: $count")

  println("doing random tests")
  val numberTests = 1000000
  val falsePositives = (1 to numberTests).foldLeft(0)((falsePositives, _) => {
    val str = createRandomString(random.nextInt(15)+1)
    if (filter.in(str))
      falsePositives + 1
    else
      falsePositives
  })
  println(s"$falsePositives false positives, FP rate: ${falsePositives.toDouble/numberTests}")
}
