package streams

object CountingMain extends App {
  // process arguments
  if (args.size != 2) {
    args.foreach(println)
    throw new IllegalArgumentException(s"Arguments missing <filename> <delimiter> <relativeSupportThreshold> [limit] ")
  }

  /*
      NOTE: Due to how the algorithm works, this can be slow on larger files
   */

  val fileDirectory = args(0)
  val numBits = args(1).toInt
  if (numBits < 1 || numBits > 32) {
    throw new IllegalArgumentException(s"numBits (arg2) must be between 1 and 32")
  }

  val testFiles = new java.io.File(fileDirectory).listFiles.filter(_.isFile)
  println(s"Found files:")
  testFiles.foreach(println(_))

  testFiles.foreach( file => {
    println(s"Testing file: $file")
    val actualUnique = my_utils.getLines(file.getAbsolutePath).foldLeft(Set[String]()) {
      case (accum, line) =>
        if (accum.contains(line))
          accum
        else
          accum + line
    }.size

    val now = System.currentTimeMillis()
    val lines = my_utils.getLines(file.getAbsolutePath).iterator
    println("Loading hashes...")
    val flajolet_Martin = new Flajolet_Martin(lines, numBits, utils.hashes)

    println(s"Hashes loaded. Took ${(System.currentTimeMillis() - now) / 1000} seconds.")

    val flaj_summary = flajolet_Martin.summarize(10)

    println(s"actual unique: $actualUnique -- flaj_summary: $flaj_summary")
    assert(flaj_summary <= 2*actualUnique)
  })
}
