package streams

object CountingMain extends App {
  // process arguments
  if (args.size != 1) {
    args.foreach(println)
    throw new IllegalArgumentException(s"Arguments missing: <data path>. Tests will iterate all files immediately under this data directory.")
  }

  /*
      NOTE: Due to how the algorithm works, this can be slow on larger files
   */

  println("Testing Flajolet Martin distinct elements algorithm. NOTE: These tests may take a couple minutes.")

  val data_dir = args(0)
  val dirFile = new java.io.File(data_dir)
  assert(dirFile.exists(), s"The supplied data directory: '$data_dir' does not exist.")
  assert(dirFile.isDirectory, s"The supplied data directory: '$data_dir' is not a directory")

  val testFiles = dirFile.listFiles.filter(_.isFile)

  println(s"Found files:")
  testFiles.foreach(println(_))

  testFiles.foreach( file => {
    TestUtils.runTest(s"Testing file: $file, distinct estimation is close to reality",
      () => {
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

        // calculate exactly how many bits this count needs
        // so our estimations are correct
        val numBits = math.ceil(math.log10(actualUnique)/math.log10(2)).toInt
        val flajolet_Martin = new Flajolet_Martin(lines, numBits, utils.hashes)

        println(s"Hashes loaded. Took ${(System.currentTimeMillis() - now) / 1000} seconds.")

        val flaj_summary = flajolet_Martin.summarize(10)

        println(s"actual unique: $actualUnique -- flaj_summary: $flaj_summary")
        assert(flaj_summary <= 2*actualUnique, "Failed unique close to reality test. Flajolet Martin summary " +
          s"much larger than actual unique values: summary: $flaj_summary, reality: $actualUnique")
        assert(flaj_summary >= 0.5*actualUnique.toDouble, "Failed unique close to reality test. Flajolet Martin summary " +
          s"much smaller than actual unique values: summary: $flaj_summary, reality: $actualUnique")
      })
  })

  (1 to 10).foreach(numBits => {
    TestUtils.runTest(s"Testing that number of bits are respected with bits $numBits on '$data_dir/4096.ints'",
      () => {
        val lines = my_utils.getLines(s"$data_dir/4096.ints")
        val flajolet_Martin = new Flajolet_Martin(lines, numBits, utils.hashes)
        val flaj_summary = flajolet_Martin.summarize(10)
        assert(flaj_summary < math.pow(2, 16))
      })
  })

  println("Flajolet Martin: All tests passed.")
}
