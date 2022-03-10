package streams

import scala.io.Source

object CountingMain extends App {
  // process arguments
  if (args.size != 2) {
    args.foreach(println)
    throw new IllegalArgumentException(s"Arguments missing <filename> <delimiter> <relativeSupportThreshold> [limit] ")
  }

  val filename = args(0)
  val numBits = args(1).toInt
  if (numBits < 1 || numBits > 32) {
    throw new IllegalArgumentException(s"numBits (arg2) must be between 1 and 32")
  }

  val actualUnique = my_utils.getLines(filename).foldLeft(Set[String]()) {
    case (accum, line) =>
      if (accum.contains(line))
        accum
      else
        accum + line
  }.size

  val lines = my_utils.getLines(filename).iterator
  val flajolet_Martin = new Flajolet_Martin(lines, numBits, utils.hashes)
  val flaj_summary = flajolet_Martin.summarize(10)

  println(s"actual unique: $actualUnique -- flaj_summary: $flaj_summary")
}
