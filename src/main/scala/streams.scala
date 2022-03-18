import scala.collection.mutable.BitSet


/* This import is not used because assignment specs say we can't import
anything, but uncomment it if you want to
test the parallelized version (mucho zoom)
*/
// import scala.collection.parallel.CollectionConverters._

package streams {

  object my_utils {
    def getLines(filename:String):Iterator[String] =
    {
      scala.io.Source.fromFile(filename).getLines()
//      scala.util.Using.resource(new java.io.BufferedReader(new java.io.FileReader(filename))) { reader =>
//        Iterator.continually(reader.readLine()).takeWhile(_ != null).toSeq
//      }
    }
  }

  /**
   * Implements the Bloom Filter algorithm
   *
   * Given a file path to a list of filter elements and an acceptable false positive rate, calculate the optimal
   * parameters for a bloom filter, and populate the filter.
   *
   * @param fileName file path to the filter elements
   * @param falsePositiveRate acceptable false positive percentage
   * @param hashes list of hashes to use for the algorithm
   */
  class Bloom_Filter (fileName: String,
    falsePositiveRate: Double,
    hashes: List[Hash_Function]) {

    // leave this unchanged
    val bloomFilter : BitSet = BitSet()

    private val m = my_utils.getLines(fileName)
      .foldLeft(0)((acc, _) => acc+1)

    // optimal k = ln(p) / ln(1-e^-ln(2))
    private def calculate_optimal_hashes(falsePositiveRate: Double): Double = {
      math.ceil(math.log(falsePositiveRate) / math.log(1-math.pow(math.E, -math.log(2))))
    }

    // optimal n = (m*ln(p)) / (ln(2)*ln(.5))
    private def calculate_optimal_buckets(falsePositiveRate: Double, numEls: Int): Double = {
      val optimal_n = (numEls.toDouble*math.log(falsePositiveRate))/(math.log(2)*math.log(0.5))
      math.ceil(utils.truncate(optimal_n, 7))
    }

    private val k = calculate_optimal_hashes(falsePositiveRate).toInt
    private val n = calculate_optimal_buckets(falsePositiveRate, m).toInt

    private val lineIter = my_utils.getLines(fileName)

    private val hashesToUse = hashes.take(k)

    lineIter.foreach(element => {
      hashesToUse.foreach(hash => {
        val bucket = hash(element) % n

        // mutable - gross
        bloomFilter.add(bucket)
      })
    })

    def parameters(): Filter_Parms = Filter_Parms(m, n, k, bloomFilter)

    println(s"n=$n, k=$k, m=$m, false positive = $falsePositiveRate")

    /**
     * Check if an element is in the bloom filter.
     *
     * Will be 100% accurate on elements that are allowed. Will have a false positive rate with the approximate
     * amount supplied to the constructor.
     * @param v Element to check the bloom filter for membership
     * @return true if the element is allowed by the filter, false otherwise
     */
    def in(v:String):Boolean = {
      hashesToUse.forall(hash => bloomFilter(hash(v) % n))
    }
  }

  /**
   * Implements the Flajolet Martin algorithm
   *
   * @param s iterator to the stream elements
   * @param bits number of bits to hash the elements to
   * @param hashes a list of hash functions to use for the algorithm
   */
  class Flajolet_Martin(
    s : Iterator[String],
    bits: Int,
    hashes: List[Hash_Function]) {

    private val nBits: Int = bits

    /**
     * Get number of least significant bits that match the bitType
     *
     * Checks if the LSB is 'bitType' (0 or 1) by bitwise anding with 1 and comparing the bit,
     * if it is 'bitType' then we add 1 to the length and right shift n by 1 bit
     *
     * Stop when we hit a 1
     * @param n the value we are checking the tailLength for
     * @param bitType 0 or 1, if set to 0 it will check for the tailLength of 0's, and vice versa
     * @param length recursive parameter, should not be set
     * @return tailLength of n
     */
    @scala.annotation.tailrec
    private def tailLength(n:Int, bitType: Int, length: Int = 0): Int = {
      // if the value is 0, the element hashed to exactly 0 (after masking)
      if (n == 0)
        nBits
      else if ((n & 0x1) == bitType)
        tailLength(n >> 1, bitType, length+1)
      else
        length
    }

    // create a bitmask to truncate our values to specified bit length
    // note: bitwise and of 2^n-1 is the same as modulo 2^n
    private val bitMask = (math.pow(2, bits) - 1).toInt

    println(s"Bitmask is: $bitMask")

    def reduceHashes(l1: List[Double], l2: List[Double]): List[Double] = {
      l1.zip(l2).map {
        case (el1, el2) => math.max(el1, el2)
      }
    }

    // ---------------------------------------------------------------------------------
    // if you want to test the parallelized version, uncomment this code and the import
    // at the top of the file, and comment out the next code chunk
    // ---------------------------------------------------------------------------------

    // parallelized version, not allowed due to assignment specs saying no import
//    private[this] val hashCounts: List[Double] = s.sliding(150,150).to(LazyList).par.map(s => {
//      s.map(el => {
//        hashes.map(_ (el) & bitMask)
//          .map(tailLength(_, 0))
//          .map(math.pow(2, _))
//      }).fold(List.fill(hashes.length)(0.0))(reduceHashes)
//    }).reduce(reduceHashes)

    // ---------------------------------------------------------------------------------
    // if you want to test the parallelized version, comment out this block and uncomment
    // the above block and import at the top of the file
    // ---------------------------------------------------------------------------------
    private[this] val hashCounts: List[Double] = s.foldLeft(List.fill(hashes.length)(0.0))((curCounts, el) => {
        val newCounts = hashes
          .map(_(el) & bitMask)
          .map(tailLength(_,0))
          .map(math.pow(2, _))

        reduceHashes(curCounts, newCounts)
      })

    /**
     * Summarize the distinct counts by groupSize
     * @param groupSize size of the groups to average over
     * @return estimation of the number of distinct elements
     */
    def summarize(groupSize:Int):Double =
    {
      // median then avg (as in class), text does avg then median
      val medians = hashCounts.sliding(groupSize, groupSize)
        .foldLeft(List[Double]())((intermediaryMedian, group) => {
          val (l1,l2) = group.sortWith(_ < _).splitAt(group.length/2)
          if (l1.length == l2.length)
            intermediaryMedian:+ (l1.last + l2.head) / 2
          else
            intermediaryMedian :+ l2.head
        })

      medians.sum / medians.length
    }
  }

  /**
   * Implements resevoir sampling
   */
  object reservoirSample {

    /**
     * processes a stream for resevoir sampling
     * @param s iterator to the stream elements
     * @param sizeSample size of a sample
     * @param r a seeded random generator for the sampling
     * @param queries a list of queries to perform on each sample
     */
    def process(
      s : Iterator[Int],
      sizeSample: Int,
      r: scala.util.Random,
      queries: List[Standing_Query]
    ): Unit = {
      // your code goes here
      val (sampleWithIndex, stream) = s.zipWithIndex.splitAt(sizeSample)
      val initialSample: Vector[Int] = sampleWithIndex.map(_._1).toVector

      // initial query on first sizeSample samples
      if (initialSample.nonEmpty)
        queries.foreach(_(initialSample, initialSample.length))

      // index + 1 since zipWithIndex is 0 based
      stream.filter({
        case (_, index) =>
          r.nextDouble() < (sizeSample.toDouble/(index+1))
      }).foldLeft(initialSample)
      {
        case(sample, (element, curSample)) =>
          val index = r.nextInt(sizeSample)

          // replace vector index
          val newSample: Vector[Int] = sample.zipWithIndex.map {
              case (el, idx) => if (idx == index) element else el
          }

          queries.foreach(_(newSample, curSample+1))
          newSample
      }
    }
  }

}

