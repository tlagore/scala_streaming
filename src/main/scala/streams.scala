import scala.collection.mutable.BitSet

package streams {

  /*
    case class Filter_Parms(
    nItemsSeen:Int, // number of items used to create the filter
    nBits:Int, // number of bits used in the filter
    nHashes:Int, // number of hashing functions used by the filter
    filter:BitSet   // the actual bitset
  )
   */

  class Bloom_Filter (fileName: String,
    falsePositiveRate: Double,
    hashes: List[Hash_Function]) {

    // leave this unchanged
    val bloomFilter : BitSet = BitSet()

    private val m = scala.io.Source.fromFile(fileName).getLines
      .foldLeft(0)((acc, _) => acc+1)

    private def calculate_optimal_hashes(falsePositiveRate: Double, numEls: Int): Double = {
      // optimal k = ln(p) / ln(1-e^-ln(2))
      math.ceil(math.log(falsePositiveRate) / math.log(1-math.pow(math.E, -math.log(2))))
    }

    private def calculate_optimal_buckets(numHashes: Int, numEls: Int): Double = {
      // optimal k = ln(p) / ln(1-e^-ln(2))
      math.ceil(utils.truncate((numHashes*numEls)/math.log(2), 7))
    }

    private val k = calculate_optimal_hashes(falsePositiveRate, m).toInt
    private val n = calculate_optimal_buckets(k, m).toInt

    private val lineIter = scala.io.Source.fromFile(fileName).getLines()
    private val hashesToUse = hashes.take(k)

    lineIter.foreach(element => {
      hashesToUse.foreach(hash => {
        val bucket = hash(element) % n
        bloomFilter.add(bucket)
      })
    })

    println(s"m=$m  k=$k and n=$n")

    def parameters(): Filter_Parms = Filter_Parms(m, n, k, bloomFilter)

    def in(v:String):Boolean = {
      hashesToUse.forall(hash => bloomFilter(hash(v) % n))
    }
  }
 
  class Flajolet_Martin(
    s : Iterator[String],
    bits: Int,
    hashes: List[Hash_Function]) {
    
    // your code goes here

    val hashCounts: List[Int] =  ???

    def summarize(groupSize:Int):Double = ???


  }

  object reservoirSample {
  
    def process(
      s : Iterator[Int],
      sizeSample: Int,
      r: scala.util.Random,
      queries: List[Standing_Query]
    )  = ???
    
    // your code goes here

      // remember that the algorithm assumes
      // that the position of the element in the stream
      // starts at zero (not 1)
      // thus is the stream is 10,20,30
      // element 1 is 10, element 2 is 20 and element 3 is 30

      // use r.nextDouble to generate a random number between [0,1)
      // use r.nextInt(n) to generate a number between 0 and n-1

      
  }

}

