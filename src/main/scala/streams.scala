import scala.collection.mutable.BitSet

package streams {

  class Bloom_Filter (fileName: String,
    falsePositiveRate: Double,
    hashes: List[Hash_Function]) {

    // leave this unchanged
    val bloomFilter : BitSet = BitSet()

    def parameters(): Filter_Parms = ???

    // your code goes here

    def in(v:String):Boolean = ???

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

