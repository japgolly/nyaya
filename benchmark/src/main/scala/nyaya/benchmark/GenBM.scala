package nyaya.benchmark

import nyaya.benchmark.GenBM._
import nyaya.gen._
import org.openjdk.jmh.annotations._

object GenBM {

  def bm[A](g: Gen[A], genSize: Int = 50, sampleSize: Int = 100): () => List[A] = {
    val gs = GenSize(genSize)
    () => g.withSeed(0).samples(gs).take(sampleSize).toList
  }

  val intSet = (1 to 200 by 3).toSet
  val intSetG = Gen pure intSet

  val char128Range  = 1.toChar to 128.toChar
  val char128Vector = char128Range.toVector
  val char128Array  = char128Vector.toArray

  val char30kRange  = 1.toChar to 30000.toChar
  val char30kVector = char30kRange.toVector
  val char30kArray  = char30kVector.toArray
}

@State(Scope.Benchmark)
class ChooseChar {

  val chooseChar128RangeBM = bm(Gen chooseIndexed_! char128Range)
  @Benchmark def chooseChar128Range = chooseChar128RangeBM()

  val chooseChar128VectorBM = bm(Gen chooseIndexed_! char128Vector)
  @Benchmark def chooseChar128Vector = chooseChar128VectorBM()

  val chooseChar128ArrayBM = bm(Gen chooseArray_! char128Array)
  @Benchmark def chooseChar128Array = chooseChar128ArrayBM()

  val chooseChar30kRangeBM = bm(Gen chooseIndexed_! char30kRange)
  @Benchmark def chooseChar30kRange = chooseChar30kRangeBM()

  val chooseChar30kVectorBM = bm(Gen chooseIndexed_! char30kVector)
  @Benchmark def chooseChar30kVector = chooseChar30kVectorBM()

  val chooseChar30kArrayBM = bm(Gen chooseArray_! char30kArray)
  @Benchmark def chooseChar30kArray = chooseChar30kArrayBM()
}

@State(Scope.Benchmark)
class ChooseIntBM {

  val chooseInt0BM = bm(Gen.chooseInt(0, 0x12345678))
  @Benchmark def chooseInt0 = chooseInt0BM()

  val chooseIntLBM = bm(Gen.chooseInt(0x02345678, 0x12345678))
  @Benchmark def chooseIntL = chooseIntLBM()
}

@State(Scope.Benchmark)
class GenBM {

  val mapByBM = bm(Gen.int mapBy Gen.int)
  @Benchmark def mapBy = mapByBM()

  val mapByKeySubsetBM = bm(Gen.int mapByKeySubset intSet)
  @Benchmark def mapByKeySubset = mapByKeySubsetBM()

  val mkStringBM = bm(Gen.pure('x').string1)
  @Benchmark def mkString = mkStringBM()

  val sizedSetBM = bm(Gen.int.sizedSet(20))
  @Benchmark def sizedSet = sizedSetBM()

  val subsetBM = bm(Gen subset intSet)
  @Benchmark def subset = subsetBM()

  val take20BM = bm(intSetG take 20)
  @Benchmark def take20 = take20BM()

  val vectorBM = bm(Gen.int.vector)
  @Benchmark def vector = vectorBM()
}
