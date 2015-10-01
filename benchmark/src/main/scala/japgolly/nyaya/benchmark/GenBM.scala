package japgolly.nyaya.benchmark

import org.openjdk.jmh.annotations._
import japgolly.nyaya.test._
import SizeSpec.Exactly
import GenBM._

object GenBM {

  def bm[A](g: Gen[A], genSize: Int = 50, sampleSize: Int = 100): () => List[A] = {
    val gs = GenSize(genSize)
    () => g.samples(GenCtx(gs, 0), sampleSize).toList
  }

  val intSet = (1 to 200 by 3).toSet
  val intSetG = Gen pure intSet

  val char128Range  = (1.toChar to 128.toChar)
  val char128Vector = char128Range.toVector
  val char128Array  = char128Vector.toArray

  val char30kRange  = (1.toChar to 30000.toChar)
  val char30kVector = char30kRange.toVector
  val char30kArray  = char30kVector.toArray
}

@State(Scope.Benchmark)
class ChooseChar {
  /*
  [info] Benchmark                                                    Mode  Cnt       Score       Error   Units
  [info] GenBM.chooseChar128Array                                    thrpt  200  733394.100 ±  7087.381   ops/s
  [info] GenBM.chooseChar128Array:·gc.alloc.rate                     thrpt  200    1818.108 ±    17.565  MB/sec
  [info] GenBM.chooseChar128Array:·gc.alloc.rate.norm                thrpt  200    2600.001 ±     0.001    B/op
  [info] GenBM.chooseChar128Array:·gc.churn.PS_Eden_Space            thrpt  200    1796.088 ±    67.154  MB/sec
  [info] GenBM.chooseChar128Array:·gc.churn.PS_Eden_Space.norm       thrpt  200    2568.830 ±    92.801    B/op
  [info] GenBM.chooseChar128Array:·gc.churn.PS_Survivor_Space        thrpt  200       0.071 ±     0.011  MB/sec
  [info] GenBM.chooseChar128Array:·gc.churn.PS_Survivor_Space.norm   thrpt  200       0.101 ±     0.016    B/op
  [info] GenBM.chooseChar128Array:·gc.count                          thrpt  200     823.000              counts
  [info] GenBM.chooseChar128Array:·gc.time                           thrpt  200    1264.000                  ms
  [info] GenBM.chooseChar128Range                                    thrpt  200  235000.505 ±  3061.021   ops/s
  [info] GenBM.chooseChar128Range:·gc.alloc.rate                     thrpt  200    1659.837 ±    21.629  MB/sec
  [info] GenBM.chooseChar128Range:·gc.alloc.rate.norm                thrpt  200    7408.002 ±     0.001    B/op
  [info] GenBM.chooseChar128Range:·gc.churn.PS_Eden_Space            thrpt  200    1650.624 ±    57.642  MB/sec
  [info] GenBM.chooseChar128Range:·gc.churn.PS_Eden_Space.norm       thrpt  200    7361.455 ±   230.841    B/op
  [info] GenBM.chooseChar128Range:·gc.churn.PS_Survivor_Space        thrpt  200       0.068 ±     0.010  MB/sec
  [info] GenBM.chooseChar128Range:·gc.churn.PS_Survivor_Space.norm   thrpt  200       0.307 ±     0.047    B/op
  [info] GenBM.chooseChar128Range:·gc.count                          thrpt  200     870.000              counts
  [info] GenBM.chooseChar128Range:·gc.time                           thrpt  200    1354.000                  ms
  [info] GenBM.chooseChar128Vector                                   thrpt  200  614523.153 ± 21307.092   ops/s
  [info] GenBM.chooseChar128Vector:·gc.alloc.rate                    thrpt  200    1523.731 ±    52.640  MB/sec
  [info] GenBM.chooseChar128Vector:·gc.alloc.rate.norm               thrpt  200    2600.801 ±     0.568    B/op
  [info] GenBM.chooseChar128Vector:·gc.churn.PS_Eden_Space           thrpt  200    1523.718 ±    68.957  MB/sec
  [info] GenBM.chooseChar128Vector:·gc.churn.PS_Eden_Space.norm      thrpt  200    2602.014 ±    75.436    B/op
  [info] GenBM.chooseChar128Vector:·gc.churn.PS_Survivor_Space       thrpt  200       0.074 ±     0.010  MB/sec
  [info] GenBM.chooseChar128Vector:·gc.churn.PS_Survivor_Space.norm  thrpt  200       0.132 ±     0.020    B/op
  [info] GenBM.chooseChar128Vector:·gc.count                         thrpt  200     911.000              counts
  [info] GenBM.chooseChar128Vector:·gc.time                          thrpt  200    1355.000                  ms
  [info] GenBM.chooseChar30kArray                                    thrpt  200  596279.593 ±  8519.257   ops/s
  [info] GenBM.chooseChar30kArray:·gc.alloc.rate                     thrpt  200    2387.891 ±    34.121  MB/sec
  [info] GenBM.chooseChar30kArray:·gc.alloc.rate.norm                thrpt  200    4200.001 ±     0.001    B/op
  [info] GenBM.chooseChar30kArray:·gc.churn.PS_Eden_Space            thrpt  200    2383.405 ±    69.514  MB/sec
  [info] GenBM.chooseChar30kArray:·gc.churn.PS_Eden_Space.norm       thrpt  200    4194.831 ±   112.548    B/op
  [info] GenBM.chooseChar30kArray:·gc.churn.PS_Survivor_Space        thrpt  200       0.082 ±     0.011  MB/sec
  [info] GenBM.chooseChar30kArray:·gc.churn.PS_Survivor_Space.norm   thrpt  200       0.145 ±     0.021    B/op
  [info] GenBM.chooseChar30kArray:·gc.count                          thrpt  200     846.000              counts
  [info] GenBM.chooseChar30kArray:·gc.time                           thrpt  200    1327.000                  ms
  [info] GenBM.chooseChar30kRange                                    thrpt  200  215945.687 ±  4688.179   ops/s
  [info] GenBM.chooseChar30kRange:·gc.alloc.rate                     thrpt  200    2513.469 ±    54.446  MB/sec
  [info] GenBM.chooseChar30kRange:·gc.alloc.rate.norm                thrpt  200   12207.202 ±     0.568    B/op
  [info] GenBM.chooseChar30kRange:·gc.churn.PS_Eden_Space            thrpt  200    2504.975 ±    82.104  MB/sec
  [info] GenBM.chooseChar30kRange:·gc.churn.PS_Eden_Space.norm       thrpt  200   12161.946 ±   282.137    B/op
  [info] GenBM.chooseChar30kRange:·gc.churn.PS_Survivor_Space        thrpt  200       0.076 ±     0.010  MB/sec
  [info] GenBM.chooseChar30kRange:·gc.churn.PS_Survivor_Space.norm   thrpt  200       0.375 ±     0.054    B/op
  [info] GenBM.chooseChar30kRange:·gc.count                          thrpt  200     871.000              counts
  [info] GenBM.chooseChar30kRange:·gc.time                           thrpt  200    1351.000                  ms
  [info] GenBM.chooseChar30kVector                                   thrpt  200  505884.120 ±  5925.184   ops/s
  [info] GenBM.chooseChar30kVector:·gc.alloc.rate                    thrpt  200    1254.080 ±    14.720  MB/sec
  [info] GenBM.chooseChar30kVector:·gc.alloc.rate.norm               thrpt  200    2600.001 ±     0.001    B/op
  [info] GenBM.chooseChar30kVector:·gc.churn.PS_Eden_Space           thrpt  200    1245.397 ±    38.840  MB/sec
  [info] GenBM.chooseChar30kVector:·gc.churn.PS_Eden_Space.norm      thrpt  200    2581.198 ±    72.722    B/op
  [info] GenBM.chooseChar30kVector:·gc.churn.PS_Survivor_Space       thrpt  200       0.069 ±     0.010  MB/sec
  [info] GenBM.chooseChar30kVector:·gc.churn.PS_Survivor_Space.norm  thrpt  200       0.143 ±     0.022    B/op
  [info] GenBM.chooseChar30kVector:·gc.count                         thrpt  200     899.000              counts
  [info] GenBM.chooseChar30kVector:·gc.time                          thrpt  200    1394.000                  ms
  */

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
  /*
  [info] Benchmark                                                    Mode  Cnt       Score       Error   Units
  [info] GenBM.chooseInt0                                            thrpt  200  599073.853 ±  7518.607   ops/s
  [info] GenBM.chooseInt0:·gc.alloc.rate                             thrpt  200    2399.082 ±    30.111  MB/sec
  [info] GenBM.chooseInt0:·gc.alloc.rate.norm                        thrpt  200    4200.001 ±     0.001    B/op
  [info] GenBM.chooseInt0:·gc.churn.PS_Eden_Space                    thrpt  200    2399.574 ±    70.878  MB/sec
  [info] GenBM.chooseInt0:·gc.churn.PS_Eden_Space.norm               thrpt  200    4199.509 ±   108.836    B/op
  [info] GenBM.chooseInt0:·gc.churn.PS_Survivor_Space                thrpt  200       0.077 ±     0.010  MB/sec
  [info] GenBM.chooseInt0:·gc.churn.PS_Survivor_Space.norm           thrpt  200       0.136 ±     0.019    B/op
  [info] GenBM.chooseInt0:·gc.count                                  thrpt  200     924.000              counts
  [info] GenBM.chooseInt0:·gc.time                                   thrpt  200    1396.000                  ms
  [info] GenBM.chooseIntL                                            thrpt  200  555398.010 ±  6518.394   ops/s
  [info] GenBM.chooseIntL:·gc.alloc.rate                             thrpt  200    2224.131 ±    26.101  MB/sec
  [info] GenBM.chooseIntL:·gc.alloc.rate.norm                        thrpt  200    4200.001 ±     0.001    B/op
  [info] GenBM.chooseIntL:·gc.churn.PS_Eden_Space                    thrpt  200    2199.230 ±    69.453  MB/sec
  [info] GenBM.chooseIntL:·gc.churn.PS_Eden_Space.norm               thrpt  200    4152.859 ±   120.244    B/op
  [info] GenBM.chooseIntL:·gc.churn.PS_Survivor_Space                thrpt  200       0.082 ±     0.011  MB/sec
  [info] GenBM.chooseIntL:·gc.churn.PS_Survivor_Space.norm           thrpt  200       0.157 ±     0.022    B/op
  [info] GenBM.chooseIntL:·gc.count                                  thrpt  200     898.000              counts
  [info] GenBM.chooseIntL:·gc.time                                   thrpt  200    1379.000                  ms
  */

  val chooseInt0BM = bm(Gen.chooseInt(0, 0x12345678))
  @Benchmark def chooseInt0 = chooseInt0BM()

  val chooseIntLBM = bm(Gen.chooseInt(0x02345678, 0x12345678))
  @Benchmark def chooseIntL = chooseIntLBM()
}

@State(Scope.Benchmark)
class GenBM {
/*
[info] Benchmark                                                    Mode  Cnt       Score       Error   Units
[info] GenBM.mapBy                                                 thrpt  200    4451.500 ±    50.047   ops/s
[info] GenBM.mapBy:·gc.alloc.rate                                  thrpt  200    1891.851 ±    21.269  MB/sec
[info] GenBM.mapBy:·gc.alloc.rate.norm                             thrpt  200  445720.106 ±     0.002    B/op
[info] GenBM.mapBy:·gc.churn.PS_Eden_Space                         thrpt  200    1879.025 ±    65.233  MB/sec
[info] GenBM.mapBy:·gc.churn.PS_Eden_Space.norm                    thrpt  200  442686.965 ± 14489.564    B/op
[info] GenBM.mapBy:·gc.churn.PS_Survivor_Space                     thrpt  200       0.169 ±     0.029  MB/sec
[info] GenBM.mapBy:·gc.churn.PS_Survivor_Space.norm                thrpt  200      39.810 ±     6.882    B/op
[info] GenBM.mapBy:·gc.count                                       thrpt  200     847.000              counts
[info] GenBM.mapBy:·gc.time                                        thrpt  200    1336.000                  ms
[info] GenBM.mapByKeySubset                                        thrpt  200    3047.655 ±    29.496   ops/s
[info] GenBM.mapByKeySubset:·gc.alloc.rate                         thrpt  200    1622.409 ±    15.704  MB/sec
[info] GenBM.mapByKeySubset:·gc.alloc.rate.norm                    thrpt  200  558312.463 ±     0.356    B/op
[info] GenBM.mapByKeySubset:·gc.churn.PS_Eden_Space                thrpt  200    1618.359 ±    53.541  MB/sec
[info] GenBM.mapByKeySubset:·gc.churn.PS_Eden_Space.norm           thrpt  200  556775.487 ± 17368.173    B/op
[info] GenBM.mapByKeySubset:·gc.churn.PS_Survivor_Space            thrpt  200       0.197 ±     0.031  MB/sec
[info] GenBM.mapByKeySubset:·gc.churn.PS_Survivor_Space.norm       thrpt  200      67.969 ±    10.506    B/op
[info] GenBM.mapByKeySubset:·gc.count                              thrpt  200     938.000              counts
[info] GenBM.mapByKeySubset:·gc.time                               thrpt  200    1375.000                  ms
[info] GenBM.mkString                                              thrpt  200  187826.176 ±  2334.137   ops/s
[info] GenBM.mkString:·gc.alloc.rate                               thrpt  200    3331.084 ±    41.398  MB/sec
[info] GenBM.mkString:·gc.alloc.rate.norm                          thrpt  200   18600.002 ±     0.001    B/op
[info] GenBM.mkString:·gc.churn.PS_Eden_Space                      thrpt  200    3303.757 ±   101.038  MB/sec
[info] GenBM.mkString:·gc.churn.PS_Eden_Space.norm                 thrpt  200   18443.954 ±   510.071    B/op
[info] GenBM.mkString:·gc.churn.PS_Survivor_Space                  thrpt  200       0.089 ±     0.013  MB/sec
[info] GenBM.mkString:·gc.churn.PS_Survivor_Space.norm             thrpt  200       0.501 ±     0.074    B/op
[info] GenBM.mkString:·gc.count                                    thrpt  200     905.000              counts
[info] GenBM.mkString:·gc.time                                     thrpt  200    1442.000                  ms
[info] GenBM.subset                                                thrpt  200    3653.056 ±    39.171   ops/s
[info] GenBM.subset:·gc.alloc.rate                                 thrpt  200    1635.903 ±    17.541  MB/sec
[info] GenBM.subset:·gc.alloc.rate.norm                            thrpt  200  469661.729 ±     1.705    B/op
[info] GenBM.subset:·gc.churn.PS_Eden_Space                        thrpt  200    1638.344 ±    47.901  MB/sec
[info] GenBM.subset:·gc.churn.PS_Eden_Space.norm                   thrpt  200  470292.985 ± 12615.323    B/op
[info] GenBM.subset:·gc.churn.PS_Survivor_Space                    thrpt  200       0.144 ±     0.023  MB/sec
[info] GenBM.subset:·gc.churn.PS_Survivor_Space.norm               thrpt  200      41.258 ±     6.690    B/op
[info] GenBM.subset:·gc.count                                      thrpt  200     946.000              counts
[info] GenBM.subset:·gc.time                                       thrpt  200    1404.000                  ms
[info] GenBM.take20                                                thrpt  200    3533.019 ±    44.259   ops/s
[info] GenBM.take20:·gc.alloc.rate                                 thrpt  200    1163.691 ±    14.629  MB/sec
[info] GenBM.take20:·gc.alloc.rate.norm                            thrpt  200  345440.134 ±   151.534    B/op
[info] GenBM.take20:·gc.churn.PS_Eden_Space                        thrpt  200    1160.482 ±    40.422  MB/sec
[info] GenBM.take20:·gc.churn.PS_Eden_Space.norm                   thrpt  200  344498.587 ± 11253.893    B/op
[info] GenBM.take20:·gc.churn.PS_Survivor_Space                    thrpt  200       0.095 ±     0.016  MB/sec
[info] GenBM.take20:·gc.churn.PS_Survivor_Space.norm               thrpt  200      28.342 ±     4.723    B/op
[info] GenBM.take20:·gc.count                                      thrpt  200     778.000              counts
[info] GenBM.take20:·gc.time                                       thrpt  200    1272.000                  ms
[info] GenBM.vector                                                thrpt  200   23950.304 ±   256.589   ops/s
[info] GenBM.vector:·gc.alloc.rate                                 thrpt  200    1893.402 ±    20.284  MB/sec
[info] GenBM.vector:·gc.alloc.rate.norm                            thrpt  200   82912.019 ±     0.001    B/op
[info] GenBM.vector:·gc.churn.PS_Eden_Space                        thrpt  200    1881.617 ±    67.332  MB/sec
[info] GenBM.vector:·gc.churn.PS_Eden_Space.norm                   thrpt  200   82427.243 ±  2867.595    B/op
[info] GenBM.vector:·gc.churn.PS_Survivor_Space                    thrpt  200       0.098 ±     0.015  MB/sec
[info] GenBM.vector:·gc.churn.PS_Survivor_Space.norm               thrpt  200       4.300 ±     0.663    B/op
[info] GenBM.vector:·gc.count                                      thrpt  200     774.000              counts
[info] GenBM.vector:·gc.time                                       thrpt  200    1366.000                  ms
 */

  val mapByBM = bm(Gen.int mapBy Gen.int)
  @Benchmark def mapBy = mapByBM()

  val mapByKeySubsetBM = bm(Gen.int mapByKeySubset intSet)
  @Benchmark def mapByKeySubset = mapByKeySubsetBM()

  val mkStringBM = bm(Gen.pure('x').string1)
  @Benchmark def mkString = mkStringBM()

  val subsetBM = bm(Gen subset intSet)
  @Benchmark def subset = subsetBM()

  val take20BM = bm(intSetG take 20)
  @Benchmark def take20 = take20BM()

  val vectorBM = bm(Gen.int.vector)
  @Benchmark def vector = vectorBM()
}
