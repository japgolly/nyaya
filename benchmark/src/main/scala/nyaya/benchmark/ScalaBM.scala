package nyaya.benchmark

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class MakeVector1BM {
  /*
  [info] Benchmark                                       Mode  Cnt        Score       Error   Units
  [info] mkVec1_2_b                                     thrpt  200  3776159.438 ±  3952.096   ops/s
  [info] mkVec1_2_e1n                                   thrpt  200  2796367.211 ±  5988.536   ops/s
  [info] mkVec1_2_en1                                   thrpt  200  2804714.821 ±  6460.150   ops/s
  [info] mkVec1_2_n1                                    thrpt  200  2296352.866 ±  5464.419   ops/s

  [info] mkVec1_4_b                                     thrpt  200  3469438.172 ±  2425.216   ops/s
  [info] mkVec1_4_e1n                                   thrpt  200  1560992.387 ± 14420.334   ops/s
  [info] mkVec1_4_en1                                   thrpt  200  1571285.480 ±  5551.518   ops/s
  [info] mkVec1_4_n1                                    thrpt  200  2103884.572 ±  5187.166   ops/s

  [info] mkVec1_8_b                                     thrpt  200  2932393.888 ±  2032.306   ops/s
  [info] mkVec1_8_e1n                                   thrpt  200  1423695.694 ±  5866.048   ops/s
  [info] mkVec1_8_en1                                   thrpt  200  1465745.921 ±  9565.937   ops/s
  [info] mkVec1_8_n1                                    thrpt  200  1785399.006 ±  4347.964   ops/s

  [info] # Run complete. Total time: 01:21:28
  [info]
  [info] Benchmark                                       Mode  Cnt        Score       Error   Units
  [info] mkVec1_2_b                                     thrpt  200  3776159.438 ±  3952.096   ops/s
  [info] mkVec1_2_b:·gc.alloc.rate                      thrpt  200      948.886 ±     1.045  MB/sec
  [info] mkVec1_2_b:·gc.alloc.rate.norm                 thrpt  200      264.000 ±     0.001    B/op
  [info] mkVec1_2_b:·gc.churn.PS_Eden_Space             thrpt  200      948.166 ±     6.117  MB/sec
  [info] mkVec1_2_b:·gc.churn.PS_Eden_Space.norm        thrpt  200      263.804 ±     1.712    B/op
  [info] mkVec1_2_b:·gc.churn.PS_Survivor_Space         thrpt  200        0.022 ±     0.004  MB/sec
  [info] mkVec1_2_b:·gc.churn.PS_Survivor_Space.norm    thrpt  200        0.006 ±     0.001    B/op
  [info] mkVec1_2_b:·gc.count                           thrpt  200     2880.000              counts
  [info] mkVec1_2_b:·gc.time                            thrpt  200     2042.000                  ms
  [info] mkVec1_2_e1n                                   thrpt  200  2796367.211 ±  5988.536   ops/s
  [info] mkVec1_2_e1n:·gc.alloc.rate                    thrpt  200     1150.522 ±     2.480  MB/sec
  [info] mkVec1_2_e1n:·gc.alloc.rate.norm               thrpt  200      432.000 ±     0.001    B/op
  [info] mkVec1_2_e1n:·gc.churn.PS_Eden_Space           thrpt  200     1150.641 ±     7.973  MB/sec
  [info] mkVec1_2_e1n:·gc.churn.PS_Eden_Space.norm      thrpt  200      432.041 ±     2.814    B/op
  [info] mkVec1_2_e1n:·gc.churn.PS_Survivor_Space       thrpt  200        0.030 ±     0.005  MB/sec
  [info] mkVec1_2_e1n:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.011 ±     0.002    B/op
  [info] mkVec1_2_e1n:·gc.count                         thrpt  200     2815.000              counts
  [info] mkVec1_2_e1n:·gc.time                          thrpt  200     2041.000                  ms
  [info] mkVec1_2_en1                                   thrpt  200  2804714.821 ±  6460.150   ops/s
  [info] mkVec1_2_en1:·gc.alloc.rate                    thrpt  200     1153.845 ±     2.714  MB/sec
  [info] mkVec1_2_en1:·gc.alloc.rate.norm               thrpt  200      432.000 ±     0.001    B/op
  [info] mkVec1_2_en1:·gc.churn.PS_Eden_Space           thrpt  200     1152.515 ±     9.014  MB/sec
  [info] mkVec1_2_en1:·gc.churn.PS_Eden_Space.norm      thrpt  200      431.501 ±     3.201    B/op
  [info] mkVec1_2_en1:·gc.churn.PS_Survivor_Space       thrpt  200        0.020 ±     0.004  MB/sec
  [info] mkVec1_2_en1:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.007 ±     0.002    B/op
  [info] mkVec1_2_en1:·gc.count                         thrpt  200     2774.000              counts
  [info] mkVec1_2_en1:·gc.time                          thrpt  200     2032.000                  ms
  [info] mkVec1_2_n1                                    thrpt  200  2296352.866 ±  5464.419   ops/s
  [info] mkVec1_2_n1:·gc.alloc.rate                     thrpt  200     1049.834 ±     2.508  MB/sec
  [info] mkVec1_2_n1:·gc.alloc.rate.norm                thrpt  200      480.000 ±     0.001    B/op
  [info] mkVec1_2_n1:·gc.churn.PS_Eden_Space            thrpt  200     1050.384 ±     8.004  MB/sec
  [info] mkVec1_2_n1:·gc.churn.PS_Eden_Space.norm       thrpt  200      480.254 ±     3.495    B/op
  [info] mkVec1_2_n1:·gc.churn.PS_Survivor_Space        thrpt  200        0.025 ±     0.004  MB/sec
  [info] mkVec1_2_n1:·gc.churn.PS_Survivor_Space.norm   thrpt  200        0.011 ±     0.002    B/op
  [info] mkVec1_2_n1:·gc.count                          thrpt  200     2715.000              counts
  [info] mkVec1_2_n1:·gc.time                           thrpt  200     2048.000                  ms
  [info] mkVec1_4_b                                     thrpt  200  3469438.172 ±  2425.216   ops/s
  [info] mkVec1_4_b:·gc.alloc.rate                      thrpt  200      898.736 ±     0.680  MB/sec
  [info] mkVec1_4_b:·gc.alloc.rate.norm                 thrpt  200      272.000 ±     0.001    B/op
  [info] mkVec1_4_b:·gc.churn.PS_Eden_Space             thrpt  200      897.302 ±     6.159  MB/sec
  [info] mkVec1_4_b:·gc.churn.PS_Eden_Space.norm        thrpt  200      271.569 ±     1.873    B/op
  [info] mkVec1_4_b:·gc.churn.PS_Survivor_Space         thrpt  200        0.027 ±     0.004  MB/sec
  [info] mkVec1_4_b:·gc.churn.PS_Survivor_Space.norm    thrpt  200        0.008 ±     0.001    B/op
  [info] mkVec1_4_b:·gc.count                           thrpt  200     2840.000              counts
  [info] mkVec1_4_b:·gc.time                            thrpt  200     2037.000                  ms
  [info] mkVec1_4_e1n                                   thrpt  200  1560992.387 ± 14420.334   ops/s
  [info] mkVec1_4_e1n:·gc.alloc.rate                    thrpt  200      816.048 ±     8.475  MB/sec
  [info] mkVec1_4_e1n:·gc.alloc.rate.norm               thrpt  200      548.800 ±     1.736    B/op
  [info] mkVec1_4_e1n:·gc.churn.PS_Eden_Space           thrpt  200      814.281 ±    10.252  MB/sec
  [info] mkVec1_4_e1n:·gc.churn.PS_Eden_Space.norm      thrpt  200      547.617 ±     4.297    B/op
  [info] mkVec1_4_e1n:·gc.churn.PS_Survivor_Space       thrpt  200        0.037 ±     0.006  MB/sec
  [info] mkVec1_4_e1n:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.025 ±     0.004    B/op
  [info] mkVec1_4_e1n:·gc.count                         thrpt  200     2841.000              counts
  [info] mkVec1_4_e1n:·gc.time                          thrpt  200     2038.000                  ms
  [info] mkVec1_4_en1                                   thrpt  200  1571285.480 ±  5551.518   ops/s
  [info] mkVec1_4_en1:·gc.alloc.rate                    thrpt  200      814.123 ±     2.880  MB/sec
  [info] mkVec1_4_en1:·gc.alloc.rate.norm               thrpt  200      544.000 ±     0.001    B/op
  [info] mkVec1_4_en1:·gc.churn.PS_Eden_Space           thrpt  200      813.700 ±     6.588  MB/sec
  [info] mkVec1_4_en1:·gc.churn.PS_Eden_Space.norm      thrpt  200      543.720 ±     3.980    B/op
  [info] mkVec1_4_en1:·gc.churn.PS_Survivor_Space       thrpt  200        0.041 ±     0.005  MB/sec
  [info] mkVec1_4_en1:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.028 ±     0.003    B/op
  [info] mkVec1_4_en1:·gc.count                         thrpt  200     2834.000              counts
  [info] mkVec1_4_en1:·gc.time                          thrpt  200     2034.000                  ms
  [info] mkVec1_4_n1                                    thrpt  200  2103884.572 ±  5187.166   ops/s
  [info] mkVec1_4_n1:·gc.alloc.rate                     thrpt  200      977.879 ±     2.438  MB/sec
  [info] mkVec1_4_n1:·gc.alloc.rate.norm                thrpt  200      488.000 ±     0.001    B/op
  [info] mkVec1_4_n1:·gc.churn.PS_Eden_Space            thrpt  200      976.892 ±     6.396  MB/sec
  [info] mkVec1_4_n1:·gc.churn.PS_Eden_Space.norm       thrpt  200      487.507 ±     2.949    B/op
  [info] mkVec1_4_n1:·gc.churn.PS_Survivor_Space        thrpt  200        0.029 ±     0.004  MB/sec
  [info] mkVec1_4_n1:·gc.churn.PS_Survivor_Space.norm   thrpt  200        0.014 ±     0.002    B/op
  [info] mkVec1_4_n1:·gc.count                          thrpt  200     2708.000              counts
  [info] mkVec1_4_n1:·gc.time                           thrpt  200     2040.000                  ms
  [info] mkVec1_8_b                                     thrpt  200  2932393.888 ±  2032.306   ops/s
  [info] mkVec1_8_b:·gc.alloc.rate                      thrpt  200      804.378 ±     0.577  MB/sec
  [info] mkVec1_8_b:·gc.alloc.rate.norm                 thrpt  200      288.000 ±     0.001    B/op
  [info] mkVec1_8_b:·gc.churn.PS_Eden_Space             thrpt  200      803.699 ±     5.579  MB/sec
  [info] mkVec1_8_b:·gc.churn.PS_Eden_Space.norm        thrpt  200      287.763 ±     2.030    B/op
  [info] mkVec1_8_b:·gc.churn.PS_Survivor_Space         thrpt  200        0.029 ±     0.004  MB/sec
  [info] mkVec1_8_b:·gc.churn.PS_Survivor_Space.norm    thrpt  200        0.010 ±     0.002    B/op
  [info] mkVec1_8_b:·gc.count                           thrpt  200     2855.000              counts
  [info] mkVec1_8_b:·gc.time                            thrpt  200     2055.000                  ms
  [info] mkVec1_8_e1n                                   thrpt  200  1423695.694 ±  5866.048   ops/s
  [info] mkVec1_8_e1n:·gc.alloc.rate                    thrpt  200      766.125 ±     5.402  MB/sec
  [info] mkVec1_8_e1n:·gc.alloc.rate.norm               thrpt  200      564.800 ±     1.736    B/op
  [info] mkVec1_8_e1n:·gc.churn.PS_Eden_Space           thrpt  200      765.274 ±     7.702  MB/sec
  [info] mkVec1_8_e1n:·gc.churn.PS_Eden_Space.norm      thrpt  200      564.180 ±     4.446    B/op
  [info] mkVec1_8_e1n:·gc.churn.PS_Survivor_Space       thrpt  200        0.041 ±     0.005  MB/sec
  [info] mkVec1_8_e1n:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.031 ±     0.004    B/op
  [info] mkVec1_8_e1n:·gc.count                         thrpt  200     2793.000              counts
  [info] mkVec1_8_e1n:·gc.time                          thrpt  200     2035.000                  ms
  [info] mkVec1_8_en1                                   thrpt  200  1465745.921 ±  9565.937   ops/s
  [info] mkVec1_8_en1:·gc.alloc.rate                    thrpt  200      781.773 ±     5.100  MB/sec
  [info] mkVec1_8_en1:·gc.alloc.rate.norm               thrpt  200      560.000 ±     0.001    B/op
  [info] mkVec1_8_en1:·gc.churn.PS_Eden_Space           thrpt  200      781.559 ±     7.556  MB/sec
  [info] mkVec1_8_en1:·gc.churn.PS_Eden_Space.norm      thrpt  200      559.856 ±     4.089    B/op
  [info] mkVec1_8_en1:·gc.churn.PS_Survivor_Space       thrpt  200        0.045 ±     0.004  MB/sec
  [info] mkVec1_8_en1:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.032 ±     0.003    B/op
  [info] mkVec1_8_en1:·gc.count                         thrpt  200     2789.000              counts
  [info] mkVec1_8_en1:·gc.time                          thrpt  200     2027.000                  ms
  [info] mkVec1_8_n1                                    thrpt  200  1785399.006 ±  4347.964   ops/s
  [info] mkVec1_8_n1:·gc.alloc.rate                     thrpt  200      857.016 ±     2.101  MB/sec
  [info] mkVec1_8_n1:·gc.alloc.rate.norm                thrpt  200      504.000 ±     0.001    B/op
  [info] mkVec1_8_n1:·gc.churn.PS_Eden_Space            thrpt  200      856.053 ±     6.587  MB/sec
  [info] mkVec1_8_n1:·gc.churn.PS_Eden_Space.norm       thrpt  200      503.440 ±     3.719    B/op
  [info] mkVec1_8_n1:·gc.churn.PS_Survivor_Space        thrpt  200        0.028 ±     0.004  MB/sec
  [info] mkVec1_8_n1:·gc.churn.PS_Survivor_Space.norm   thrpt  200        0.016 ±     0.003    B/op
  [info] mkVec1_8_n1:·gc.count                          thrpt  200     2827.000              counts
  [info] mkVec1_8_n1:·gc.time                           thrpt  200     2046.000                  ms
  [success] Total time: 4910 s, completed 05/10/2015 12:20:16 PM
   */

  def b[A](a: A, as: A*): Vector[A] = {
    val b = Vector.newBuilder[A]
    b += a
    b ++= as
    b.result()
  }

  def e1n[A](a: A, as: A*): Vector[A] =
    (Vector.empty[A] :+ a) ++ as

  def en1[A](a: A, as: A*): Vector[A] =
    (Vector.empty[A] ++ as) :+ a

  def n1[A](a: A, as: A*): Vector[A] =
    Vector(as: _*) :+ a

  @Benchmark def mkVec1_2_b   =   b(1, 2)
  @Benchmark def mkVec1_2_e1n = e1n(1, 2)
  @Benchmark def mkVec1_2_en1 = en1(1, 2)
  @Benchmark def mkVec1_2_n1  =  n1(1, 2)


  @Benchmark def mkVec1_4_b   =   b(1, 2, 3, 4)
  @Benchmark def mkVec1_4_e1n = e1n(1, 2, 3, 4)
  @Benchmark def mkVec1_4_en1 = en1(1, 2, 3, 4)
  @Benchmark def mkVec1_4_n1  =  n1(1, 2, 3, 4)


  @Benchmark def mkVec1_8_b   =   b(1, 2, 3, 4, 5, 6, 7, 8)
  @Benchmark def mkVec1_8_e1n = e1n(1, 2, 3, 4, 5, 6, 7, 8)
  @Benchmark def mkVec1_8_en1 = en1(1, 2, 3, 4, 5, 6, 7, 8)
  @Benchmark def mkVec1_8_n1  =  n1(1, 2, 3, 4, 5, 6, 7, 8)
}

@State(Scope.Benchmark)
class MakeIndexedSeqBM {
  /*
  [info] Benchmark                                                     Mode  Cnt        Score       Error   Units
  [info] MakeIndexedSeqBM.mkIS_2_ti                                   thrpt  200  2537669.842 ± 10467.725   ops/s
  [info] MakeIndexedSeqBM.mkIS_2_tv                                   thrpt  200  2776142.888 ± 16689.844   ops/s
  [info] MakeIndexedSeqBM.mkIS_2_vb                                   thrpt  200  3623010.641 ±  5513.222   ops/s

  [info] MakeIndexedSeqBM.mkIS_4_ti                                   thrpt  200  2438481.649 ± 16029.081   ops/s
  [info] MakeIndexedSeqBM.mkIS_4_tv                                   thrpt  200  2580086.921 ± 13986.032   ops/s
  [info] MakeIndexedSeqBM.mkIS_4_vb                                   thrpt  200  3380245.554 ±  4240.056   ops/s

  [info] MakeIndexedSeqBM.mkIS_8_ti                                   thrpt  200  2291079.072 ± 10220.737   ops/s
  [info] MakeIndexedSeqBM.mkIS_8_tv                                   thrpt  200  2484470.125 ± 15363.238   ops/s
  [info] MakeIndexedSeqBM.mkIS_8_vb                                   thrpt  200  2821178.411 ±  2272.425   ops/s

  [info] Benchmark                                                     Mode  Cnt        Score       Error   Units
  [info] MakeIndexedSeqBM.mkIS_2_ti                                   thrpt  200  2537669.842 ± 10467.725   ops/s
  [info] MakeIndexedSeqBM.mkIS_2_ti:·gc.alloc.rate                    thrpt  200      734.276 ±     3.046  MB/sec
  [info] MakeIndexedSeqBM.mkIS_2_ti:·gc.alloc.rate.norm               thrpt  200      304.000 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_2_ti:·gc.churn.PS_Eden_Space           thrpt  200      734.210 ±     6.185  MB/sec
  [info] MakeIndexedSeqBM.mkIS_2_ti:·gc.churn.PS_Eden_Space.norm      thrpt  200      303.978 ±     2.270    B/op
  [info] MakeIndexedSeqBM.mkIS_2_ti:·gc.churn.PS_Survivor_Space       thrpt  200        0.023 ±     0.004  MB/sec
  [info] MakeIndexedSeqBM.mkIS_2_ti:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.010 ±     0.002    B/op
  [info] MakeIndexedSeqBM.mkIS_2_ti:·gc.count                         thrpt  200     2828.000              counts
  [info] MakeIndexedSeqBM.mkIS_2_ti:·gc.time                          thrpt  200     2037.000                  ms
  [info] MakeIndexedSeqBM.mkIS_2_tv                                   thrpt  200  2776142.888 ± 16689.844   ops/s
  [info] MakeIndexedSeqBM.mkIS_2_tv:·gc.alloc.rate                    thrpt  200      803.639 ±     4.842  MB/sec
  [info] MakeIndexedSeqBM.mkIS_2_tv:·gc.alloc.rate.norm               thrpt  200      304.000 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_2_tv:·gc.churn.PS_Eden_Space           thrpt  200      803.466 ±     7.432  MB/sec
  [info] MakeIndexedSeqBM.mkIS_2_tv:·gc.churn.PS_Eden_Space.norm      thrpt  200      303.944 ±     2.199    B/op
  [info] MakeIndexedSeqBM.mkIS_2_tv:·gc.churn.PS_Survivor_Space       thrpt  200        0.028 ±     0.004  MB/sec
  [info] MakeIndexedSeqBM.mkIS_2_tv:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.011 ±     0.002    B/op
  [info] MakeIndexedSeqBM.mkIS_2_tv:·gc.count                         thrpt  200     2802.000              counts
  [info] MakeIndexedSeqBM.mkIS_2_tv:·gc.time                          thrpt  200     2047.000                  ms
  [info] MakeIndexedSeqBM.mkIS_2_vb                                   thrpt  200  3623010.641 ±  5513.222   ops/s
  [info] MakeIndexedSeqBM.mkIS_2_vb:·gc.alloc.rate                    thrpt  200      938.407 ±     1.457  MB/sec
  [info] MakeIndexedSeqBM.mkIS_2_vb:·gc.alloc.rate.norm               thrpt  200      272.000 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_2_vb:·gc.churn.PS_Eden_Space           thrpt  200      938.704 ±     6.092  MB/sec
  [info] MakeIndexedSeqBM.mkIS_2_vb:·gc.churn.PS_Eden_Space.norm      thrpt  200      272.091 ±     1.758    B/op
  [info] MakeIndexedSeqBM.mkIS_2_vb:·gc.churn.PS_Survivor_Space       thrpt  200        0.024 ±     0.004  MB/sec
  [info] MakeIndexedSeqBM.mkIS_2_vb:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.007 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_2_vb:·gc.count                         thrpt  200     2913.000              counts
  [info] MakeIndexedSeqBM.mkIS_2_vb:·gc.time                          thrpt  200     2053.000                  ms
  [info] MakeIndexedSeqBM.mkIS_4_ti                                   thrpt  200  2438481.649 ± 16029.081   ops/s
  [info] MakeIndexedSeqBM.mkIS_4_ti:·gc.alloc.rate                    thrpt  200      724.539 ±     4.749  MB/sec
  [info] MakeIndexedSeqBM.mkIS_4_ti:·gc.alloc.rate.norm               thrpt  200      312.000 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_4_ti:·gc.churn.PS_Eden_Space           thrpt  200      724.781 ±     6.640  MB/sec
  [info] MakeIndexedSeqBM.mkIS_4_ti:·gc.churn.PS_Eden_Space.norm      thrpt  200      312.111 ±     2.051    B/op
  [info] MakeIndexedSeqBM.mkIS_4_ti:·gc.churn.PS_Survivor_Space       thrpt  200        0.031 ±     0.004  MB/sec
  [info] MakeIndexedSeqBM.mkIS_4_ti:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.013 ±     0.002    B/op
  [info] MakeIndexedSeqBM.mkIS_4_ti:·gc.count                         thrpt  200     2893.000              counts
  [info] MakeIndexedSeqBM.mkIS_4_ti:·gc.time                          thrpt  200     2047.000                  ms
  [info] MakeIndexedSeqBM.mkIS_4_tv                                   thrpt  200  2580086.921 ± 13986.032   ops/s
  [info] MakeIndexedSeqBM.mkIS_4_tv:·gc.alloc.rate                    thrpt  200      766.600 ±     4.165  MB/sec
  [info] MakeIndexedSeqBM.mkIS_4_tv:·gc.alloc.rate.norm               thrpt  200      312.000 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_4_tv:·gc.churn.PS_Eden_Space           thrpt  200      766.341 ±     6.696  MB/sec
  [info] MakeIndexedSeqBM.mkIS_4_tv:·gc.churn.PS_Eden_Space.norm      thrpt  200      311.894 ±     2.118    B/op
  [info] MakeIndexedSeqBM.mkIS_4_tv:·gc.churn.PS_Survivor_Space       thrpt  200        0.034 ±     0.005  MB/sec
  [info] MakeIndexedSeqBM.mkIS_4_tv:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.014 ±     0.002    B/op
  [info] MakeIndexedSeqBM.mkIS_4_tv:·gc.count                         thrpt  200     2843.000              counts
  [info] MakeIndexedSeqBM.mkIS_4_tv:·gc.time                          thrpt  200     2045.000                  ms
  [info] MakeIndexedSeqBM.mkIS_4_vb                                   thrpt  200  3380245.554 ±  4240.056   ops/s
  [info] MakeIndexedSeqBM.mkIS_4_vb:·gc.alloc.rate                    thrpt  200      901.281 ±     1.175  MB/sec
  [info] MakeIndexedSeqBM.mkIS_4_vb:·gc.alloc.rate.norm               thrpt  200      280.000 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_4_vb:·gc.churn.PS_Eden_Space           thrpt  200      900.632 ±     6.955  MB/sec
  [info] MakeIndexedSeqBM.mkIS_4_vb:·gc.churn.PS_Eden_Space.norm      thrpt  200      279.802 ±     2.156    B/op
  [info] MakeIndexedSeqBM.mkIS_4_vb:·gc.churn.PS_Survivor_Space       thrpt  200        0.030 ±     0.004  MB/sec
  [info] MakeIndexedSeqBM.mkIS_4_vb:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.009 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_4_vb:·gc.count                         thrpt  200     2841.000              counts
  [info] MakeIndexedSeqBM.mkIS_4_vb:·gc.time                          thrpt  200     2044.000                  ms
  [info] MakeIndexedSeqBM.mkIS_8_ti                                   thrpt  200  2291079.072 ± 10220.737   ops/s
  [info] MakeIndexedSeqBM.mkIS_8_ti:·gc.alloc.rate                    thrpt  200      715.607 ±     3.193  MB/sec
  [info] MakeIndexedSeqBM.mkIS_8_ti:·gc.alloc.rate.norm               thrpt  200      328.000 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_8_ti:·gc.churn.PS_Eden_Space           thrpt  200      715.175 ±     6.293  MB/sec
  [info] MakeIndexedSeqBM.mkIS_8_ti:·gc.churn.PS_Eden_Space.norm      thrpt  200      327.804 ±     2.502    B/op
  [info] MakeIndexedSeqBM.mkIS_8_ti:·gc.churn.PS_Survivor_Space       thrpt  200        0.032 ±     0.004  MB/sec
  [info] MakeIndexedSeqBM.mkIS_8_ti:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.015 ±     0.002    B/op
  [info] MakeIndexedSeqBM.mkIS_8_ti:·gc.count                         thrpt  200     2808.000              counts
  [info] MakeIndexedSeqBM.mkIS_8_ti:·gc.time                          thrpt  200     2020.000                  ms
  [info] MakeIndexedSeqBM.mkIS_8_tv                                   thrpt  200  2484470.125 ± 15363.238   ops/s
  [info] MakeIndexedSeqBM.mkIS_8_tv:·gc.alloc.rate                    thrpt  200      776.099 ±     4.795  MB/sec
  [info] MakeIndexedSeqBM.mkIS_8_tv:·gc.alloc.rate.norm               thrpt  200      328.000 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_8_tv:·gc.churn.PS_Eden_Space           thrpt  200      775.372 ±     7.424  MB/sec
  [info] MakeIndexedSeqBM.mkIS_8_tv:·gc.churn.PS_Eden_Space.norm      thrpt  200      327.707 ±     2.496    B/op
  [info] MakeIndexedSeqBM.mkIS_8_tv:·gc.churn.PS_Survivor_Space       thrpt  200        0.037 ±     0.005  MB/sec
  [info] MakeIndexedSeqBM.mkIS_8_tv:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.016 ±     0.002    B/op
  [info] MakeIndexedSeqBM.mkIS_8_tv:·gc.count                         thrpt  200     2872.000              counts
  [info] MakeIndexedSeqBM.mkIS_8_tv:·gc.time                          thrpt  200     2041.000                  ms
  [info] MakeIndexedSeqBM.mkIS_8_vb                                   thrpt  200  2821178.411 ±  2272.425   ops/s
  [info] MakeIndexedSeqBM.mkIS_8_vb:·gc.alloc.rate                    thrpt  200      795.401 ±     0.648  MB/sec
  [info] MakeIndexedSeqBM.mkIS_8_vb:·gc.alloc.rate.norm               thrpt  200      296.000 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_8_vb:·gc.churn.PS_Eden_Space           thrpt  200      794.263 ±     5.659  MB/sec
  [info] MakeIndexedSeqBM.mkIS_8_vb:·gc.churn.PS_Eden_Space.norm      thrpt  200      295.582 ±     2.132    B/op
  [info] MakeIndexedSeqBM.mkIS_8_vb:·gc.churn.PS_Survivor_Space       thrpt  200        0.035 ±     0.004  MB/sec
  [info] MakeIndexedSeqBM.mkIS_8_vb:·gc.churn.PS_Survivor_Space.norm  thrpt  200        0.013 ±     0.001    B/op
  [info] MakeIndexedSeqBM.mkIS_8_vb:·gc.count                         thrpt  200     2834.000              counts
  [info] MakeIndexedSeqBM.mkIS_8_vb:·gc.time                          thrpt  200     2044.000                  ms
  */

  def ti[A](as: A*): IndexedSeq[A] =
    as.toIndexedSeq

  def tv[A](as: A*): Vector[A] =
    as.toVector

  def vb[A](as: A*): Vector[A] =
    (Vector.newBuilder[A] ++= as).result()

  @Benchmark def mkIS_2_ti = ti(1, 2)
  @Benchmark def mkIS_2_tv = tv(1, 2)
  @Benchmark def mkIS_2_vb = vb(1, 2)

  @Benchmark def mkIS_4_ti = ti(1, 2, 3, 4)
  @Benchmark def mkIS_4_tv = tv(1, 2, 3, 4)
  @Benchmark def mkIS_4_vb = vb(1, 2, 3, 4)

  @Benchmark def mkIS_8_ti = ti(1, 2, 3, 4, 5, 6, 7, 8)
  @Benchmark def mkIS_8_tv = tv(1, 2, 3, 4, 5, 6, 7, 8)
  @Benchmark def mkIS_8_vb = vb(1, 2, 3, 4, 5, 6, 7, 8)
}

