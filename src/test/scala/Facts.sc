import java.lang.Math._

object Facts {
  println("Stirling's Approx to n!")              //> Stirling's Approx to n!

	implicit class Pz(num:Int) {
		def n = num
	}
	implicit val pz: Pz = 10                  //> pz  : Facts.Pz = Facts$$anonfun$main$1$Pz$2@b684286
  
  def ListN(x: Int) = (1 to x toList)             //> ListN: (x: Int)List[Int]
  
  def h[U](f: Int => U)(implicit pz: Pz): List[U] = ListN(pz.n).map(f)
                                                  //> h: [U](f: Int => U)(implicit pz: Facts.Pz)List[U]
  def h2[U,T](f: Int => U,g: Int => T)(implicit pz: Pz): List[(U,T)] = h(f) zip h(g)
                                                  //> h2: [U, T](f: Int => U, g: Int => T)(implicit pz: Facts.Pz)List[(U, T)]
  def r[U,T](f: Int => Double,g: Int => Double)(implicit pz: Pz): List[Float] = h2(f,g) map (x => (x._1 / x._2) toFloat)
                                                  //> r: [U, T](f: Int => Double, g: Int => Double)(implicit pz: Facts.Pz)List[Flo
                                                  //| at]
                                                  
  def fact(x: Int) = ListN(x).foldLeft(1.0) (_ * _)
                                                  //> fact: (x: Int)Double
  
  def p(x: Int, b: Double): Double = pow(b, x)    //> p: (x: Int, b: Double)Double
  def pe(x: Int): Double = exp(x)                 //> pe: (x: Int)Double
                                                  
  def p2(x: Int) = p(x,2.0)                       //> p2: (x: Int)Double
  
  def px(x: Int) = p(x,x)                         //> px: (x: Int)Double
  
  def ps(x: Int) = px(x) / pe(x)                  //> ps: (x: Int)Double
  
  def st(x: Int) = ps(x) * sqrt( 2.0 * PI * x)    //> st: (x: Int)Double

 val n = 20                                       //> n  : Int = 20
  
 h(fact)                                          //> res0: List[Double] = List(1.0, 2.0, 6.0, 24.0, 120.0, 720.0, 5040.0, 40320.0
                                                  //| , 362880.0, 3628800.0)
 h(p2)                                            //> res1: List[Double] = List(2.0, 4.0, 8.0, 16.0, 32.0, 64.0, 128.0, 256.0, 512
                                                  //| .0, 1024.0)
 h(pe)                                            //> res2: List[Double] = List(2.718281828459045, 7.38905609893065, 20.0855369231
                                                  //| 87668, 54.598150033144236, 148.4131591025766, 403.4287934927351, 1096.633158
                                                  //| 4284585, 2980.9579870417283, 8103.083927575384, 22026.465794806718)

 h(px)                                            //> res3: List[Double] = List(1.0, 4.0, 27.0, 256.0, 3125.0, 46656.0, 823543.0, 
                                                  //| 1.6777216E7, 3.87420489E8, 1.0E10)
 h(ps)                                            //> res4: List[Double] = List(0.36787944117144233, 0.5413411329464508, 1.3442508
                                                  //| 459323264, 4.688803555515951, 21.056084372142085, 115.64866155454563, 750.97
                                                  //| 4009558663, 5628.128968248068, 47811.48664665559, 453999.2976248485)
 h(st)                                            //> res5: List[Double] = List(0.9221370088957891, 1.9190043514889832, 5.83620959
                                                  //| 1345863, 23.506175132893294, 118.01916795759008, 710.0781846421849, 4980.395
                                                  //| 831612462, 39902.3954526567, 359536.87284194824, 3598695.618741036)
                                                   
  h2(fact,p2)                                     //> res6: List[(Double, Double)] = List((1.0,2.0), (2.0,4.0), (6.0,8.0), (24.0,1
                                                  //| 6.0), (120.0,32.0), (720.0,64.0), (5040.0,128.0), (40320.0,256.0), (362880.0
                                                  //| ,512.0), (3628800.0,1024.0))
   h2(fact,pe)                                    //> res7: List[(Double, Double)] = List((1.0,2.718281828459045), (2.0,7.38905609
                                                  //| 893065), (6.0,20.085536923187668), (24.0,54.598150033144236), (120.0,148.413
                                                  //| 1591025766), (720.0,403.4287934927351), (5040.0,1096.6331584284585), (40320.
                                                  //| 0,2980.9579870417283), (362880.0,8103.083927575384), (3628800.0,22026.465794
                                                  //| 806718))

	r(fact,pe)                                //> res8: List[Float] = List(0.36787945, 0.27067056, 0.29872242, 0.43957534, 0.
                                                  //| 80855364, 1.7847016, 4.5958853, 13.525853, 44.78295, 164.74727)
                                                   
  r(fact,px)                                      //> res9: List[Float] = List(1.0, 0.5, 0.22222222, 0.09375, 0.0384, 0.015432099
                                                  //| , 0.006119899, 0.0024032593, 9.366567E-4, 3.6288E-4)
                                                  
	r(fact,ps)                                //> res10: List[Float] = List(2.7182817, 3.694528, 4.463453, 5.1185765, 5.69906
                                                  //| 5, 6.225753, 6.711284, 7.164015, 7.589808, 7.992964)
                                                  
	r(fact,st)                                //> res11: List[Float] = List(1.0844376, 1.0422071, 1.0280645, 1.0210083, 1.016
                                                  //| 784, 1.0139729, 1.0119678, 1.0104656, 1.0092984, 1.0083654)
	
  r(fact,st) (60)                                 //> res12: List[Float] = List(1.0844376, 1.0422071, 1.0280645, 1.0210083, 1.016
                                                  //| 784, 1.0139729, 1.0119678, 1.0104656, 1.0092984, 1.0083654, 1.0076025, 1.00
                                                  //| 6967, 1.0064296, 1.0059692, 1.0055702, 1.0052212, 1.0049134, 1.0046399, 1.0
                                                  //| 043952, 1.0041751, 1.0039759, 1.0037948, 1.0036296, 1.003478, 1.0033387, 1.
                                                  //| 0032101, 1.0030911, 1.0029805, 1.0028776, 1.0027815, 1.0026917, 1.0026075, 
                                                  //| 1.0025283, 1.0024539, 1.0023837, 1.0023174, 1.0022547, 1.0021954, 1.002139,
                                                  //|  1.0020854, 1.0020345, 1.001986, 1.0019398, 1.0018957, 1.0018536, 1.0018132
                                                  //| , 1.0017745, 1.0017376, 1.0017021, 1.001668, 1.0016353, 1.0016038, 1.001573
                                                  //| 6, 1.0015444, 1.0015163, 1.0014892, 1.001463, 1.0014378, 1.0014135, 1.00138
                                                  //| 99)
}