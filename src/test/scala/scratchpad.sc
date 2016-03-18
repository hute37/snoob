object scratchpad {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val d = Map("a" -> "A1", "b" -> "B2", "c" -> "C3")
                                                  //> d  : scala.collection.immutable.Map[String,String] = Map(a -> A1, b -> B2, c
                                                  //|  -> C3)
                          
  // @see: http://stackoverflow.com/questions/1052476/what-is-scalas-yield/1052510#1052510
                                                     
    var found1 = false                            //> found1  : Boolean = false
   for (x <- List.range(1, 10); if x % 2 == 1 && !found1)
  		if (x == 5) found1 = true else println(x)
                                                  //> 1
                                                  //| 3
    var found2 = false                            //> found2  : Boolean = false
		for (x <- Stream.range(1, 10); if x % 2 == 1 && !found2)
  		if (x == 5) found2 = true else println(x)
                                                  //> 1
                                                  //| 3
                                                  
    var found3 = false                            //> found3  : Boolean = false
		List.range(1,10).filter(_ % 2 == 1 && !found3).foreach(x => if (x == 5) found3 = true else println(x))
                                                  //> 1
                                                  //| 3
                                                  //| 7
                                                  //| 9
1                                                 //> res0: Int(1) = 1

    var found4 = false                            //> found4  : Boolean = false
		List.range(1,10).withFilter(_ % 2 == 1 && !found4).foreach(x => if (x == 5) found4 = true else println(x))
                                                  //> 1
                                                  //| 3

                                                  
}