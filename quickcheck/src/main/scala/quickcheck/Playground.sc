package quickcheck

object Playground {

  val h = new QuickCheckHeap with Bogus3BinomialHeap
                                                  //> h  : quickcheck.QuickCheckHeap with quickcheck.Bogus3BinomialHeap = Prop
  h.check                                         //> + Heap.min1: OK, passed 100 tests.
                                                  //| + Heap.gen1: OK, passed 100 tests.
                                                  //| + Heap.gen2: OK, passed 100 tests.
                                                  //| + Heap.del1: OK, passed 100 tests.
                                                  //| (-482258721,List(Node(-482258721,1,List(Node(-482258721,0,List())))))
                                                  //| (-482258721,List(Node(-482258721,0,List())))
                                                  //| (1966512715,List(Node(1966512715,0,List())))
                                                  //| (-1305311649,List(Node(-361133540,0,List()), Node(-1305311649,1,List(Node(-1
                                                  //| 305311649,0,List())))))
                                                  //| (-1305311649,List(Node(-1305311649,1,List(Node(-1305311649,0,List())))))
                                                  //| (-1305311649,List(Node(-1305311649,0,List())))
                                                  //| (-2147483648,List(Node(-2147483648,0,List())))
                                                  //| (-814723215,List(Node(-814723215,2,List(Node(-814723215,1,List(Node(-8147232
                                                  //| 15,0,List()))), Node(-814723215,0,List())))))
                                                  //| (-814723215,List(Node(-814723215,0,List()), Node(-814723215,1,List(Node(-814
                                                  //| 723215,0,List())))))
                                                  //| (-814723215,List(Node(-814723215,1,List(Node(-814723215,0,List())))))
                                                  //| (-814723215,List(Node(-814723215,0,List())))
                                                  //| (942304209,List(
                                                  //| Output exceeds cutoff limit.
  
  
  
}