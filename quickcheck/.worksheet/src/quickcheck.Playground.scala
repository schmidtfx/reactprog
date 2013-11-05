package quickcheck

object Playground {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(93); 

  val h = new QuickCheckHeap with Bogus3BinomialHeap;System.out.println("""h  : quickcheck.QuickCheckHeap with quickcheck.Bogus3BinomialHeap = """ + $show(h ));$skip(10); 
  h.check}
  
  
  
}
