/**
  * Created by Pritam on 16-Oct-16.
  */
import java.io._
import scala.io.Source

//objects are an anonymous (inaccessible) class and creates a single instance of this class 
object eclat {
  case class itemSet(i: Set[Char], t_i: Set[Int])
  case class d_itemSet(i: Set[Char], d_i: Set[Int], sup_i : Int)

  def main(args: Array[String]) {
    // Read data from .dat file
    var P:Set[itemSet] = Set()
    var transactions:Int = 0
    var curr_char:Set[Char] = Set()
    var split_string:Array[String] = Array()
    var curr_trans:Set[Int] = Set()

    val min_support = 100
    val run_eclat:Boolean = true
    val run_both_prog_and_compare = true

    for(line <- Source.fromFile("large.dat").getLines()) {
      if(transactions == 0){
        curr_char = Set()
        curr_char += line.charAt(0)
        //println(curr_char)
        transactions = 1
      } else {
        curr_trans = Set()
        split_string = line.split(" ")
        split_string.foreach{ string =>
          curr_trans += string.toInt
        }
        //println(curr_trans)
        if(curr_trans.size > min_support) {
          P += itemSet(curr_char, curr_trans)
        }
        transactions = 0
      }
    }

    //println(P)
    /*
    val x1:itemSet = itemSet(Set('A'), Set(1,3,4,5))
    val x2:itemSet = itemSet(Set('B'), Set(1,2,3,4,5,6))
    val x3:itemSet = itemSet(Set('C'), Set(2,4,5,6))
    val x4:itemSet = itemSet(Set('D'), Set(1,3,5,6))
    val x5:itemSet = itemSet(Set('E'), Set(1,2,3,4,5))
    val P:Set[itemSet] = Set(x1,x2,x3,x4,x5)
    */

    // ECLAT initialization
    val F:Set[itemSet] = Set()

    // Start FileWriter to append/plot data in csv format
    var writer:FileWriter = new FileWriter(new File("empty.csv"), true)
    if(run_eclat) {
      writer = new FileWriter(new File("eclat_plotter2.csv"), true)
    } else {
      writer = new FileWriter(new File("declat_plotter2.csv"), true)
    }
    val max_loop:Int = 10 // number of times the code will run

    var time_sum:Long = 0 // in nano seconds

    // Loop n times and print execution time to file
    if(run_eclat) {
      for (i <- 1 to max_loop) {
        //println(time_sum)
        val t0 = System.nanoTime()
        ECLAT(P, min_support, F)
        val t1 = System.nanoTime()
        time_sum += (t1 - t0)
        writer.write((t1 - t0).toString + ", ")
      }
      writer.write("\n")
      println("Time required for ECLAT : " + time_sum.toDouble / (max_loop * 1000).toDouble + " micro sec")
    }


    // dECLAT initialization
    var DP:Set[d_itemSet] = Set()
    var T:Set[Int] = Set()
    val DF:Set[d_itemSet] = Set()
    P.foreach{ item =>
      T = T | item.t_i
    }
    P.foreach{ item =>
      val t:d_itemSet = d_itemSet(item.i, T -- item.t_i, item.t_i.size)
      if(t.sup_i >= min_support) {
        DP += t
      }
    }

    if(!run_eclat) {
      time_sum = 0 // initialization : in nano seconds
      // Loop n times and print execution time to file
      for (i <- 1 to max_loop) {
        //println(time_sum)
        val t0 = System.nanoTime()
        dECLAT(DP, min_support, DF)
        val t1 = System.nanoTime()
        time_sum += (t1 - t0)
        writer.write((t1 - t0).toString + ", ")
      }
      writer.write("\n")
      println("Time required for dECLAT : " + time_sum.toDouble / (max_loop * 1000).toDouble + " micro sec")
    }

    writer.close()

    // Check if output of ECLAT & dECLAT output is same
    if(run_both_prog_and_compare) {
      val out_eclat: Set[itemSet] = ECLAT(P, min_support, F)
      val out_declat: Set[d_itemSet] = dECLAT(DP, min_support, DF)
      var out_eclat_refined: Set[Set[Char]] = Set()
      var out_declat_refined: Set[Set[Char]] = Set()
      out_eclat.foreach { item =>
        out_eclat_refined += item.i
      }
      out_declat.foreach { item =>
        out_declat_refined += item.i
      }
      val union_of_eclat_declat: Set[Set[Char]] = out_eclat_refined | out_declat_refined
      if (union_of_eclat_declat.size == out_eclat_refined.size & union_of_eclat_declat.size == out_declat_refined.size) {
        println("\nEclat and dEclat output matches.\n")
      }

      writer = new FileWriter(new File("comparison_graph2.csv"))
      var eclat_avg_array = Array.fill[Long](max_loop)(0)
      var declat_avg_array = Array.fill[Long](max_loop)(0)
      var split_line:Array[String] = Array()
      for(i <- 1 to max_loop){
        for(line <- Source.fromFile("eclat_plotter2.csv").getLines()) {
          split_line = line.split(", ")
          eclat_avg_array(i-1) += split_line(i-1).toInt
          //println(split_line(i-1).toInt)
        }
        for(line <- Source.fromFile("declat_plotter2.csv").getLines()) {
          split_line = line.split(", ")
          declat_avg_array(i-1) += split_line(i-1).toInt
        }
      }

      val count1 = Source.fromFile("eclat_plotter2.csv").getLines().size
      val count2 = Source.fromFile("declat_plotter2.csv").getLines().size
      for(i <- 0 to max_loop - 1){
        writer.write(eclat_avg_array(i)/count1 + ", ")
      }
      writer.write("\n")
      for(i <- 0 to max_loop - 1){
        writer.write(declat_avg_array(i)/count2 + ", ")
      }
      writer.write("\n")

      writer.close()
      println("Generated comparison graph.\n")
    }

  }

  def dECLAT(DP:Set[d_itemSet], min_sup:Int, DF:Set[d_itemSet]) : Set[d_itemSet] = {
    var F_curr:Set[d_itemSet] = DF  //initialize F_curr as null set
    // for each item in P check possible frequent next level items
    DP.foreach { item1 =>
      F_curr = F_curr + item1
      var P1:Set[d_itemSet] = Set()
      //
      DP.foreach { item2 =>
        if(isValid(item1.i, item2.i)){
          var commonChar:Set[Char] = Set()
          commonChar = item1.i | item2.i        //union of item1 and item2
          var commonSet:Set[Int] = Set()
          commonSet = item2.d_i -- item1.d_i     //intersection of tid of item1 and item2
          val support_12 = item1.sup_i - commonSet.size
          //println(commonChar)
          //println(item1 + "\t" + item2 + "\t" + commonChar + "\t" + commonSet + "\t" + support_12 + "\n")
          if(support_12 >= min_sup){        //support of commonSet greater than minsup
          val tempItem:d_itemSet = d_itemSet(commonChar, commonSet, support_12)
            P1 = P1 + tempItem                  //add current item to P1
          }
        }
      }
      //repeat above for next level items
      if(P1.nonEmpty){
        val F_next:Set[d_itemSet] = dECLAT(P1,min_sup,F_curr)
        //add next level frequent items to current level frequent items
        F_curr = F_curr | F_next
      }
    }
    //println(F_curr)
    //return all frequent items
    return F_curr
  }

 //compare Xa with Xb, return true if Xa is less than Xb
  def isValid(Xa:Set[Char], Xb:Set[Char]): Boolean = {
    val unionSet:Set[Char] = Xa | Xb       //common items
    val diffA:Set[Char] = unionSet -- Xa   //items in Xb except common items
    val diffB:Set[Char] = unionSet -- Xb   //items in Xa except common items
    val seqA = diffA.toString()
    val seqB = diffB.toString()
    //println(seqA + " " + seqB)
   //compare first character of diffA and diffB
    if(seqA(4) != ')' && seqB(4) != ')' && seqA(4) > seqB(4))
      {//println("ok "+ Xa +" "+Xb + " "  + seqA(4) +" "+ seqB(4))
      return true}
    else
      {//println("no " + Xa +" "+Xb + " " + seqA(4) +" "+ seqB(4))
      return false}
  }

 //computes frequent items in depth first search manner
  def ECLAT(P:Set[itemSet], min_sup:Int, F:Set[itemSet]): Set[itemSet] = {
    var F_curr:Set[itemSet] = F  //initialize F_curr as null set
   // for each item in P check possible frequent next level items 
   P.foreach { item1 =>
      F_curr = F_curr + item1
      var P1:Set[itemSet] = Set()
      //
      P.foreach { item2 =>
        if(isValid(item1.i, item2.i)){
          var commonChar:Set[Char] = Set()
          commonChar = item1.i | item2.i        //union of item1 and item2
          var commonSet:Set[Int] = Set()
          commonSet = item1.t_i & item2.t_i     //intersection of tid of item1 and item2
          //println(commonChar)
          if(commonSet.size >= min_sup){        //support of commonSet greater than minsup
            val tempItem:itemSet = itemSet(commonChar, commonSet)
            P1 = P1 + tempItem                  //add current item to P1
          }
        }
      }
      //repeat above for next level items
      if(P1.nonEmpty){
        val F_next:Set[itemSet] = ECLAT(P1,min_sup,F_curr)
        //println(F_next)
       //add next level frequent items to current level frequent items
        F_curr = F_curr | F_next
        /*F_next.foreach(item =>
          F_curr = F_curr + item
        )
        return F_curr*/
      }
    }
    //println(F_curr)
   //return all frequent items
    return F_curr
  }
}
