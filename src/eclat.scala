/**
  * Created by Pritam on 16-Oct-16.
  */
import java.io._
import scala.io.Source
import java.util.concurrent

//objects are an anonymous (inaccessible) class and creates a single instance of this class 
object eclat {
  case class itemSet(i: Set[Char], t_i: Set[Int])
  case class d_itemSet(i: Set[Char], d_i: Set[Int], sup_i : Int)

  def main(args: Array[String]) {
    val x1:itemSet = itemSet(Set('A'), Set(1,3,4,5))
    val x2:itemSet = itemSet(Set('B'), Set(1,2,3,4,5,6))
    val x3:itemSet = itemSet(Set('C'), Set(2,4,5,6))
    val x4:itemSet = itemSet(Set('D'), Set(1,3,5,6))
    val x5:itemSet = itemSet(Set('E'), Set(1,2,3,4,5))
    val P:Set[itemSet] = Set(x1,x2,x3,x4,x5)
    val F:Set[itemSet] = Set()
    val min_support = 3
    //println("Hello, world!" + P)

    // ECLAT
    val writer = new PrintWriter(new File("time_plotter.csv" ))
    val max_loop:Int = 50 // number of times the code will run

    var time_sum:Long = 0 // in nano seconds
    for(i <- 1 to max_loop) {
      //println(time_sum)
      val t0 = System.nanoTime()
      ECLAT(P, min_support, F)
      val t1 = System.nanoTime()
      time_sum += (t1 - t0)
      writer.write((t1 - t0).toString + ", ")
    }
    writer.write("\n")
    //println(output_eclat)
    writer.close()
    println("Time required for eclat : " + time_sum.toDouble / (max_loop * 1000).toDouble + " ms")

    // dECLAT
    var DP:Set[d_itemSet] = Set()
    var T:Set[Int] = Set()
    val DF:Set[d_itemSet] = Set()
    P.foreach{ item =>
      T = T | item.t_i
    }
    P.foreach{ item =>
      val t:d_itemSet = new d_itemSet(item.i, T -- item.t_i, item.t_i.size)
      DP += t
    }
    //println(DP)
    println(dECLAT(DP, min_support, DF))

  }

  def dECLAT(DP:Set[d_itemSet], min_sup:Int, F:Set[d_itemSet]) : Set[d_itemSet] = {
    var F_curr:Set[d_itemSet] = F  //initialize F_curr as null set
    
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
