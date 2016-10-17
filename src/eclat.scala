/**
  * Created by Pritam on 16-Oct-16.
  */
object eclat {
  case class itemSet(i: Set[Char], t_i: Set[Int])

  def main(args: Array[String]) {
    val x1:itemSet = itemSet(Set('A'), Set(1,3,4,5))
    val x2:itemSet = itemSet(Set('B'), Set(1,2,3,4,5,6))
    val x3:itemSet = itemSet(Set('C'), Set(2,4,5,6))
    val x4:itemSet = itemSet(Set('D'), Set(1,3,5,6))
    val x5:itemSet = itemSet(Set('E'), Set(1,2,3,4,5))
    val P:Set[itemSet] = Set(x1,x2,x3,x4,x5)
    val F:Set[itemSet] = Set()
    //println("Hello, world!" + P)
    println(ECLAT(P, 3, F))
  }

  def isValid(Xa:Set[Char], Xb:Set[Char]): Boolean = {
    val unionSet:Set[Char] = Xa | Xb
    val diffA:Set[Char] = unionSet -- Xa
    val diffB:Set[Char] = unionSet -- Xb
    val seqA = diffA.toString()
    val seqB = diffB.toString()
    //println(seqA + " " + seqB)
    if(seqA(4) != ')' && seqB(4) != ')' && seqA(4) > seqB(4))
      {//println("ok "+ Xa +" "+Xb + " "  + seqA(4) +" "+ seqB(4))
      return true}
    else
      {//println("no " + Xa +" "+Xb + " " + seqA(4) +" "+ seqB(4))
      return false}
  }

  def ECLAT(P:Set[itemSet], min_sup:Int, F:Set[itemSet]): Set[itemSet] = {
    var F_curr:Set[itemSet] = F
    P.foreach { item1 =>
      F_curr = F_curr + item1
      var P1:Set[itemSet] = Set()

      P.foreach { item2 =>
        if(isValid(item1.i, item2.i)){
          var commonChar:Set[Char] = Set()
          commonChar = item1.i | item2.i
          var commonSet:Set[Int] = Set()
          commonSet = item1.t_i & item2.t_i
          //println(commonChar)
          if(commonSet.size >= min_sup){
            val tempItem:itemSet = itemSet(commonChar, commonSet)
            P1 = P1 + tempItem
          }
        }
      }

      if(P1.nonEmpty){
        val F_next:Set[itemSet] = ECLAT(P1,min_sup,F_curr)
        //println(F_next)
        F_curr = F_curr | F_next
        /*F_next.foreach(item =>
          F_curr = F_curr + item
        )
        return F_curr*/
      }
    }
    //println(F_curr)
    return F_curr
  }
}
