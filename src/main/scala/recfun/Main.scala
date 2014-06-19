package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || c==r)
      return 1;
    if(r == 0)
      return 1;
    
    return pascal(c-1,r-1) + pascal(c,r-1);
    
  };

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if(chars.length==0)
      return true;
    
    return myBalance(chars,0,0);
  };

  def myBalance(chars: List[Char],left: Int, right: Int): Boolean = {
    
    if(left < right)
      return false;
    
    if(chars.length == 0) {
      if(left == right)
        return true;
      else
        return false;
    }
    
    if(chars.head == ')') {
      return myBalance(chars.tail,left,right+1);
    } else if(chars.head == '(') {
      return myBalance(chars.tail,left+1,right);
    } else {
      return myBalance(chars.tail,left,right);
    }
  };
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.length == 0)
      return 0;
    if(money == 0) 
      return 0;
    
    return countForMe(money,coins);
  }
  
  def countForMe(money: Int, coins:List[Int]) : Int = {
    if(money == 0) {
      return 1;
    }
    
    if(money < 0) {
      return 0;
    }
    
    if(coins.length < 0) 
      return 0;
    
    return countForMe(money,coins.tail) + countForMe(money-coins.head,coins.tail);
    
  }
}
