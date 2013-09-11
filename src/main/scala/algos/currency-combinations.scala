package algos

/**
 * Find out all unique combinations of coin denominations that add up to same total
 * First wrote this in Haskell (See link below)
 *   https://github.com/kevinfrancis/haskell-algos/blob/master/coins.hs
 */
object CurrencyCombinations {
  
  val denominations = List(25, 10, 5, 1)
  
  /** Find all combinations such that each has items not exceeding the thresh */
  private def coin_comb(cents: Int, thresh: Int): List[List[Int]] = {
    
    def combK(k: Int): List[List[Int]] = {
      if (cents < k || thresh < k)
        List()
      else
        coin_comb(cents-k, k).map(k :: _)
    }
    
    if (cents > 0)
      // combK(25) ::: combK(10) ::: combK(5) ::: combK(1)
      denominations.flatMap(combK)
    else
      List(List())
  }
  
  def get_combinations(cents: Int) = coin_comb(cents, denominations.max)
}

object CurrencyCombinationsTester {
  def main(args: Array[String]): Unit = {
    
    import CurrencyCombinations.get_combinations 

    for (cents <- Range(1,32))
      println(cents + " => " + get_combinations(cents))
  }
}
