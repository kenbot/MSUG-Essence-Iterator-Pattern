

object MonoidImplementations {
  def intAdditionMonoid = Monoid(0)(_+_)
  def intMultiplicationMonoid = Monoid(1)(_*_)
  implicit def stringMonoid = Monoid("")(_+_)
  implicit def listMonoid[A] = Monoid(List.empty[A])(_++_)
  
  // Also:
  def booleanAndMonoid = Monoid(true)(_&&_)
  def booleanOrMonoid = Monoid(false)(_||_)
}

object RepetitiveMonoidExample {

  // 1. Integers (0, +)
  def sum(list: List[Int]): Int = {
    var total = 0
    for (i <- list) {
      total = total + i
    }
    total
  }
  
  // 2. Integers (1, *)
  def product(list: List[Int]): Int = {
    var total = 1
    for (i <- list) {
      total = total * i
    }
    total
  }

  // 3. Strings ("",  +)
  def appendAll(list: List[String]): String = {
    var str = ""
    for (s <- list) {
      str = str + s
    }
    str
  }

  // 4. Lists of A (List.empty,  ++)
  def flatten[A](lists: List[List[A]]): List[A] = {
    var flatList = List.empty[A]
    for (list <- lists) {
      flatList = flatList ++ list
    }
    flatList
  }
}

object GenericMonoidExample {
  
  import MonoidImplementations._
  
  // This method signature de-sugars to:
  // def accumulate[M](list: List[M])(implicit monoid: Monoid[M]): M
  //
  // I think "M: Monoid" conveys the intent more cleanly: "Some type M that has a Monoid context"
  
  def accumulate[M: Monoid](list: List[M]): M = {
    val monoid = implicitly[Monoid[M]]
    var accum = monoid.zero
    
    for (m <- list) {
      accum = monoid.append(accum, m)
    }
    accum
  }
  
  // We get this algorithm for free now, for all monoids, everywhere, ever!
  def sum(list: List[Int]) = accumulate(list)(intAdditionMonoid)
  def product(list: List[Int]) = accumulate(list)(intMultiplicationMonoid)
  def appendAll(list: List[String]) = accumulate(list)
  def flatten[A](list: List[List[A]]) = accumulate(list)
  def all(list: List[Boolean]) = accumulate(list)(booleanAndMonoid)
  def any(list: List[Boolean]) = accumulate(list)(booleanOrMonoid)
}