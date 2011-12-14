

object FunctorImplementations {

  // Not that useful for Scala collections that already have map() built in...
  implicit def listFunctor[A] = new Functor[List] {
    def fmap[A, B](f: A => B, list: List[A]) = list map f
  }

  // ... But we can use this pattern on objects that don't.
  implicit def basketFunctor[A] = new Functor[Basket] {
    def fmap[A, B](f: A => B, basket: Basket[A]) = Basket(listFunctor.fmap(f, basket.items))
  }
  
}