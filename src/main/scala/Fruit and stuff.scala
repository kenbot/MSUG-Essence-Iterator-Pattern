
object Basket {

  def empty[A] = new Basket[A](Nil)
  def apply[A](items: A*): Basket[A] = new Basket(items.toList)

  implicit def basketTraverse: Traverse[Basket] = new Traverse[Basket] {
    def traverse[Context[_]: Applicative, A, B](basket: Basket[A], mapAndLift: A => Context[B]): Context[Basket[B]] = {
      def addToBasket(bas: Basket[B], b: B) = bas + b
      
      basket match {
        case Basket(Nil) => Applicative(Basket(Nil))
        case Basket(first :: rest) => Applicative(
            addToBasket,                        // (Basket[B], B) => Basket[B]
            traverse(Basket(rest), mapAndLift), // Context[Basket[B]]
            mapAndLift(first))                  // Context[B]
      }
    }
  }
}


case class Basket[+A](items: List[A]) {
  def +[B >: A](item: B) = Basket[B](item :: items)
}

trait Fruit {
  def press: Juice
}
case object Orange extends Fruit {
  def press = OrangeJuice
}
case object Apple extends Fruit {
  def press = AppleJuice
}
case object Pear extends Fruit {
  def press = PearJuice
}

trait Juice
case object AppleJuice extends Juice
case object OrangeJuice extends Juice
case object PearJuice extends Juice