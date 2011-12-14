

object TraverseAlgorithms {
  
  // Returns a function which just executes the given expression, ignoring its parameter.
  def justDo[S](something: => S) = (_: Any) => something
  
  
  // The collect algorithm!  Takes an effectful context derived from each item, a mapping function, 
  // a traversable, and returns the traversable mapped inside the context.
  def collect[Context[_]: Applicative, T[_]: Traverse, A, B](
          effectFn: A => Context[Unit],           // a -> m ()
          mapFn: A => B,                          // a -> b
          traversable: T[A]): Context[T[B]] = {   // t a
                                                  // m (t b)

    Traverse(traversable){ a => Applicative(justDo(mapFn(a)), effectFn(a)) }
  }

  // Disperse algorithm.  Takes some applicative context, a traversable, a way to combine the 
  // values in the traversable and the context, and yields a context of the traversable 
  // containing the results. 
  def disperse[Context[_]: Applicative, T[_]: Traverse, A, B, C](
          contextB: Context[B],                  // m b
          combineAB: (A, B) => C,                // a -> b -> c
          traversableA: T[A]): Context[T[C]] = { // t a
                                                 // m (t b)
          
    Traverse(traversableA){ a => Applicative(combineAB(a, _: B), contextB) }
  }
}

object ImperativeCollectExample {

  // Contains different concerns that can be abstracted:
  def squishAndCount(fruitBasket: Basket[Fruit]): (Basket[Juice], Int) = {
    var count = 0 
    var juiceBasket = Basket.empty[Juice]
    
    for (f <- fruitBasket.items) {
      count += 1 // 1. Accumulating a count
      juiceBasket += f.press // 2. Mapping fruits to juices
      
    }
    return (juiceBasket, count)
  }
  
  squishAndCount(Basket(Orange, Apple))
  // res0: (Basket[Juice], Int) = (Basket(List(AppleJuice, OrangeJuice)),2)
}

object FunctionalCollectExample {

  import TraverseAlgorithms._
  import State._
  
  var counterState = 0
  
  // a -> m ()
  // 1. Accumulates a count.
  def count(a: Any) = State[Int, Unit](n => ((), n+1))
  
  // a -> b
  // 2. Maps Fruits to juices
  def squishFruit(f: Fruit): Juice = f.press

  
  // Much more modular!  We have separated the different iteration aspects of squishAndCount().
  def squishAndCount(fruitBasket: Basket[Fruit]): (Basket[Juice], Int) = {
    // m (t b)
    collect[State.apply[Int]#toArg1, Basket, Fruit, Juice](
        count, squishFruit, fruitBasket).runState(0)
  }
  
  squishAndCount(Basket(Orange, Apple))
  // res0: (Basket[Juice], Int) = (Basket(List(OrangeJuice, AppleJuice)),2)
}

object ImperativeDisperseExample {

  // Again, the different aspects of the iteration are smooshed together.
  def labelWithCount(fruitBasket: Basket[Fruit]): (Basket[String], Int) = {
    var count = 0
    var labels = Basket.empty[String]
    
    for (f <- fruitBasket.items) {
      count += 1                              // 1. Accumulating a count
      labels += "Fruit #" + count + ": " + f  // 2. Mapping from both the count and the item
    }
    (labels, count) 
  }

  labelWithCount(Basket(Orange, Apple, Pear))
  // res0: (Basket[String], Int) = (Basket(List(Fruit #3: Pear, Fruit #2: Apple, Fruit #1: Orange)),3)
}


object FunctionalDisperseExample {

  import TraverseAlgorithms._
  import State._
  
  // Again, we have separated the different aspects of the method.
  
  // m b
  // 1. Accumulating a count
  val count = State[Int, Int]((n: Int) => (n+1, n+1))

  // a -> b -> c
  // 2. Mapping from both the count and the item
  def makeLabel(fruit: Fruit, i: Int) = "Fruit #" + i + ": " + fruit

  def labelWithCount(fruitBasket: Basket[Fruit]): (Basket[String], Int) = {
    // m (t b)
    disperse[State.apply[Int]#toArg1, Basket, Fruit, Int, String](
        count, makeLabel, fruitBasket).runState(0)
  }
  
  labelWithCount(Basket(Orange, Apple, Pear))
}