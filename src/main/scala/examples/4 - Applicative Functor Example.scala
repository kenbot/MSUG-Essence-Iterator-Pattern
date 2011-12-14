
case class Monster(val name: String, val health: Int, val damage: Int)
case class ValidationFailedException(errors: List[String]) 
  extends RuntimeException("Validation failed: " + errors.mkString(", "))


object ImperativeValidationExample {

  
  def createValidMonster(name: String, health: Int, damage: Int): Monster = {

    // We don't really have a String, Int and Int; we have "a String that might be valid or not, an Int that 
    // that might be valid or not, and an Int that might be valid or not".  In other words, the values exist
    // within a validation context, which is our applicative functor.
    var errors = List.empty[String]


    if (name.length <= 0) {
      // "Danger zone": We have failed once, and can never un-fail.  
      // Keep going though; there might be more failures to accumulate.
      errors ::= "Name can't be empty"
    }
    // "Safe zone", where the name is valid and safe to use.
    // Note value and move on.
    
    if (health <= 0 || health >= 100) {
      errors ::= "Health must be > 0 and < 100"
    }
    
    if (damage <= 0) {
      errors ::= "Damage must be > 0"
    }
    
    if (errors.isEmpty) {
      // Execute if ALL computations succeeded
      Monster(name, health, damage)
    }
    else {
      // Execute if ANY computations failed.
      throw new ValidationFailedException(errors) // Really being used like a parallel return value
    }
  } 
  
  val myValidMonster: Monster = createValidMonster("Orc", 33, 44)
  // res0: Monster = Monster(Orc,33,44)
  
  def myFailedMonster: Monster = createValidMonster("Orc", 101, -5) // Throws exception with failures!
  // ValidationFailedException: Validation failed: Damage must be > 0, Health must be > 0 and < 100
  //        at ImperativeValidationExample$.createValidMonster(4 - Applicative Functor Example.scala:40)
  //        at ImperativeValidationExample$.myFailedMonster(4 - Applicative Functor Example.scala:47)
}





object ApplicativeValidationExample {

  def validateName(name: String) = if (name.length > 0) Success(name)
                                   else Failure("Name can't be empty" :: Nil)
  def validateHealth(health: Int) = if (health > 0 && health < 100) Success(health)
                                    else Failure("Health must be > 0 and < 100" :: Nil)
  def validateDamage(damage: Int) = if (damage > 0) Success(damage)
                                    else Failure("Damage must be > 0" :: Nil)        
                                    
  def createValidMonster(name: Validation[String], health: Validation[Int], damage: Validation[Int]): Validation[Monster] = {
    // Applicative now does the heavy "lifting" for us!
    Applicative(Monster.apply, name, health, damage)
  }
  
  val myValidMonster: Validation[Monster] = createValidMonster(validateName("Orc"), validateHealth(33), validateDamage(44))
  // res0: Validation[Monster] = Success(Monster(Orc, 33, 44))
  
  val myFailedMonster: Validation[Monster] = createValidMonster(validateName("Orc"), validateHealth(101), validateDamage(-3))
  // res1: Validation[Monster] = Failure(List(Health must be > 0 and < 100, Damage must be > 0))
}


