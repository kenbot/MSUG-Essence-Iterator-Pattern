


object AwfulOptionMonadExampleWithNulls {
  // All these values are nullable! :(
  class Helicopter(val pilot: Pilot, val ejectorSeat: EjectorSeat)
  class Pilot
  class EjectorSeat(val ejectButton: Button)
  class EjectAction(pilot: Pilot, ejectButton: Button)
  class Button
  
  def getEjectAction(helicopter: Helicopter): EjectAction = {
    
    val ejectAction: EjectAction = null
    
    if (helicopter != null) { // Enter helicopter "safe zone", if possible
      val pilot = helicopter.pilot
      val ejectorSeat = helicopter.ejectorSeat
      
      if (pilot != null && ejectorSeat != null) { // Enter pilot and ejectorSeat "safe zone", if possible
        val ejectButton = ejectorSeat.ejectButton
        if (ejectButton != null) {  // Enter ejectButton "safe zone"
        
          // Success! All the values are finally usable down here.
          return new EjectAction(pilot, ejectButton)
        }
      }
    
    }
  
    // Failure value; if any test fails, it will short-circuit, leaving us here.
    return null;
  }
}

object BetterOptionMonadExample {
  class Helicopter(val pilot: Option[Pilot], val ejectorSeat: Option[EjectorSeat])
  class Pilot
  class EjectorSeat(val ejectButton: Option[Button])
  class EjectAction(pilot: Pilot, ejectButton: Button)
  class Button
  
  // The flatMap/map methods on the Option class let us do this monadically.
  def getEjectActionForComprehension(helicopter: Option[Helicopter]): Option[EjectAction] = 
    for {
      heli <- helicopter
      pilot <- heli.pilot
      ejectorSeat <- heli.ejectorSeat
      ejectButton <- ejectorSeat.ejectButton
    } yield new EjectAction(pilot, ejectButton)

    
  // Which desugars to:
  def getEjectActionDesugared(helicopter: Option[Helicopter]): Option[EjectAction] = {
    helicopter.flatMap { heli =>
      heli.pilot.flatMap { pilot =>
        heli.ejectorSeat.flatMap { ejectorSeat => 
          // For the innermost computation, we generally use map instead of flatMap.  This is equivalent to the 
          // "pure" method in our monad definition.  Why?  Let's look at two ways of writing it:

          // Too verbose - we have to manually lift the return value to an Option.
          ejectorSeat.ejectButton.flatMap { ejectButton => 
            Some(new EjectAction(pilot, ejectButton))
          }
          
          // Preferred; we can just return our value, and map() lifts it for us.
          ejectorSeat.ejectButton.map { ejectButton => 
            new EjectAction(pilot, ejectButton)
          }
        }
      }
    }
  }
  
}

object ListMonadExample {

  class RealityShow(val contestants: List[Bogan])
  class Bogan(val criminalRecord: List[Incident])
  class Incident(val crime: Crime, date: String)
  class Crime(val name: String)

  def getEligibleBogansManual(realityShow: RealityShow, disqualifyingCrimes: List[Crime]): List[Bogan] = {
    var disqualifiedContestants = List.empty[Bogan]
    for (bogan <- realityShow.contestants) {
      for (incident <- bogan.criminalRecord) {
        for (crime <- disqualifyingCrimes) { 
          if (crime == incident.crime) {
            disqualifiedContestants ::= bogan
          } 
        }
      }
    }
    disqualifiedContestants
  }

  def getEligibleBogansForComprehension(realityShow: RealityShow, disqualifyingCrimes: List[Crime]): List[Bogan] = 
    for {
      bogan <- realityShow.contestants
      incident <- bogan.criminalRecord
      crime <- disqualifyingCrimes
      if crime == incident.crime
    } yield bogan
    
  def getEligibleBogansDesugared(realityShow: RealityShow, disqualifyingCrimes: List[Crime]): List[Bogan] = {
    realityShow.contestants.flatMap { bogan =>
      bogan.criminalRecord.flatMap { incident =>
      
        // The "if" guard desugars as this withFilter call.  Why not filter?  Well, both map() and filter()
        // do O(n) traversals of the collection.  If we do withFilter(), it will store the predicate and use it
        // with map, doing only a single O(n) traversal; much more efficient.
        disqualifyingCrimes.withFilter { crime =>
          incident.crime == crime
        }.map {_ =>  
          bogan  // And again, we map() at the end.
        }
      }
    }
  }

}

