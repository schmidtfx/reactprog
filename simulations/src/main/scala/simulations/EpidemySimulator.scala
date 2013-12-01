package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prerelevanceRate = 0.01
    val deathRate = 0.25
    val infectionRate = 0.4

    val airplanes = false
    val airplaneProbability = 0.01

    val chosenFews = false
    val vipRate = 0.05

    val reduceMobility = false

    val incubationPeriod = 6
    val maybeDie = 14
    val immunePeriod = 16
    val healingPeriod = 18

    def moveTime = 1 + randomBelow(5).toInt
  }

  import SimConfig._

  val persons: List[Person] = List.tabulate(SimConfig.population) { x => new Person(x) }

  def forPersons(row: Int, col: Int)(action: (Person) => Boolean) = {
    def forPersons0(persons: List[Person]): Boolean = persons match {
      case Nil => false
      case x :: xs => {
        if (x.row == row && x.col == col && action(x)) true
        else forPersons0(xs)
      }
    }
    forPersons0(persons)
  }

  def isDangerous(row: Int, col: Int) = forPersons(row, col) { x => x.sick || x.dead }

  def isContagious(row: Int, col: Int) = forPersons(row, col) { x => x.infected || x.sick || x.dead }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = SimConfig.chosenFews && random <= SimConfig.vipRate
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    var nextRow = row
    var nextCol = col

    def scheduleMove {
      /**
       * Rule 1: After each mode (and also after the beginning of the simulation), a person
       * moves to one of their neighbouring rooms within the next 5 days (with equally distributed probability).
       * A mode refers to the decision of a person to move. Note that the first row is considered to be a
       * neighbour of row eight (and vice versa); analogously, the first column is a neighbour of column
       * eight (and vice versa).
       */

      val moveTime = SimConfig.moveTime
      afterDelay(if (SimConfig.reduceMobility) (if (sick) moveTime * 4 else moveTime * 2) else moveTime)(move)
    }

    def move {
      /**
       * Rule 2: A person avoids rooms with sick or dead (visibly infectious) people. This means that
       * if a person is surrounded by visibly infectious people, he does not change position; however,
       * he might change position the next time he tries to move (for example, if a visibly infectious
       * person moved out of one of the neighbouring rooms or became immune).
       */

      if (!dead) {
        def testMove(newRow: Int, newCol: Int): Boolean = {
          if (!isDangerous(newRow, newCol)) {
            nextRow = newRow
            nextCol = newCol
            afterDelay(0)(applyDecisions)
            true
          }
          false
        }

        def testMoves(): Unit = for (m <- scala.util.Random.shuffle(getNeighbors)) {
          if (testMove(m._1, m._2)) return
        }
        testMoves()

        if (SimConfig.airplanes && random <= SimConfig.airplaneProbability) {
          nextRow = (random * SimConfig.roomRows).toInt
          nextCol = (random * SimConfig.roomColumns).toInt
          afterDelay(0)(applyDecisions)
        }
      }

      scheduleMove
    }

    def applyDecisions() {
      row = nextRow
      col = nextCol

      /**
       * Rule 3: When a person moves into a room with an infectious person he might get infected
       * according to the transmissibility rate, unless the person is already infected or immune.
       * A person cannot get infected between moves (this is slightly unrealistic, but will simplify
       * your implementation).
       */
      if (isContagious(row, col) && random <= SimConfig.infectionRate) infect
    }

    def getNeighbors = {
      List(
        ((row - 1 + roomRows) % roomRows, col),
        ((row + 1) % roomRows, col),
        (row, (col + 1) % roomColumns),
        (row, (col - 1 + roomColumns) % roomColumns))
    }

    def infect {
      if (immune || dead || infected) return

      /**
       * Rule 4: When a person becomes infected, he does not immediately get sick, but
       * enters a phase of incubation in which he is infectious but not sick.
       */

      infected = true

      /**
       * Rule 5: After 6 days of becoming infected, a person becomes sick and is therefore visibly infectious.
       */
      afterDelay(SimConfig.incubationPeriod)(sicken)

      /**
       * Rule 6: After 14 days of becoming infected, a person dies with a probability of 25%. Dead people do not move,
       * but stay visibly infectious.
       */
      afterDelay(SimConfig.maybeDie)(maybeDie)

      /**
       * Rule 7: After 16 days of becoming infected, a person becomes immune. He is no longer visibly infectious,
       * but remains infectious. An immune person cannot get infected.
       */
      afterDelay(SimConfig.immunePeriod)(immunize)

      /**
       * After 18 days of becoming infected, a person turns healthy. He is now in the same state as he was before his infection,
       * which means that he can get infected again.
       */
      afterDelay(SimConfig.healingPeriod)(healing)
    }

    def sicken {
      sick = true
    }

    def maybeDie {
      if (random <= SimConfig.deathRate) {
        dead = true
      }
    }

    def immunize() {
      if (dead) return
      immune = true
      sick = false
    }

    def healing() {
      if (dead) return
      infected = false
      sick = false
      immune = false
    }

    afterDelay(0)(move)

    if (id < (SimConfig.prerelevanceRate * SimConfig.population)) {
      infect
    }

  }

}
