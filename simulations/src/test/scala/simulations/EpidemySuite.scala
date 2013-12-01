package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpidemySuite extends FunSuite {

  test("prevalence rate") {
    val prevalenceRate = 0.01

    val es = new EpidemySimulator
    val numInfected = es.persons.count(_.infected)

    assert(numInfected == es.SimConfig.population * prevalenceRate,
      "prevalence rate should be 0.01")
  }

  test("dead person stays dead") {
    val es = new EpidemySimulator

    val chosenOne = es.persons.head
    chosenOne.infected = true
    chosenOne.sick = true
    chosenOne.dead = true
    chosenOne.immune = false

    val (row, col) = (chosenOne.row, chosenOne.col)

    val testDays = 100

    while (!es.agenda.isEmpty && es.agenda.head.time < testDays) {
      es.next

      assert(chosenOne.dead == true, "Dead person should keep dead state")
      assert(chosenOne.infected == true, "Dead person keeps infected")
      assert(chosenOne.immune == false, "Dead person cannot become immune")
      assert(chosenOne.sick == true, "Dead person keeps sick")
      assert(chosenOne.col == col && chosenOne.row == row, "Dead person cannot move")
    }
  }

  test("life cycle") {
    var personDied = true;
    while (!personDied) {
      val es = new EpidemySimulator

      val incubationTime = 6
      val dieTime = 14
      val immuneTime = 16
      val healTime = 18

      val prevalenceRate = 0.01
      val transRate = 0.4
      val dieRate = 0.25

      val infectedPerson = (es.persons.find { _.infected }).get

      //before incubation time
      while (es.agenda.head.time < incubationTime) {
        assert(infectedPerson.infected == true, "Infected person keeps infected in 6 days")
        assert(infectedPerson.sick == false, "Infected person does not get sick in 6 days")
        assert(infectedPerson.immune == false, "Infected person cannot become immune in 6 days")
        assert(infectedPerson.dead == false, "Infected person does not die in 6 days")
        es.next
      }

      //incubation time has passed, there should be an event for getting sick
      assert(es.agenda.head.time == incubationTime, "You should set a 'sick' event after incubation time")
      while (es.agenda.head.time == incubationTime) es.next
      assert(infectedPerson.sick == true, "Infected person should become sick after 6 days")

      //wait for dieTime
      while (es.agenda.head.time < dieTime) {
        assert(infectedPerson.infected == true, "Sick person keeps infected")
        assert(infectedPerson.sick == true, "Sick person keeps sick before turning immune")
        assert(infectedPerson.immune == false, "Sick person is not immune")
        assert(infectedPerson.dead == false, "Sick person does not die before 14 infected days")
        es.next
      }

      assert(es.agenda.head.time == dieTime, "You should set a 'die' event (decides with a probability 25% whether the person dies) after 14 days")
      while (es.agenda.head.time == dieTime) es.next
    }
  }

  test("transmissibility rate") {
    var infectedTimes = 0
    for (i <- 0 to 100) {
      val es = new EpidemySimulator
      val healthyPerson = (es.persons find { p => !p.infected }).get
      es.persons.filter(p => p != healthyPerson) foreach { _.infected = true }

      while (es.agenda.head.time < 6) es.next

      infectedTimes = infectedTimes + (if (healthyPerson.infected) 1 else 0)
    }
    assert(infectedTimes > 0, "A person should get infected according to the transmissibility rate when he moves into a room with an infectious person")
  }

  test("dies within 14 days") {
    val es = new EpidemySimulator
    val start = es.persons.filter(p => p.infected)
    val mortalityRate = (start.length * .25).toInt

    while (es.agenda.head.time < 14) {
      es.next
    }
    val alive = start.filter(p => p.dead)
    val diff = mortalityRate - alive.length
    assert(diff === 0, "25% of people should have died. Have " + diff + " left")
  }

  test("moves within 5 days") {
    val es = new EpidemySimulator
    val start = collection.mutable.HashMap[es.Person, (Int, Int)]()
    es.persons foreach { p =>
      start.put(p, (p.row, p.col))
    }
    while (es.agenda.head.time < 5) {
      es.persons foreach { p =>
        start.get(p) match {
          case Some(last) if (last != (p.row, p.col)) => start.remove(p)
          case _ =>
        }
      }
      es.next
    }
    assert(start.isEmpty)
  }
  
  test("moves within 5 days 2") {
    val es = new EpidemySimulator
    val start = collection.mutable.HashMap[es.Person, (Int, Int)]()
    es.persons foreach { p =>
      start.put(p, (p.row, p.col))
    }
    while(es.agenda.head.time < 5) {
      es.persons foreach { p =>
        start.get(p) match {
          case Some((origRow, origCol)) => {
            val dRow = (p.row - origRow + es.SimConfig.roomRows) % es.SimConfig.roomRows
            val dCol = (p.col - origCol + es.SimConfig.roomColumns) % es.SimConfig.roomColumns
            assert(dCol == 0 || dRow == 0, "Detected diagonal movement")
            if ((dCol == 0 && (dRow == 1 || dRow == es.SimConfig.roomRows - 1))
              || (dRow == 0 && (dCol == 1 || dCol == es.SimConfig.roomColumns - 1))){
              println(s"moved person ${p.id}")
              start.remove(p)
            }
          }
          case _ =>
        }
      }
      es.next
    }
    assert(start.isEmpty, "All persons should move within 5 days")
  }
}