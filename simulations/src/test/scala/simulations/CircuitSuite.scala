package simulations

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite with Checkers {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("demux base") {
    val in = new Wire
    val out = List.fill(1) { new Wire }
    demux(in, Nil, out)

    in.setSignal(false)
    run

    assert(out.head.getSignal === false, "demux 1")

    in.setSignal(true)
    run

    assert(out.head.getSignal === true, "demux 2")
  }

  test("demux 1to2") {
    val in = new Wire
    val d0, d1 = new Wire
    val d = List(d1, d0)
    val c = new Wire

    demux(in, List(c), d)

    testCombination(in, List(c), d, false, List(false), List(false, false), "demux 1")
    testCombination(in, List(c), d, false, List(true), List(false, false), "demux 2")
    testCombination(in, List(c), d, true, List(false), List(false, true), "demux 3")
    testCombination(in, List(c), d, true, List(true), List(true, false), "demux 4")
  }

  test("demux 2to4") {
    val in = new Wire
    val d0, d1, d2, d3 = new Wire
    val d = List(d0, d1, d2, d3)
    val c0, c1 = new Wire
    val c = List(c0, c1)

    demux(in, c, d)

    testCombination(in, c, d, false, List(false, false), List(false, false, false, false), "demux 1")
    testCombination(in, c, d, false, List(false, true), List(false, false, false, false), "demux 2")
    testCombination(in, c, d, false, List(true, false), List(false, false, false, false), "demux 3")
    testCombination(in, c, d, false, List(true, true), List(false, false, false, false), "demux 4")
    testCombination(in, c, d, true, List(false, false), List(false, false, false, true), "demux 5")
    testCombination(in, c, d, true, List(false, true), List(false, false, true, false), "demux 6")
    testCombination(in, c, d, true, List(true, false), List(false, true, false, false), "demux 7")
    testCombination(in, c, d, true, List(true, true), List(true, false, false, false), "demux 8")
  }
  
  test("circuit") {
    check(QuickCircuit)
  }

  def testCombination(in: Wire, c: List[Wire], d: List[Wire], i: Boolean, c_in: List[Boolean], d_in: List[Boolean], comment: String) {
    in.setSignal(i)
    c.zip(c_in).foreach { x => x._1 setSignal x._2 }
    run
    d.zip(d_in).foreach { x => assert(x._1.getSignal === x._2, "%s %s".format(comment, "output match failed")) }
  }
}
