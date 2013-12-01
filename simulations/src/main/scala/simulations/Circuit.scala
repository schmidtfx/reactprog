package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () =>
        afterDelay(0) {
          println(
            "  " + currentTime + ": " + name + " -> " + wire.getSignal)
        }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val tmp1, tmp2, tmp3, tmp4, tmp5 = new Wire
    andGate(a1, a1, tmp1)
    andGate(a2, a2, tmp2)
    inverter(tmp1, tmp3)
    inverter(tmp2, tmp4)
    andGate(tmp3, tmp4, tmp5)
    inverter(tmp5, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case Nil => in addAction {
        () => out.head.setSignal(in.getSignal)
      }
      case ch :: ct => {
        val out0 = new Wire
        val out1 = new Wire
        val not = new Wire
        inverter(ch, not)
        andGate(ch, in, out1)
        andGate(not, in, out0)
        val (upper, lower) = out.splitAt(out.length / 2)
        demux(out0, ct, lower)
        demux(out1, ct, upper)
      }
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def demuxExample {
    val in = new Wire
    val d0, d1, d2, d3 = new Wire
    val d = List(d0, d1, d2, d3)
    val c0, c1 = new Wire
    val c = List(c1, c0)
    demux(in, c, d)
    probe("in", in)
    probe("d0", d0)
    probe("d1", d1)
    probe("d2", d2)
    probe("d3", d3)
    probe("c0", c0)
    probe("c1", c1)

    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(false)
    run
  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  //Circuit.andGateExample

  Circuit.demuxExample
}
