/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /**
   * Request with identifier `id` to insert an element `elem` into the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to check whether an element `elem` is present
   * in the tree. The actor at reference `requester` should be notified when
   * this operation is completed.
   */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to remove the element `elem` from the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /**
   * Holds the answer to the Contains request with identifier `id`.
   * `result` is true if and only if the element is present in the tree.
   */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case i: Insert => root ! i
    case c: Contains => root ! c
    case r: Remove => root ! r
    case GC =>
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot), true)
      root ! CopyTo(newRoot)
  }

  // optional
  /**
   * Handles messages while garbage collection is performed.
   * `newRoot` is the root of the new binary tree where we want to copy
   * all non-removed elements into.
   */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case m: Operation => pendingQueue = pendingQueue :+ m
    case CopyFinished =>
      context.become(normal, true)
      root ! PoisonPill
      root = newRoot
      pendingQueue foreach { newRoot ! _ }
      pendingQueue = Queue.empty
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor with ActorLogging {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normalLeaf

  def normalLeaf: Receive = {
    case Contains(r, id, e) if e == elem && removed => r ! ContainsResult(id, false)
    case Contains(r, id, e) => r ! ContainsResult(id, e == elem)
    case Insert(r, id, e) if e < elem =>
      subtrees = subtrees + (Left -> context.actorOf(props(e, false)))
      context.become(normalMiddleNodeLeft, true)
      r ! OperationFinished(id)
    case Insert(r, id, e) if e == elem =>
      removed = false
      r ! OperationFinished(id)
    case Insert(r, id, e) if e > elem =>
      subtrees = subtrees + (Right -> context.actorOf(props(e, false)))
      context.become(normalMiddleNodeRight, true)
      r ! OperationFinished(id)
    case Remove(r, id, e) if e == elem =>
      removed = true
      r ! OperationFinished(id)
    case Remove(r, id, e) =>
      r ! OperationFinished(id)
    case CopyTo(nr) if !removed =>
      nr ! Insert(self, elem, elem)
      context.become(copying(Set.empty, false), true)
    case CopyTo(nr) if removed =>
      sender ! CopyFinished
    case e => println(s"leaf: $elem $e")
  }

  def normalMiddleNodeLeft: Receive = {
    case Contains(r, id, e) if e == elem => r ! ContainsResult(id, !removed)
    case Contains(r, id, e) if e < elem => subtrees(Left) ! Contains(r, id, e)
    case Contains(r, id, e) => r ! ContainsResult(id, false)
    case Insert(r, id, e) if e == elem =>
      removed = false
      r ! OperationFinished(id)
    case Insert(r, id, e) if e < elem => subtrees(Left) ! Insert(r, id, e)
    case Insert(r, id, e) if e > elem =>
      subtrees = subtrees + (Right -> context.actorOf(props(e, false)))
      context.become(normalMiddleNodeBoth, true)
      r ! OperationFinished(id)
    case Remove(r, id, e) if e == elem =>
      removed = true
      r ! OperationFinished(id)
    case Remove(r, id, e) if e < elem => subtrees(Left) ! Remove(r, id, e)
    case Remove(r, id, e) if e > elem => r ! OperationFinished(id)
    case CopyTo(nr) if !removed =>
      context.become(copying(subtrees.values.toSet, false), true)
      subtrees(Left) ! CopyTo(nr)
      nr ! Insert(self, elem, elem)
    case CopyTo(nr) if removed =>
      context.become(copying(subtrees.values.toSet, true), true)
      subtrees(Left) ! CopyTo(nr)
    case e => println(s"left: $elem $e")
  }

  def normalMiddleNodeRight: Receive = {
    case Contains(r, id, e) if e == elem => r ! ContainsResult(id, !removed)
    case Contains(r, id, e) if e > elem => subtrees(Right) ! Contains(r, id, e)
    case Contains(r, id, e) => r ! ContainsResult(id, false)
    case Insert(r, id, e) if e == elem =>
      removed = false
      r ! OperationFinished(id)
    case Insert(r, id, e) if e < elem =>
      subtrees = subtrees + (Left -> context.actorOf(props(e, false)))
      context.become(normalMiddleNodeBoth, true)
      r ! OperationFinished(id)
    case Insert(r, id, e) if e > elem => subtrees(Right) ! Insert(r, id, e)
    case Remove(r, id, e) if e == elem =>
      removed = true
      r ! OperationFinished(id)
    case Remove(r, id, e) if e > elem => subtrees(Right) ! Remove(r, id, e)
    case Remove(r, id, e) if e < elem => r ! OperationFinished(id)
    case CopyTo(nr) if !removed =>
      context.become(copying(subtrees.values.toSet, false), true)
      subtrees(Right) ! CopyTo(nr)
      nr ! Insert(self, elem, elem)
    case CopyTo(nr) if removed =>
      context.become(copying(subtrees.values.toSet, true), true)
      subtrees(Right) ! CopyTo(nr)
    case e => println(s"right: $elem $e")
  }

  def normalMiddleNodeBoth: Receive = {
    case Contains(r, id, e) if e == elem => r ! ContainsResult(id, !removed)
    case Contains(r, id, e) if e < elem => subtrees(Left) ! Contains(r, id, e)
    case Contains(r, id, e) if e > elem => subtrees(Right) ! Contains(r, id, e)
    case Insert(r, id, e) if e == elem =>
      removed = false
      r ! OperationFinished(id)
    case Insert(r, id, e) if e < elem => subtrees(Left) ! Insert(r, id, e)
    case Insert(r, id, e) if e > elem => subtrees(Right) ! Insert(r, id, e)
    case Remove(r, id, e) if e == elem =>
      removed = true
      r ! OperationFinished(id)
    case Remove(r, id, e) if e < elem => subtrees(Left) ! Remove(r, id, e)
    case Remove(r, id, e) if e > elem => subtrees(Right) ! Remove(r, id, e)
    case CopyTo(nr) if !removed =>
      context.become(copying(subtrees.values.toSet, false), true)
      nr ! Insert(self, elem, elem)
      subtrees.values.foreach { _ ! CopyTo(nr) }
    case CopyTo(nr) if removed =>
      context.become(copying(subtrees.values.toSet, true), true)
      subtrees.values.foreach { _ ! CopyTo(nr) }
    case e => println(s"both: $elem $e")
  }

  // optional
  /**
   * `expected` is the set of ActorRefs whose replies we are waiting for,
   * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
   */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished if !expected.isEmpty =>
      val newExpected = expected - sender
      if (newExpected.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        subtrees.values.foreach { _ ! PoisonPill }
      } else {
        context.become(copying(newExpected, insertConfirmed), true)
      }
    case OperationFinished(_) if expected.isEmpty =>
      context.parent ! CopyFinished
      subtrees.values.foreach { _ ! PoisonPill }
    case OperationFinished(_) =>
      context.become(copying(expected, true))

    case e => println(s"copy $elem $e")
  }

}
