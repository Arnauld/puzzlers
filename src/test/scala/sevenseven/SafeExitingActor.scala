package sevenseven

import actors.{TIMEOUT, Exit, Actor}

/*
 *  ~~~~~~   Actor   ~~~~~~
 */
case object OnExit

class SafeExitingActor(pf: PartialFunction[Any,Any], supervisor: Option[Actor]) extends Actor {
  def act : Nothing = react {
      case Exit => {
           println("exit requested, clearing the queue")
           exitRequested
      }
      case message => {
           processMessage(message)
           act
      }
  }

  // reactWithin(0) gives a TIMEOUT as soon as the mailbox is empty
  def exitRequested : Nothing = reactWithin(0) {
     case Exit => {
         println("extra exit requested, ignoring")
         exitRequested // already know about the exit, keep processing
     }
     case TIMEOUT => {
         println("timeout, queue is empty, shutting down")
         supervisor match {
           case None => // no op
           case Some(sup) => sup!OnExit
         }
         exit // TIMEOUT so nothing more to process, we can shut down
     }
     case message => {
         processMessage(message, true)
         exitRequested
     }
  }

  // process is a separate method to avoid duplicating in act and exitRequested
  def processMessage(message : Any) = pf.apply(message)
}




/*
 *  ~~~~~~   Node   ~~~~~~
 */








/*
 *  ~~~~~~   Operator   ~~~~~~
 */







/*
 *  ~~~~~~   Actor   ~~~~~~
 */


