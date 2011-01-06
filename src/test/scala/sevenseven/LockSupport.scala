package sevenseven

import org.specs.Specification
import actors.Actor._
import java.util.concurrent.locks.{Lock, ReentrantLock}
import LockSupport._
import actors.{TIMEOUT, Exit, Actor}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}


object LockSupport {
  def withinLock[T](lock:Lock)(f: =>T):T = try {
    lock.lock
    f
  }finally lock.unlock
}




/*
 *  ~~~~~~   Node   ~~~~~~
 */








/*
 *  ~~~~~~   Operator   ~~~~~~
 */





