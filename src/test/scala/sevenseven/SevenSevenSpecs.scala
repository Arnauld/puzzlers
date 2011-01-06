package sevenseven

import org.specs.Specification
import actors.Actor._
import java.util.concurrent.locks.{Lock, ReentrantLock}
import LockSupport._
import actors.{TIMEOUT, Exit, Actor}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

class SevenSevenSpecs extends Specification {

  import SevenSeven._

  "SevenSeven" should {

    "provide a way to obtain all trees" in {
      val trees = allTrees(6)
      trees.size mustVerify (_ > 2)
      trees.foreach { t => println(t) }
    }

    "provide a way to obtain all combinaisons" in {
      val start = System.nanoTime
      val solutions = calculateSolutions
      val end = System.nanoTime

      val total = new AtomicInteger
      solutions.keySet.toList.sorted.foreach {
        key =>
          val formulae = solutions(key)
          total.addAndGet(formulae.size)
          //println (key + " <~~ " + formulae(0) + (if(formulae.size>1) "..."+(formulae.size-1)+" more" else ""))
      }

      println("Elapsed: " + (end-start)/1000000L + "ms #" + total.get)

      solutions.size mustVerify (_ > 2)
    }

    "provide a way to obtain all combinaisons using actors" in {
      val lock = new ReentrantLock
      val condition = lock.newCondition
      val runnings = new AtomicInteger

      var solutions = Map[Int,List[Formula]]()
      val pF:PartialFunction[Any, Any] = {
        case f:Formula =>
              solutions = multiMapPut(solutions, f.evalPositive, f)
      }

      val allProcessed = new AtomicBoolean
      val supervisor = actor {
        loop {
          react {
            case OnExit =>
              allProcessed.set(true)
              withinLock(lock) { condition.signal }
              exit('stop)
          }
        }
      }
      val collector = new SafeExitingActor (pF, Some(supervisor))
      collector.start

      val start = System.nanoTime

      allTrees(6).foreach { tree =>
        val nodes = tree.traverse( nodeCollector, List[Node]())
        runnings.incrementAndGet
        scala.concurrent.ops.spawn {
          val initialCtx = NodeContext(Map())
          recursivelyChangeOperator(tree, initialCtx, 0, nodes, evalCollectorActor, collector)
          runnings.decrementAndGet
          withinLock(lock) {
            condition.signal
          }
        }
      }

      while (runnings.get > 0)
        withinLock(lock) {
          condition.await
        }
      collector ! Exit

      while (!allProcessed.get)
        withinLock(lock) {
          condition.await
        }

      val end = System.nanoTime

      val total = new AtomicInteger
      solutions.keySet.toList.sorted.foreach {
        key =>
          val formulae = solutions(key)
          total.addAndGet(formulae.size)
          //println (key + " <~~ " + formulae(0) + (if(formulae.size>1) "..."+(formulae.size-1)+" more" else ""))
      }

      println("Elapsed: " + (end-start)/1000000L + "ms #" + total.get)

      solutions.size mustVerify (_ > 2)
    }
  }
}
