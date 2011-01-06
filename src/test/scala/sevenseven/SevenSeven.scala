package sevenseven

import org.specs.Specification

class SevenSevenSpecs extends Specification {

  import SevenSeven._

  "SevenSeven" should {

    "provide a way to obtain all trees" in {
      val trees = allTrees(6)
      trees.size mustVerify (_ > 2)
      trees.foreach { t => println(t) }
    }

    "provide a way to obtain all combinaisons" in {
      val empty = Map[Int,List[String]]()
      val solutions = allTrees(6).foldLeft(empty) { (collected,tree) =>
        val nodes = tree.traverse( nodeCollector, List[Node]())
        nodes.size mustVerify (_ > 2)
        recursivelyChangeOperator(tree, 0, nodes, mapCollector, collected)
      }

      solutions.keySet.toList.sorted.foreach {
        key =>
          val formulae = solutions(key)
          println (key + " <~~ " + formulae(0) + (if(formulae.size>1) "..."+(formulae.size-1)+" more" else ""))
      }
    }
  }
}

object SevenSeven {
  val C7 = Const(7)

  def mapCollector = (tree:TreeItem, collected:Map[Int,List[String]]) => {
    val asInt = Math.abs(tree.eval).toInt
    collected + (asInt -> (collected.get(asInt) match {
      case None => List(tree.toString)
      case Some(list) => tree.toString :: list
    }))
  }

  def nodeCollector = (item:TreeItem, collected:List[Node]) =>
    if(item.isInstanceOf[Node])
      item.asInstanceOf[Node] :: collected 
    else
      collected

  def allTrees(size:Int):List[TreeItem] = {
    if(size==0)
      return List(C7);

    (for(
      i <- 0 until size;
      left <- allTrees(i);
      right <- allTrees(size-i-1)
    ) yield new Node(left,right, Op.ADD).asInstanceOf[TreeItem]
    ).toList
  }

  def isSuitable(d:Double) = Math.abs(d-d.toInt)<1e-6

  def recursivelyChangeOperator[T](tree:TreeItem, index:Int, treeNodes:List[Node], callback:(TreeItem,T)=>T, arg:T):T =
    index match {
      case x if(x==treeNodes.size) =>
        isSuitable(tree.eval) match {
          case true => callback(tree, arg)
          case false => arg
        }
      case _ =>
        Op.ALL.foldLeft(arg) { (collected, op) =>
          // the only? mutable part... must find a smart way to clone the tree
          treeNodes(index).operator = op
          recursivelyChangeOperator(tree, index+1, treeNodes, callback, collected)
        }
    }
}


/*
 *  ~~~~~~   Node   ~~~~~~
 */

trait TreeItem {
  def eval:Double
  def traverse[T](visitor:(TreeItem,T)=>T, arg:T):T = visitor(this,arg)
}

case class Const(value:Int) extends TreeItem {
  def eval = value.asInstanceOf[Double]

  override def toString = "_"+value
}

class Node(val left:TreeItem, val right:TreeItem, var operator:Operator) extends TreeItem {

  override def traverse[T](visitor: (TreeItem,T)=>T, arg:T):T = {
    val argLeft = left.traverse(visitor, arg)
    val argNode = super.traverse(visitor, argLeft)
    right.traverse(visitor, argNode)
  }

  def eval = operator.eval(left.eval, right.eval)

  override def toString = "("+left+operator+right+")"
}

/*
 *  ~~~~~~   Operator   ~~~~~~
 */

case class Operator(f:(Double, Double)=>Double, symbol:String) {
  def eval(value1:Double, value2:Double):Double = f(value1, value2)
  override def toString = symbol
}

object Op {
  val ADD:Operator = new Operator((_+_), "+")
  val SUB:Operator = new Operator((_-_), "-")
  val MUL:Operator = new Operator((_*_), "*")
  val DIV:Operator = new Operator((_/_), "/")

  val ALL = List(ADD,SUB,MUL,DIV)
}
