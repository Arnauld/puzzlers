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
      val empty = Map[Int,List[Formula]]()
      val solutions = allTrees(6).foldLeft(empty) { (collected,tree) =>
        val nodes = tree.traverse( nodeCollector, List[Node]())
        val initialCtx = NodeContext(Map())
        recursivelyChangeOperator(tree, initialCtx, 0, nodes, evalCollector, collected)
      }

      solutions.keySet.toList.sorted.foreach {
        key =>
          val formulae = solutions(key)
          println (key + " <~~ " + formulae(0) + (if(formulae.size>1) "..."+(formulae.size-1)+" more" else ""))
      }

      solutions.size mustVerify (_ > 2)
    }
  }
}

case class Formula(tree:TreeItem, ctx:NodeContext) {
  override def toString = tree.toString(ctx)
}

object SevenSeven {
  val C7 = Const(7)

  def evalCollector = (tree:TreeItem, ctx:NodeContext, collected:Map[Int,List[Formula]]) => {
    tree.eval(ctx) match {
      case value if(isSuitable(value)) =>
        val asInt = Math.abs(value).toInt
        val formula = Formula(tree, ctx)
        multiMapPut(collected , asInt, formula)
      case _ =>
        collected
    }
  }

  def multiMapPut[K,V](map:Map[K,List[V]], key:K, value:V) = map + (key -> (map.get(key) match {
    case None => List(value)
    case Some(list) => value :: list
  }))

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
    ) yield new Node(left,right).asInstanceOf[TreeItem]
    ).toList
  }

  def isSuitable(d:Double) = Math.abs(d-d.toInt)<1e-6

  def recursivelyChangeOperator[T](
          tree:TreeItem,
          /* evaluation context */
          ctx:NodeContext,
          /* recursive data */
          index:Int, treeNodes:List[Node],
          /* foldLeft alike */
          callback:(TreeItem, NodeContext, T)=>T, arg:T):T =
    index match {
      case x if(x==treeNodes.size) =>
        isSuitable(tree.eval(ctx)) match {
          case true => callback(tree, ctx, arg)
          case false => arg
        }
      case _ =>
        Op.ALL.foldLeft(arg) { (collected, op) =>
          val nextCtx = ctx.changeOperator(treeNodes(index), op)
          recursivelyChangeOperator(tree, nextCtx, index+1, treeNodes, callback, collected)
        }
    }
}


/*
 *  ~~~~~~   Node   ~~~~~~
 */
case class NodeContext(operatorMap:Map[Node,Operator]) {
  def operatorFor(node:Node):Operator = operatorMap.get(node) match {
    case None => Op.ADD // rely on Add by default
    case Some(op) => op
  }
  def changeOperator(node:Node, op:Operator):NodeContext = NodeContext(operatorMap + (node->op))
}

trait TreeItem {
  def eval(ctx:NodeContext):Double
  def toString(ctx:NodeContext):String = toString
  def traverse[T](visitor:(TreeItem,T)=>T, arg:T):T = visitor(this,arg)
}

case class Const(value:Int) extends TreeItem {
  def eval(ctx:NodeContext) = value.asInstanceOf[Double]

  override def toString = "_"+value
}

class Node(val left:TreeItem, val right:TreeItem) extends TreeItem {

  override def traverse[T](visitor: (TreeItem,T)=>T, arg:T):T = {
    val argLeft = left.traverse(visitor, arg)
    val argNode = super.traverse(visitor, argLeft)
    right.traverse(visitor, argNode)
  }

  def eval(ctx:NodeContext) = ctx.operatorFor(this).eval(left.eval(ctx), right.eval(ctx))

  override def toString = "("+left+ " # " +right+")"
  override def toString(ctx:NodeContext) = "("+left.toString(ctx)+ ctx.operatorFor(this) +right.toString(ctx)+")"
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
