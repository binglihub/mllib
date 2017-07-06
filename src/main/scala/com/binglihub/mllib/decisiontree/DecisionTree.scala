package com.binglihub.mllib.decisiontree


object DecisionTree {

  def id3[T](impurity: (Iterable[Double]) => Option[Double], data: Array[Array[T]]): Option[Node[T]] =
    if (data == null || data.length == 0 || data(0).length == 0) None
    else {

      def find(used:List[Int],dep:Int,maxDep:Int,d:Array[Array[T]]):Node[T]={

        val index = (0 until d(0).length - 1)
          .filter(!used.contains(_))
          .map(x=>{x->GainFunc.gain(d.map(y=>(y(x),y.last)))(impurity)})
          .maxBy(_._2.get)._1

        val values = d.map(_(index)).groupBy(x=>x).map(_._1) //d.map(_(index)).groupBy(_).map(_._1)
        var children = Map[T,Node[T]]()

        values.foreach(x=>{
          val subset = d.filter(_(index)==x)
          val classes= subset.map(_.last).groupBy(x=>x)
          if(classes.size==1) children += ((x, new Node[T](index,classes.last._2(0))))
          else if (dep >= maxDep || used.size == d(0).length-2) children += ((x, new Node[T](index, classes.maxBy(_._2.size)._2(0))))
          else children += ((x, find(index::used,dep+1,maxDep,subset)))
        })

        new Node[T](index,children)
      }


     Some(find(List[Int](),1,data(0).length-1,data))
    }

}

/**
  * This class contains the decision tree information
  *
  * @param impurity         the impurity function
  * @param decisionTreeFunc the decision tree function
  * @tparam T the type of data
  */
class DecisionTree[T](
                       val impurity: (Iterable[Double]) => Option[Double],
                       val decisionTreeFunc: ((Iterable[Double]) => Option[Double], Array[Array[T]]) => Option[Node[T]]
                     ) {
  private var root: Node[T] = null

  /**
    * the short constructor
    */
  def this() = this(ImpurityFunc.entropy, DecisionTree.id3)

  /**
    * build a new decision tree
    *
    * @param data the training data
    */
  def train(data: Array[Array[T]]): Unit = {
    root = decisionTreeFunc(impurity, data).get
  }

  /**
    * make a decision
    *
    * @param record an array contains a record
    * @return the decision result
    */
  def predict(record: Array[T]): Option[T] =
    if (root == null) None
    else Some(root.predict(record))


}

/**
  * This class contains node information in the decision tree
  *
  * @param index    the index of feature
  * @param isLeaf   true is this is a leaf, false otherwise
  * @param children a list of children nodes
  * @tparam T the type of data
  */
case class Node[T](val index: Int, val isLeaf: Boolean, val children: Map[T, Node[T]]) {

  private var decision:T = _
  /**
    * The constructor for the leaf nodes
    *
    * @param index    the index of feature
    * @param decision the decision
    */
  def this(index: Int, decision: T) = {
    this(index, true, null)
    this.decision = decision
  }

  /**
    * The constructor for the non-leaf nodes
    *
    * @param index    the index of feature
    * @param children the list of children nodes
    */
  def this(index: Int, children: Map[T, Node[T]]) = this(index, false, children)

  /**
    * make a decision
    *
    * @param record an array contains a record
    * @return the decision result
    */
  def predict(record: Array[T]): T =
    if (isLeaf) decision
    else children.get(record(index)).get.predict(record)
}