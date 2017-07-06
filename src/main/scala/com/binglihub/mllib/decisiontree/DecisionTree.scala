package com.binglihub.mllib.decisiontree

object DecisionTree {

  def id3[T](impurity: (Iterable[Double]) => Option[Double], data: Array[Array[T]]): Option[Node[T]] =
    if (data == null || data.length == 0 || data(0).length == 0) None
    else {

      (0 until data(0).length - 1).map(x=>{
        GainFunc.gain(data.map(y=>(y(x),y.last)))(impurity)
      }).foreach(println)
      None
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