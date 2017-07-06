package com.binglihub.mllib.decisiontree

object DecisionTree {

  def id3[T <: AnyVal](impurity: (Iterable[Double]) => Option[Double], data: Array[Array[T]]): Node[T] = {
    null
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
                       val decisionTreeFunc: ((Iterable[Double]) => Option[Double], Array[Array[T]]) => Node[T]
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
    root = decisionTreeFunc(impurity, data)
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
  * @param decision the decision
  * @param children a list of children nodes
  * @tparam T the type of data
  */
private case class Node[T <: AnyVal](val index: Int, val isLeaf: Boolean, val decision: T, val children: Map[T, Node[T]]) {

  /**
    * The constructor for the leaf nodes
    *
    * @param index    the index of feature
    * @param decision the decision
    */
  def this(index: Int, decision: T) = this(index, true, decision, null)

  /**
    * The constructor for the non-leaf nodes
    *
    * @param index    the index of feature
    * @param children the list of children nodes
    */
  def this(index: Int, children: Map[T, Node[T]]) = this(index, false, _, children)

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