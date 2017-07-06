package com.binglihub.mllib.decisiontree


/**
  * Created by bing on 7/5/17.
  */
class DecisionTree {

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
    * make decision
    * @param record an array contains a record
    * @return the decision result
    */
  def predict(record: Array[T]): T =
    if (isLeaf) decision
    else children.get(record(index)).get.predict(record)
}