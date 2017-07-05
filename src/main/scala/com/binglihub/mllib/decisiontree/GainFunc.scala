package com.binglihub.mllib.decisiontree

/**
  * This object contains gain functions
  */
object GainFunc {

  implicit val impurity: (Iterable[Double]) => Option[Double] = ImpurityFunc.entropy

  /**
    * This function calculates the information gain value
    *
    * @param d        an 2d array contains data (only split and class)
    * @param impurity the impurity function
    * @return Some(Double) represents the information gain value, None for wrong input
    */
  def gain[T, K](d: Array[(T, K)])(implicit impurity: (Iterable[Double]) => Option[Double]): Option[Double] = {
    if (d == null || d.length == 0) None
    else {
      val di = impurity(d.groupBy(_._2).map(_._2.length * 1.0 / d.length)).get
      val s = d.groupBy(_._1).map {
        case (_, v) =>
          (v.length * 1.0 / d.length * impurity(v.groupBy(_._2).map(_._2.length * 1.0 / v.length)).get)
      }
      Some(s.foldLeft(di)(_ - _))
    }
  }

}
