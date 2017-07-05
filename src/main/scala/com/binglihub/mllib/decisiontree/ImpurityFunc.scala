package com.binglihub.mllib.decisiontree

/**
  * This object contains all impurity functions
  */
object ImpurityFunc {
  /**
    * This function calculates the entropy of a given probability array
    *
    * @param p an array contains probability values
    * @return a Some(double) value represents the entropy value, None for wrong input
    */
  def entropy(p: Array[Double]): Option[Double] =
    if (p == null || p.length == 0) None
    else Some(p.foldLeft(0.0)({
      case (b, x) =>
        if(x<0.00000001) b
        else b - x * (Math.log(x) / Math.log(2))
    }))

}
