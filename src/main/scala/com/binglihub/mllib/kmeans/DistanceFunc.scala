package com.binglihub.mllib.kmeans

/**
  * This object contains KMeans functions
  */
object DistanceFunc {
  /**
    * Euclidean distance function
    * @param a vector a
    * @param b vector b
    * @return a Some[Double] value represents the distance between a and b, or None for wrong input vectors
    */
  def euclidean(a:Array[Double],b:Array[Double]):Option[Double]=
    if (a==null || b ==null||a.length!=b.length) None
    else Some(
      Math.sqrt(
        (0 until a.length).foldLeft(0.0){
          case(value:Double,index:Int) => value+(a(index)-b(index))*(a(index)-b(index))
        }
      )
    )



}
