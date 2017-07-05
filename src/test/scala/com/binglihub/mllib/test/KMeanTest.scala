package com.binglihub.mllib.test

import org.scalatest.{FlatSpec, Matchers}
import com.binglihub.mllib.kmeans.DistanceFunc._

/**
  * KMeans test functions
  */
class KMeanTest extends FlatSpec with Matchers{

  "The euclidean function" should "return None" in {
    euclidean(null,Array(0.0)) should be (None)
    euclidean(Array(0.0),null) should be (None)
    euclidean(Array(0.1),Array(0.1,0.2)) should be (None)
  }

  it should "return Some(5)" in {
    Math.abs(euclidean(Array(0.0,3.0),Array(4.0,0.0)).get - 5.0) should be < 0.00001
  }

}
