package com.binglihub.mllib.test

import org.scalatest.{FlatSpec, Matchers}
import com.binglihub.mllib.decisiontree.ImpurityFunc._

/**
  * Created by bing on 7/5/17.
  */
class DtreeTest extends FlatSpec with Matchers{

  "The entropy function" should "return None" in {
    entropy(null) should be (None)
    entropy(Array[Double]()) should be (None)
  }

  it should "works" in {
    Math.abs(entropy(Array(0.5,0.5)).get - 1.0) should be < 0.000001
    Math.abs(entropy(Array(1.0,0.0)).get) should be < 0.000001

//    println(entropy(Array(1.0,0.0)))
  }

}
