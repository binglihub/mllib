package com.binglihub.mllib.test

import org.scalatest.{FlatSpec, Matchers}
import com.binglihub.mllib.decisiontree.ImpurityFunc._
import com.binglihub.mllib.decisiontree.GainFunc._
import com.binglihub.mllib.decisiontree.DecisionTree._

/**
  * Created by bing on 7/5/17.
  */
class DtreeTest extends FlatSpec with Matchers {


  val data = Array(
    Array("sunny", "hot", "high", "false", "no"),
    Array("sunny", "hot", "high", "true", "no"),
    Array("overcast", "hot", "high", "false", "yes"),
    Array("rainy", "mild", "high", "false", "yes"),
    Array("rainy", "cool", "normal", "false", "yes"),
    Array("rainy", "cool", "normal", "true", "no"),
    Array("overcast", "cool", "normal", "true", "yes"),
    Array("sunny", "mild", "high", "false", "no"),
    Array("sunny", "cool", "normal", "false", "yes"),
    Array("rainy", "mild", "normal", "false", "yes"),
    Array("sunny", "mild", "normal", "true", "yes"),
    Array("overcast", "mild", "high", "true", "yes"),
    Array("overcast", "hot", "normal", "false", "yes"),
    Array("rainy", "mild", "high", "true", "no")
  )

  "The entropy function" should "return None" in {
    entropy(null) should be(None)
    entropy(Array[Double]()) should be(None)
  }

  it should "works" in {
    Math.abs(entropy(Array(0.5, 0.5)).get - 1.0) should be < 0.000001
    Math.abs(entropy(Array(1.0, 0.0)).get) should be < 0.000001
  }


  "The gain function" should "return None" in {
    gain(null) should be(None)
    gain(Array[(String, String)]()) should be(None)
  }

  it should "works" in {
    Math.abs(gain(data.map(x => (x(0), x(4)))).get - 0.247) should be < 0.001
  }

  "The id3 function" should "works" in {
    id3(entropy, data)
  }

}
