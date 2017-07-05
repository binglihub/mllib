package com.binglihub.mllib.test

import org.scalatest.{FlatSpec, Matchers}
import com.binglihub.mllib.decisiontree.ImpurityFunc._
import com.binglihub.mllib.decisiontree.GainFunc._

/**
  * Created by bing on 7/5/17.
  */
class DtreeTest extends FlatSpec with Matchers{


  val data = Array(
    ("sunny","hot","high","false","no"),
    ("sunny","hot","high","true","no"),
    ("overcast","hot","high","false","yes"),
    ("rainy","mild","high","false","yes"),
    ("rainy","cool","normal","false","yes"),
    ("rainy","cool","normal","true","no"),
    ("overcast","cool","normal","true","yes"),
    ("sunny","mild","high","false","no"),
    ("sunny","cool","normal","false","yes"),
    ("rainy","mild","normal","false","yes"),
    ("sunny","mild","normal","true","yes"),
    ("overcast","mild","high","true","yes"),
    ("overcast","hot","normal","false","yes"),
    ("rainy","mild","high","true","no")
  )

  "The entropy function" should "return None" in {
    entropy(null) should be (None)
    entropy(Array[Double]()) should be (None)
  }

  it should "works" in {
    Math.abs(entropy(Array(0.5,0.5)).get - 1.0) should be < 0.000001
    Math.abs(entropy(Array(1.0,0.0)).get) should be < 0.000001
  }


  "The gain function" should "return None" in {
    gain(null) should be (None)
    gain(Array[(String,String)]()) should be (None)
  }

  it should "works" in {
    Math.abs(gain(data.map(x=>(x._1,x._5))).get - 0.247) should be < 0.001
  }

}
