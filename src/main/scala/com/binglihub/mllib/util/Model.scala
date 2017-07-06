package com.binglihub.mllib.util

/**
  * The super class of all ml model
  * @tparam T the data type
  */
trait Model[T] {
  def train(data: Array[Array[T]]): Unit
}

/**
  * The super class of all classification model
  * @tparam T the data type
  */
trait Classification[T] extends Model[T] {
  def predict(record: Array[T]): Option[T]
}