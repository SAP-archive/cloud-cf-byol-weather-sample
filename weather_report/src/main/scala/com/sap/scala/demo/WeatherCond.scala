package com.sap.scala.demo

import scala.scalajs.js

// A set of weather conditions.
// Each city will have one or more instance
class WeatherCond(weatherItem: js.Dynamic) {
  val weatherId = weatherItem.id.asInstanceOf[Int]
  val main      = weatherItem.main.asInstanceOf[String]
  val desc      = weatherItem.description.asInstanceOf[String]
  val icon      = weatherItem.icon.asInstanceOf[String]
}
