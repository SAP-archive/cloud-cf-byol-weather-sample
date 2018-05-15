package com.sap.scala.demo

import scala.scalajs.js

// Basic atmospheric conditions:
// o Air temperature plus optional max/min variation values
// o Humidity
// o Air pressure.  This is either a simple value in mBar, or it is a
//   pair of values: air pressure at sea level and air pressure at
//   ground level
class WeatherMain(weatherMain: js.Dynamic) {
  val temp        = weatherMain.temp.asInstanceOf[Double]
  val airPressure = weatherMain.pressure.asInstanceOf[Double]
  val humidity    = weatherMain.humidity.asInstanceOf[Int]
  val temp_min    = weatherMain.temp_min.asInstanceOf[Double]
  val temp_max    = weatherMain.temp_max.asInstanceOf[Double]

  val sea_level  = if (weatherMain.sea_level.toString == "undefined")  0.0 else weatherMain.sea_level.asInstanceOf[Double]
  val grnd_level = if (weatherMain.grnd_level.toString == "undefined") 0.0 else weatherMain.grnd_level.asInstanceOf[Double]
}

