package com.sap.scala.demo

import scala.scalajs.js

// Wind speed and optional direction (given as a heading in degrees)
class Wind(w: js.Dynamic) {
  val speed   = w.speed.asInstanceOf[Double]
  val heading = if (w.deg.toString == "undefined") 0.0 else w.deg.asInstanceOf[Double]
}
