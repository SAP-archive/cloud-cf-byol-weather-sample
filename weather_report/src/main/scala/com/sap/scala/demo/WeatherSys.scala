package com.sap.scala.demo

import scala.scalajs.js
import java.lang.{Long => JLong}

// Country, sunrise and sunset times.
class WeatherSys(sys: js.Dynamic) {
  val country = if (sys.country.toString == "undefined") "" else sys.country.asInstanceOf[String]
  val sunrise = if (sys.sunrise.toString == "undefined") 0  else JLong.parseLong(sys.sunrise.toString)
  val sunset  = if (sys.sunset.toString  == "undefined") 0  else JLong.parseLong(sys.sunset.toString)
}
