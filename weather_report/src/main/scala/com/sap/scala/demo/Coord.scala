package com.sap.scala.demo

import scala.scalajs.js

// Latitude and longitude of a city
class Coord(coord: js.Dynamic) {
  val lng = coord.lon.asInstanceOf[Double]
  val lat = coord.lat.asInstanceOf[Double]
}
